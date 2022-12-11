flaringABM_main <- function(Params, jobID, Run) {

    cat("Run", Run, ":\n")
    # Initialize agents, save their initial state
    cat("...Initializing...\n")
    ti <- lapply(Params, `[[` , 1)

    if (is.na(Params$refID)) {
        set.seed(Run)
        source("./flaringABM_init_empirical.R", local=TRUE)
        saveRDS(demand, sprintf("./outputs/demand_function_%s.rds", Run))
        firms[, "RunID":= Run]
        leases[, "RunID":= Run]

        market_history <- data.table("time"=Params$t0:Params$tf, key="time",
                                    "p_grey"= NA_real_, "p_green"= NA_real_,
                                    "p_oil_mult"= with(Params, rlnorm(1+tf-t0, oil_price_mean, oil_price_sd)),
                                    "q_grey"= NA_real_, "q_green"= NA_real_, "q_oil"= NA_real_,
                                    "market_prop_green"= Params$market_prop_green,
                                    "RunID"= Run)
    } else {
        demand <- readRDS(sprintf("./outputs/demand_function_%s.rds", Run))
        # update default arguments if necessary
        with(environment(demand$new_schedule),
            formals(new_schedule) <-  c(alist(prop_green=, sample_set=list()), Params[c("p_low", "p_high")]))
        firms <- fread(sprintf("./outputs/agent_states_%s-%s.csv", Params$refID, Run),
                        colClasses=list(numeric=c("sPressure", "grey_gas_sold"), character="activity"))[
                    time==Params$t0 - 1]
        setkey(firms, firmID)

        leases <- fread(sprintf("./outputs/lease_states_%s-%s.csv", Params$refID, Run),
                        colClasses= list(integer="t_switch", numeric="cost_csgd"))[
                    time<Params$t0][!duplicated(leaseID, fromLast=TRUE)]
        leases[, "time":= Params$t0 - 1]
        setkey(leases, leaseID)

        market_history <- fread(sprintf("./outputs/market_states_%s-%s.csv", Params$refID, Run))
        market_history[time >= Params$t0, c("p_grey", "p_green", "q_grey", "q_green", "q_oil"):= NA_real_]
        market_history[time >= Params$t0, "market_prop_green":= Params$market_prop_green]
        setkey(market_history, time)
    }
    cat("...Running...\n\t")

    if (Params$reporting!="accurate") {
        leases[capEx_csgd>0, "opEx_pMCF":= opEx_pMCF - (612.5 * (capEx_csgd / 31250) / csgd_MCF)]
        leases[!is.na(firmID) & (csgd_MCF>0) & (class=="developed"), "t_switch":= NA_integer_]
        leases[!is.na(firmID) & (csgd_MCF>0) & (class=="developed"), "class":= "underdeveloped"]

        if (Params$reporting == "misreported") {
            # assume flared casinghead volumes are mis-reported as captured and up to 3 times as much gas is actually flared
            leases[capEx_csgd>0, "capEx_csgd":=
                    {"flared_MCF"= csgd_MCF - 6 * (BOE_emp - oil_BBL);
                    31250 * ceiling(pmin(3*flared_MCF, csgd_MCF)/5475)}]
        } else if (Params$reporting == "underreported") {
            # assume flared casinghead volumes are under-reported and 3 times as much gas is actually flared
            leases[capEx_csgd>0, c("capEx_csgd", "sopf_MCF", "csgd_MCF"):=
                    {"flared_MCF"= csgd_MCF - 6 * (BOE_emp - oil_BBL);
                    .(31250 * ceiling(3*flared_MCF/5475),
                    (csgd_MCF + (2 * flared_MCF)) * sopf_MCF / csgd_MCF,
                    csgd_MCF + (2 * flared_MCF))}]
        }

        leases[capEx_csgd>0, "opEx_pMCF":= opEx_pMCF + (612.5 * (capEx_csgd / 31250) / csgd_MCF)]
        leases[!is.na(firmID) & (csgd_MCF>0) & ((Params$t0 - t_found)>=120),
                "class":= replace(class, opEx_pMCF + (capEx_csgd / csgd_MCF / lifetime) < 5, "developed")]
        leases[!is.na(firmID) & (csgd_MCF>0) & (class=="developed"), "t_switch":= t_found]

        # calculate lease operating expenses
        leases[, sprintf("cost_%s", c("oil","csgd","gas")):= lease_costs(.SD)]

        # initial flaring intensity
        firms[leases[!is.na(firmID), .(sum(oil_BBL+cond_BBL), sum(gas_MCF), sum(csgd_MCF[class=="underdeveloped"]) + sum(sopf_MCF)), by=firmID],
                on="firmID", c("oil_output", "gas_output", "gas_flared"):= .(V1, V2, V3)]
        firms[, "behavior":= ifelse(gas_flared/oil_output > Params$threshold, "flaring", "economizing")]
    }

    # unique random seed for consistent results across model runs
    seed_base <- market_history[, (Run * 10^floor(1 + log10(max(time) - min(time) + 1))) - min(time)]

    if (market_history[, all(is.na(p_grey))]) {
        # initialize dummy portfolio options (abuses lack of type-checking)
        portfolio_options <- optimal_strategy(firms, replace(leases, "class", ""), "", "", list("time"=Params$t0-1))
    } else {
        t1 <- c(as.list(market_history[!is.na(p_grey), last(.SD[, c("time", "market_prop_green")])]),
                Params["threshold"], "SRoR"=0)
        set.seed(seed_base + t1$time)
        portfolio_options <- optimal_strategy(firms, leases, market_history, demand$new_schedule(t1$market_prop_green), t1)
    }

    # with what probability to exploring firms discover a new asset - validated based on EIA DPR
    Params$prob_e <- 1.23 / (Params$nagents * Params$prop_e *
                                leases[is.na(firmID), mean(oil_BBL+cond_BBL + (gas_MCF+csgd_MCF)/6)])

    fwrite(as.data.table(t(unlist(Params))), file=sprintf(logOuts, Run))
    fwrite(firms, file=sprintf(agentOuts, Run))
    fwrite(leases, file=sprintf(leaseOuts, Run))

    # step through time with appropriate parameters
    cat("|", strrep("_", options("width")[[1]]-12), "|\n\t ", strrep(" ", options("width")[[1]]-12), "|")
    for (ti in split(as.data.table(c("time"=list(Params$t0:Params$tf), Params)), by="time")) {

        cat("\r\t|", strrep("*", floor((options("width")[[1]]-12) * (ti$time - Params$t0) / (Params$tf-Params$t0))))

        set.seed(seed_base + ti$time)
        ti$period <- with(ti, time - min(market_history$time) + 1)
        leases[, "time":= ti$time]
        firms[, "time":= ti$time]


        #### AGENT ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "activity":= ifelse(runif(.N) < Params$prop_e, "exploration", "development")]

        ## Exploration
        cat("\nExploration,\t")
        do_exploration(firms, leases, ti)

        ## Development
        # optimize market value by executing the best portfolio option
        cat("Development,\t")
        do_development(firms, leases, portfolio_options, ti)
        rm(portfolio_options)

        # Update firm outputs
        firms[leases[, .SD[(status=="producing") & (class!="undeveloped"), sum(oil_BBL+cond_BBL)], by=firmID],
            on="firmID", "oil_output":= V1]
        firms[leases[, .SD[(status=="producing"), sum(csgd_MCF[class=="underdeveloped"]) + sum(sopf_MCF)], by=firmID],
                on="firmID", "gas_flared":= V1]
        firms[, "behavior":= ifelse((gas_flared==0) | ((gas_flared/oil_output) <= ti$threshold),
                                 ifelse(behavior=="flaring", "economizing", behavior), "flaring")]

        #### MARKET CONDITIONS ####
        # generate a new representative demand schedule
        demand_schedule <- demand$new_schedule(ti$market_prop_green)
        cat("Markets,\t")
        clear_gas_markets(firms, leases, market_history, demand_schedule, ti)

        ## Apply Social Pressure to each firm (beginning at time 0)
        dist_social_pressure(firms, ti)


        #### EXECUTE TRANSACTIONS ####
        ## Expenses
        cat("Transactions,\t")
        calc_debits(firms, leases)

        ## Revenues
        calc_credits(firms, market_history[.(ti$time)])

        ## Assess value
        # net cashflow from oil and gas operations
        #    (revenue from oil + gas operations) - (baseline costs + additional costs spent on mitigation)
        firms[, "sales":= oil_revenue + gas_revenue]
        firms[, "profit":= sales - (cost_P + cost_M)]
        # calculates the market value based on [Baron's formulation](zotero://select/items/0_I7NL6RPA)
        # market_value = profit + dprofit - Ai - cost*xi + cost*xi*SRoR
        firms[, "market_value":= (oil_revenue + gas_revenue - cost_P - cost_M)/ti$period +  # Net income
                                ((cost_M * ti$SRoR)/ti$period - sPressure)]                 # Net social value
        firms[, "market_value":= pmax(market_value, 0)]



        #### AGENT RESPONSE ####
        # compare profit maximizing options with and without mitigation by comparing cost to possible harm
        cat("Options\n")
        portfolio_options <- optimal_strategy(firms, leases, market_history, demand_schedule, ti)

        #### OUTPUT STATES ####
        fwrite(firms, file=sprintf(agentOuts, Run), append=TRUE)
        fwrite(leases[(lifetime + t_found+1) >= ti$time], file=sprintf(leaseOuts, Run), append=TRUE)
    }
    setnafill(market_history, fill=0)
    fwrite(market_history, file=sprintf(marketOuts, Run))
    cat("\n")
}
