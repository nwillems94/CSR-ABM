flaringABM_main <- function(Params, jobID, Run) {

    cat("Run", Run, ":\n")
    # Initialize agents, save their initial state
    cat("...Initializing...\n")
    ti <- lapply(Params, `[[` , 1)

    if (is.na(Params$refID)) {
        source("./flaringABM_init_empirical.R", local=TRUE)
        saveRDS(demand, sprintf("./outputs/demand_function_%s.rds", Run))
        firms[, "RunID":= Run]
        leases[, "RunID":= Run]
    } else {
        demand <- readRDS(sprintf("./outputs/demand_function_%s.rds", Run))
        firms <- fread(cmd=sprintf('grep "RunID$\\|,%d$" ./outputs/agent_states_%s.csv', Run, Params$refID), nrow=Params$nagents,
                        colClasses=list(numeric=c("cost_M","sPressure","grey_gas_sold","green_gas_sold"), character="activity"))
        setkey(firms, firmID)
        leases <- fread(cmd=sprintf('grep "RunID$\\|,%d$" ./outputs/lease_states_%s.csv', Run, Params$refID), nrow=34787,
                        colClasses=list(integer="t_switch", numeric="cost_csgd"))
        setkey(leases, leaseID)
    }
    cat("...Running...\n\t")
    market_history <- data.table("time"=Params$t0:Params$tf, key="time",
                                "p_grey"= NA_real_, "p_green"= NA_real_,
                                "p_oil_mult"= with(Params, rlnorm(1+tf-t0, oil_price_mean, oil_price_sd)),
                                "q_grey"= NA_real_, "q_green"= NA_real_, "q_oil"= NA_real_,
                                "market_prop_green"= Params$market_prop_green,
                                "RunID"= Run)

    # initialize dummy portfolio options (abuses lack of type-checking)
    portfolio_options <- optimal_strategy(firms, replace(leases, "class", ""), "", "", list("time"=Params$t0-1))

    # with what probability to exploring firms discover a new asset - validated based on EIA DPR
    Params$prob_e <- 0.7 * 1.23 / (Params$nagents * Params$prop_e * leases[is.na(firmID), mean(oil_BBL+cond_BBL)])
    fwrite(as.data.table(t(unlist(Params))), file=sprintf(logOuts, Run))
    fwrite(firms, file=sprintf(agentOuts, Run))
    fwrite(leases, file=sprintf(leaseOuts, Run))

    # step through time with appropriate parameters
    cat("|", strrep("_", options("width")[[1]]-12), "|\n\t ", strrep(" ", options("width")[[1]]-12), "|")
    for (ti in split(as.data.table(c("time"=list(Params$t0:Params$tf), Params)), by="time")) {

        cat("\r\t|", strrep("*", floor((options("width")[[1]]-12) * (ti$time - Params$t0) / (Params$tf-Params$t0))))
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
        firms[, "market_value":= ((oil_revenue + gas_revenue) - (cost_P + cost_M)) +    # Net income
                                ((cost_M * ti$SRoR) - sPressure)]                       # Net social value



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
