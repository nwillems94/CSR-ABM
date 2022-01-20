flaringABM_main <- function(Params, jobID, Run) {

    cat("Run", Run, ":\n")
    # Initialize agents, save their initial state
    cat("...Initializing...\n")
    ti <- lapply(Params, `[[` , 1)
    if (is.na(Params$refID)) {
        source("./flaringABM_init_empirical.R", local=TRUE)
    } else {
        firms <- fread(sprintf("./outputs/agent_states_%s.csv", Params$refID))[time==Params$t0-1 & RunID==Run]
        setkey(firms, firmID)
        leases <- fread(sprintf("./outputs/lease_states_%s.csv", Params$refID))[time==Params$t0-1 & RunID==Run]
        setkey(leases, leaseID)
    }
    cat("...Running...\n\t")
    # build initial portfolios
    portfolio_permutations <- build_permutations(firms, leases, firms$firmID, ti)

    industry_revenue <- with(Params, list("prices"= list("dirty"= market_price_dirty),
                                    "green_coeff"= (market_price_green - market_price_dirty) * market_prop_green[1]))
    options_changed <- c()

    Params$market_size <- leases[status=="producing", 1.2*sum(gas_MCF)]

    firms[, "RunID":= Run]
    leases[, "RunID":= Run]
    fwrite(as.data.table(t(unlist(Params))), file=sprintf(logOuts, Run))
    fwrite(firms, file=sprintf(agentOuts, Run))
    fwrite(leases, file=sprintf(leaseOuts, Run))

    # step through time with appropriate parameters
    cat("|", strrep("_", options("width")[[1]]-12), "|\n\t ", strrep(" ", options("width")[[1]]-12), "|")
    for (ti in split(as.data.table(c("time"=list(Params$t0:Params$tf), Params)), by="time")) {

        cat("\r\t|", strrep("*", floor((options("width")[[1]]-12) * (ti$time - Params$t0) / (Params$tf-Params$t0))))
        leases[, "time":= ti$time]
        firms[, "time":= ti$time]

        ## Update portfolio options
        # update credit parameters
        portfolio_permutations[firms, on="firmID", "free_capital":= cash - cost_O - cost_M - cost_CE]
        if (length(options_changed) > 0) {
            # update based on new aquisitions and developments
            portfolio_permutations <- rbind(portfolio_permutations[!(firmID %in% options_changed)],
                                            build_permutations(firms, leases, options_changed, ti))
            setkey(portfolio_permutations, firmID, meets_thresh)
        }
        # project options based on previous market conditions
        portfolio_permutations[firms, on="firmID", c("sPressure", "gas_revenue"):= .(i.sPressure, i.gas_revenue +
                        (add_csgd_MCF * with(industry_revenue, prices$dirty + ifelse(meets_thresh, green_coeff, 0))))]

        #### FIRM ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "activity":= ifelse(runif(.N) < Params$prop_e, "exploration", "development")]
        # compare profit maximizing options with and without mitigation by comparing cost to possible harm
        optimize_strategy(portfolio_permutations, firms, ti)

        ## Development
        # optimize market value by executing the best portfolio option
        #   firms who's strategy calls for new development
        options_changed <- portfolio_permutations[, first(best), by=firmID][V1==FALSE, firmID]
        do_development(firms, leases, portfolio_permutations, options_changed, ti)

        ## Exploration
        do_exploration(firms, leases, ti)
        # also revise the options of
        #   firms who's previous discoveries will enter their portfolio in the next turn
        options_changed <- sort(unique(c(options_changed, leases[status=="stopped"]$firmID)))

        #### MARKETS ####
        ## Apply Social Pressure to each firm (beginning at time 0)
        if (ti$time > 0) {
            dist_social_pressure(firms, ti)
        }

        ## Expenses
        calc_debits(firms, leases)

        ## Revenues
        industry_revenue <- calc_credits(firms, ti)

        ## Assess value
        # net cashflow from oil and gas operations
        #    (revenue from oil + gas operations) - (baseline costs + additional costs spent on mitigation)
        firms[, "sales":= oil_revenue + gas_revenue]
        firms[, "profit":= sales - (cost_O + cost_M + cost_CE)]
        firms[, "cash":= cash + profit]
        # calculates the market value based on Baron's formulation zotero://select/items/0_I7NL6RPA
        # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
        firms[, "market_value":= ((oil_revenue + gas_revenue) - (cost_O + cost_M)) +    # Net income
                                ((cost_M * ti$SRoR) - sPressure)]                       # Net social value

        #### OUTPUT STATES ####
        fwrite(firms, file=sprintf(agentOuts, Run), append=TRUE)
        fwrite(leases[!is.na(firmID)], file=sprintf(leaseOuts, Run), append=TRUE)
    }
    cat("\n")
}
