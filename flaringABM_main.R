flaringABM_main <- function(Params, jobID, Run) {

    cat("Run", Run, ":\n")
    # Initialize agents, save their initial state
    cat("...Initializing...\n")
    ti <- lapply(Params, `[[` , 1)
    if (is.na(Params$refID)) {
        source("./flaringABM_init_empirical.R", local=TRUE)
    } else {
        firms <- fread(cmd=sprintf('grep "RunID$\\|,%d$" ./outputs/agent_states_%s.csv', Run, Params$refID), nrow=Params$nagents,
                        colClasses=list(numeric=c("cost_CE","cost_M","sPressure"), character="activity"))
        setkey(firms, firmID)
        leases <- fread(cmd=sprintf('grep "RunID$\\|,%d$" ./outputs/lease_states_%s.csv', Run, Params$refID), nrow=40060,
                        colClasses=list(integer="t_switch", numeric="opEx_csgd"))
        setkey(leases, leaseID)
    }
    cat("...Running...\n\t")
    industry_revenue <- with(Params, list("prices"= list("dirty"= market_price_dirty),
                                    "green_coeff"= (market_price_green - market_price_dirty) * market_prop_green[1]))

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


        #### FIRM ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "activity":= ifelse(runif(.N) < Params$prop_e, "exploration", "development")]

        ## Exploration
        do_exploration(firms, leases, ti)

        ## Development
        # compare profit maximizing options with and without mitigation by comparing cost to possible harm
        portfolio_options <- optimize_strategy(firms, leases, industry_revenue, ti)
        # optimize market value by executing the best portfolio option
        do_development(firms, leases, portfolio_options, ti)


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
        fwrite(leases[!is.na(firmID) & status!="retired"], file=sprintf(leaseOuts, Run), append=TRUE)

        rm(portfolio_options)
    }
    cat("\n")
}
