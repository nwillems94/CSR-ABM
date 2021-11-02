library(data.table)

source("flaringABM_core.R")

jobID <- format(Sys.time(), "%m%d%H%M")
logOuts <- sprintf("logs/param_log_%s.csv", jobID)
agentOuts <- sprintf("outputs/agent_states_%s.csv", jobID)
wellOuts <- sprintf("outputs/well_states_%s.csv", jobID)

Params <<- list(
    "refID" = NA, # reference initialization
    "nagents" = 100,
    "nwells" = 1000,
    "t0" = -25,
    "tf" = 60,
    # Environmental Variables
    "Activism" = 5000,
    # Market conditions
    "threshold" = 0.5, # max units of gas "green" firms can flare per unit of oil produced
    "market_price_dirty" = 1,
    "market_price_green" = 1 * 1.16, # from Kitzmueller & Shimshack 16[5,20]% zotero://select/items/0_PGHV5RK7
    "oil_price" = 16,
    # Activities
    "prop_e" = 0.5, # what proportion of firms engage in exploration activities in a given time step
    "prob_e" = 0.1, # with what probability to exploring firms discover a new asset
    "prob_m" = 0.5  # probability that a follower will mimic a leader if they observe them mitigating
)
#social rate of return
#    [0: no social satisfaction from holding shares or contributing to the activist
#     1: shareholding & activist contributions are perfect substitutes for personal giving]
Params$SRoR <- c(rep(0, -Params$t0), rep(0.3, Params$tf + 1))
# from OShaughnessy et al. 3% of electricity sales green zotero://select/items/0_HW2MXA38
Params$market_prop_green <- with(Params, c(rep(0, -t0),
                                        seq(from=0, to=1.5, length.out= 1 + (tf %/% 2)),
                                        rep(1.5, tf - (tf %/% 2)))) * 0.12

run_time <- Sys.time()
for (Run in 1:20) {
    cat("Run", Run, ":\n")
    # Initialize agents, save their initial state
    cat("...Initializing...\n")
    ti <- lapply(Params, `[[` , 1)
    if (is.na(Params$refID)) {
        source("flaringABM_init.R")
    } else {
        firms <- fread(sprintf("outputs/agent_states_%s.csv", Params$refID))[time==Params$t0-1 & RunID==Run]
        setkey(firms, firmID)
        wells <- fread(sprintf("outputs/well_states_%s.csv", Params$refID))[time==Params$t0-1 & RunID==Run]
        setkey(wells, wellID)
    }
    cat("...Running...\n\t")
    # build initial portfolios
    portfolio_permutations <- build_permutations(firms$firmID)

    industry_revenue <- with(Params, list("prices"= list("dirty"= market_price_dirty),
                                    "green_coeff"= (market_price_green - market_price_dirty) * market_prop_green[1]))
    options_changed <- c()

    Params$market_size <- wells[status=="producing", sum(gas_MCF)] # sum(firms$gas_output)

    firms[, "RunID":= Run]
    wells[, "RunID":= Run]
    fwrite(as.data.table(t(unlist(Params))), file=logOuts, append=(Run!=1))
    fwrite(firms, file=agentOuts, append=(Run!=1))
    fwrite(wells, file=wellOuts, append=(Run!=1))

    # step through time with appropriate parameters
    cat("|", strrep("_", options("width")[[1]]-12), "|\n\t ", strrep(" ", options("width")[[1]]-12), "|")
    for (ti in split(as.data.table(c("time"=list(Params$t0:Params$tf), Params)), by="time")) {

        cat("\r\t|", strrep("*", floor((options("width")[[1]]-12) * (ti$time - Params$t0) / (Params$tf-Params$t0))))
        wells[, "time":= ti$time]
        firms[, "time":= ti$time]

        ## Update portfolio options
        # update credit parameters
        portfolio_permutations[firms, on="firmID", "free_capital":= cash - cost_O - cost_M - cost_CE]
        if (length(options_changed) > 0) {
            # update based on new aquisitions and developments
            portfolio_permutations <- rbind(portfolio_permutations[!(firmID %in% options_changed)],
                                            build_permutations(options_changed))
            setkey(portfolio_permutations, firmID, meets_thresh)
        }
        # project options based on previous market conditions
        portfolio_permutations[firms, on="firmID", c("sPressure", "gas_revenue"):= .(i.sPressure, i.gas_revenue +
                        (add_gas_MCF * with(industry_revenue, prices$dirty + ifelse(meets_thresh, green_coeff, 0))))]

        #### FIRM ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "activity":= ifelse(runif(.N) < Params$prop_e, "exploration", "development")]
        # compare profit maximizing options with and without mitigation by comparing cost to possible harm
        optimize_strategy(portfolio_permutations, firms)

        # TOTDO: decide whether start production at newly developed wells

        ## Development
        # optimize market value by executing the best portfolio option
        #   firms who's strategy calls for new development
        options_changed <- portfolio_permutations[best==TRUE][sapply(lapply(perm, `==`, 1), any)]$firmID
        do_development(firms, wells, portfolio_permutations, options_changed)

        ## Exploration
        do_exploration(firms, wells)
        # also revise the options of
        #   firms who's previous discoveries will enter their portfolio in the next turn
        options_changed <- sort(unique(c(options_changed, wells[status=="stopped"]$firmID)))

        #### MARKETS ####
        ## Apply Social Pressure to each firm (beginning at time 0)
        if (ti$time > 0) {
            dist_social_pressure(firms)
        }

        ## Expenses
        calc_debits(firms, wells)

        ## Revenues
        industry_revenue <- calc_credits(firms)

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
        fwrite(firms, file=agentOuts, append=TRUE)
        fwrite(wells, file=wellOuts, append=TRUE)
    }
    cat("\n")
}
cat("\n", gsub("Time difference of", "All runs complete in", capture.output(Sys.time() - run_time)), "\n")

# analytics
library(ggplot2)

agent_states <- fread(agentOuts)
agent_states$RunID <- as.factor(agent_states$RunID)

progress <- ggplot(agent_states, aes(x=time, color=RunID)) +
                geom_step(aes(y=as.numeric(behavior!="flaring")), stat="summary", fun="sum")
print(progress)
