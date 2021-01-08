library(data.table)

source("flaringABM_core.R")

jobID <- format(Sys.time(), "%m%d%H%M")
logOuts <- sprintf("logs/param_log_%s.csv", jobID)
agentOuts <- sprintf("outputs/agent_states_%s.csv", jobID)
wellOuts <- sprintf("outputs/well_states_%s.csv", jobID)

Params <<- list(
    "nagents" = 100,
    "nwells" = 1000,
    "t0" = -5,
    "tf" = 20,
    # Environmental Variables
    "Activism" = 200,
    "SRoR" = 0.8,
    # Market conditions
    "threshold" = 0.1, # max units of gas "green" firms can flare per unit of oil produce
    "market_price_dirty" = 1,
    "market_price_green" = 1 * 1.16, # from Kitzmueller & Shimshack 16[5,20]% zotero://select/items/0_PGHV5RK7
    "market_prop_green" = 0.03, # from OShaughnessy et al. % of green electricity sales zotero://select/items/0_HW2MXA38
    "oil_price" = 16,
    "capital_assets" = "upstream",
    # Activities
    "prop_e" = 0.5, #what proportion of firms engage in exploration activities in a given time step
    "prob_e" = 0.1, #with what probability to exploring firms discover a new asset
    "prob_m" = 0  #probability that a follower will mimic a leader if they observe them mitigating
)

for (Run in 1:20) {
    cat(Run, ":\t")
    # Initialize agents, save their initial state
    source("flaringABM_init.R")
    Params$market_size <- wells[status=="producing", sum(gas_MCF)] # sum(firms$gas_output)
    Params$market_rate_green <- 0 #with(Params, market_size / (1 + nrow(firms)/5) / (tf-t0))

    firms[, c("RunID", "time"):= .(Run, Params$t0-1)]
    wells[, c("RunID", "time"):= .(Run, Params$t0-1)]
    fwrite(Params, file=logOuts, append=(Run!=1))
    fwrite(firms, file=agentOuts, append=(Run!=1))
    fwrite(wells, file=wellOuts, append=(Run!=1))

    # step through time
    for (ti in Params$t0:Params$tf) {
        cat(ti, ", ")
        wells[, "time":= ti]
        firms[, "time":= ti]
        ## Calculate the Social Pressure on each firm (begins at time 0)
        if (ti>0) {
            dist_social_pressure(firms)
        }
        ## Update portfolio options
        if (length(options_changed) > 0) {
            # update credit parameters
            portfolio_permutations[firms[!(firmID %in% options_changed)], on="firmID",
                                    "free_capital":= capital - cost - add_cost]
            # update based on new aquisitions and developments
            portfolio_permutations <- rbind(portfolio_permutations[!(firmID %in% options_changed)],
                                            build_permutations(options_changed))
            setkey(portfolio_permutations, firmID, meets_thresh)
        }
        # project options revenue based on previous market conditions
        portfolio_permutations[firms, on="firmID", "gas_revenue":= i.gas_revenue +
                            (add_gas_MCF * with(industry_revenue, prices$dirty + ifelse(meets_thresh, green_coeff, 0)))]

        #### FIRM ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "do_e":= runif(.N) < Params$prop_e]
        # compare profit maximizing options with and without mitigation by comparing cost to possible harm
        optimize_strategy(portfolio_permutations, firms)

        # TOTDO: decide whether start production at newly developed wells

        ## Development
        # optimize market value by executing the best portfolio option
        #   firms who's strategy calls for new development
        options_changed <- portfolio_permutations[best==TRUE][sapply(Map("==", perm, 1), any)]$firmID
        do_development(firms, wells, portfolio_permutations, options_changed, ti)

        ## Exploration
        do_exploration(firms, wells, ti)
        # also revise the options of
        #   firms who's previous discoveries will enter their portfolio in the next turn
        options_changed <- sort(unique(c(options_changed, wells[status=="stopped"]$firmID)))

        #### MARKETS ####
        ## Expenses
        calc_debits(firms, wells, ti)

        ## Revenues
        industry_revenue <- calc_credits(firms, ti)

        ## Assess value
        # net cashflow from oil and gas operations
        #    (revenue from oil + gas operations) - (baseline costs + additional costs spent on mitigation)
        firms[, "cash":= cash + (oil_output*Params$oil_price + gas_revenue) - (cost + add_cost)]
        # calculates the market value based on Baron's formulation zotero://select/items/0_I7NL6RPA
        # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
        firms[, "market_value":= ((oil_output * Params$oil_price) + gas_revenue - cost - add_cost) +  # Net cash flow
                                ((add_cost * Params$SRoR) - (sPressure / Params$SRoR))]               # Net social value

        #### OUTPUT STATES ####
        fwrite(firms, file=agentOuts, append=TRUE)
        fwrite(wells, file=wellOuts, append=TRUE)
    }
    cat("\n")
}

# analytics
library(ggplot2)

agent_states <- fread(agentOuts)
agent_states$RunID <- as.factor(agent_states$RunID)

progress <- ggplot(agent_states, aes(x=time, color=RunID)) +
                geom_step(aes(y=mitigation), stat="summary", fun="sum")
print(progress)