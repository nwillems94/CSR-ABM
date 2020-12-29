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
    "market_prop_green" = 0.02,
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

    firms[, "RunID":= Run]
    wells[, "RunID":= Run]
    if (Run==1) {
        fwrite(Params, file=logOuts)
        fwrite(firms[, "time":= NA_integer_], file=agentOuts)
        fwrite(wells[, "time":= NA_integer_], file=wellOuts)
    }

    # step through time
    for (t in Params$t0:Params$tf) {
        cat(t, ", ")
        #### OUTPUT STATES ####
        fwrite(firms[, "time":= t], file=agentOuts, append=TRUE)
        fwrite(wells[, "time":= t], file=wellOuts, append=TRUE)

        #### MARKETS ####
        ## Social Pressure
        # calculate the pressure on each firm (begins at t=0)
        if (t>0) {
            dist_social_pressure(firms)
        }

        ## Expenses
        calc_debits(firms, wells, t)

        ## Revenues
        calc_credits(firms, portfolio_permutations, t)

        ## Assess value
        # net cashflow from oil and gas operations
        #    (revenue from oil + gas operations) - (baseline costs + additional costs spent on mitigation)
        firms[, "cash":= cash + (oil_output*Params$oil_price + gas_revenue) - (cost + add_cost)]
        # calculates the market value based on Baron's formulation zotero://select/items/0_I7NL6RPA
        # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
        firms[, "market_value":= ((oil_output * Params$oil_price) + gas_revenue - cost - add_cost) +  # Net cash flow
                                ((add_cost * Params$SRoR) - (sPressure / Params$SRoR))]               # Net social value

        #### FIRM ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "do_e":= runif(.N) < Params$prop_e]

        # TOTDO: decide whether start production at newly developed wells

        ## Development
        # optimize market value by executing the best portfolio option
        #    compare profit maximizing options with and without mitigation by comparing cost to possible harm
        optimize_strategy(portfolio_permutations, firms)
        new_options <- portfolio_permutations[(best)][sapply(Map("==", perm, 1), any)]$firmID
        do_development(firms, wells, portfolio_permutations, new_options, t)

        ## Exploration
        do_exploration(firms, wells, t)
        # also revise the options of firms who's previous discoveries will enter their portfolio in the next turn
        new_options <- sort(c(new_options, unique(wells[status=="stopped"]$firmID)))

        ## Update portfolio options
        if (length(new_options) == 0) { next }
        # update credit parameters
        portfolio_permutations[firms[!(firmID %in% new_options)],
            on="firmID", "free_capital":= capital- cost - add_cost]
        # update based on new aquisitions and developments
        portfolio_permutations <- rbind(portfolio_permutations[!(firmID %in% new_options)],
                                        build_permutations(new_options))
        setkey(portfolio_permutations, firmID, meets_thresh)
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