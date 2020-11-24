library(data.table)

source("flaringABM_core.R")

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
    "oil_price" = 16,
    "capital_assets" = "upstream"
)

for (Run in 1:20) {
    cat(Run, ":  ")
    # Initialize agents, save their initial state
    source("flaringABM_init.R")
    Params$market_size <- sum(firms$gas_output)
    Params$green_size_rate <- with(Params, market_size / (1 + nrow(firms)/5) / (tf-t0))

    firms[, "RunID":=Run]
    if (Run==1) {
        fwrite(Params, file="logs/param_log.csv")
        fwrite(firms[, "time":=NA_integer_], file="outputs/agent_states.csv")
    }

    # step through time
    for (t in Params$t0:Params$tf) {
        cat(t, ", ")
        #### OUTPUT STATES ####
        fwrite(firms[, "time":=t], file="outputs/agent_states.csv", append=TRUE)

        #### MARKETS ####
        # calculate the social pressure on each firm (begins at t=0)
        if (t>0) {
            dist_social_pressure(firms)
        }

        ## Expenses
        # baseline operating costs
        firms[firms[wells, on="firmID"][,
                sum(baseline_oCost), by=firmID], on="firmID", "cost":= V1]
        # additional mitigating operating costs
        firms[firms[wells, on="firmID"][(!is.na(t_switch)),
                sum(green_add_oCost), by=firmID], on="firmID", "add_cost":= V1]
        # additional costs from paying off fixed mitigation expenses
        firms[firms[wells, on="firmID"][(t_switch + i_horizon > t),
                sum(green_fCost / i_horizon), by=firmID], on="firmID", "add_cost":= add_cost + V1]
        firms[, "cost":= cost + add_cost]

        ## Income
        firms[, "capital":= calc_capital_equivC(firms)]
        industry_revenue <- calc_revenueC(firms, t)
        firms[, c("gas_revenue"):= industry_revenue$gas_revenue]

        ## Net
        firms[, "cash":= cash + gas_revenue + oil_output*Params$oil_price - cost]
        # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
        firms[, "market_value":= ((oil_output * Params$oil_price) + gas_revenue - cost) +       # Net cash flow
                                 ((add_cost * Params$SRoR) - (sPressure / Params$SRoR))]          # social value

        # options projections based on current markets
        portfolio_permutations[firms, on="firmID", "revenue":=
                gas_revenue + (add_gas_MCF * with(industry_revenue, ifelse(meets_thresh, prices$green, prices$dirty)))]

        #### FIRM ACTIVITIES ####
        # randomly assign either development or exploration activities
        firms[, "do_e":= runif(nrow(firms)) > 0.5]
        developers <- firms[!(do_e)]$firmID
        # update from previous turns
        wells[class=="developed" & status=="stopped", "status":= .("producing")]

        ## development
        # optimize market value by executing the best portfolio option
        #    compare profit maximizing options with and without mitigation by comparing cost to possible harm
        optimize_strategy(portfolio_permutations, Params$SRoR)
        # update well classes to reflect new development
        wells[(portfolio_permutations[.(developers)][(best), unlist(Map("[", wellIDs, lapply(perm, as.logical)))]),
                c("class", "t_switch"):= .("developed", t)]
        firms[.(portfolio_permutations[.(developers)][(best & meets_thresh)]$firmID), "mitigation":= 1]
        firms[.(portfolio_permutations[.(developers)][(best & !meets_thresh)]$firmID), "mitigation":= 0]

        ## exploration
        #progress undeveloped wells from previous time step
        wells[class=="undeveloped", c("class", "status"):=
                .(ifelse(gas_MCF>0, "underdeveloped", "developed"), "stopped")]

        # 10% chance a firm finds a new well
        done_e <- sort(firms[(do_e) & runif(.N)>0.9]$firmID)
        wells[sample(which(is.na(firmID)), length(done_e)), c("firmID", "class"):= .(done_e, "undeveloped")]

        #### UPDATE ATTRIBUTES ####
        # gas output from development and exploration
        firms[wells[firmID %in% c(developers, done_e) & class=="developed", .(sum(gas_MCF)), by=.(firmID)],
                on="firmID", "gas_output":= .(V1)]
        # oil output from exploration
        firms[wells[firmID %in% done_e, .(sum(oil_BBL)), by=.(firmID)],
                on="firmID", "oil_output":= .(V1)]
        # portfolio options based on new aquisitions and developments
        portfolio_permutations <- setkey(rbind(portfolio_permutations[!(firmID %in% c(developers, done_e))],
                                                build_permutations(c(developers, done_e))), "firmID")
    }
    cat("\n")
}


# analytics
library(ggplot2)

agent_states <- fread("outputs/agent_states.csv")
agent_states$RunID <- as.factor(agent_states$RunID)

progress <- ggplot(agent_states, aes(x=time, color=RunID)) +
                geom_step(aes(y=mitigation), stat="summary", fun="sum")
print(progress)