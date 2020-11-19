# THIS SCRIPT CONTAINS THE DECISION MAKING FUNCTIONALITY OF THE flaringABM
# THE FUNCTIONS WHICH INFORM THESE ARE WRITTEN IN C++ & CONTAINED IN flaringABM_core.cpp
# 
#
library(data.table)
library(Rcpp)
sourceCpp("flaringABM_core.cpp")

#*
#*** SOCIAL PRESSURE ***#
#*
calc_total_pressure <- function() {
    # Calculate the total social pressure
    return(Params$Activism)
}###--------------------    END OF FUNCTION calc_total_pressure     --------------------###

dist_social_pressure <- function(agents, method="even", focus=1) {
    # Determine what proportion of the total social pressure is allocated to each agent
    A <- calc_total_pressure()

    if (method=="even") {
        agents[mitigation!=1, "sPressure":= A / .N]
    } else if (method=="focused") {
        focus <- intersect(focus, which(agents$mitigation!=1))
        agents[focus, "sPressure":= A / length(focus)]
    } else if (method=="gas_output") {
        agents[mitigation!=1, "sPressure":= A * gas_output / sum(gas_output)]
    }

}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###

#*
#*** FIRM VALUATION ***#
#*
calc_market_value <- function(agents, SRoR, time) {
    # calculates the market value based on Baron's formulation

    cost        <- calc_costC(agents, time, NA)                              #baseline cost
    add_cost    <- calc_costC(agents, time) - calc_costC(agents, time, NA)   #additional cost of mitigation
    revenue     <- calc_revenueC(agents, time)                               #revenue

    # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
    # (profit in this formulation does not capture the additional cost of mitigating the externality [cost*xi])
    market_value <- (revenue - cost - add_cost) +                   #Net cash flow
                    (add_cost*SRoR - (agents[,"sPressure"] / SRoR)) #Net social value

    return(market_value)
}###--------------------    END OF FUNCTION calc_market_value       --------------------###

build_permutations <- function(firmIDs) {
    portfolio_permutations <- wells[firmID %in% firmIDs][,
                                        .("wellIDs"= .(wellID),
                                        "class"= .(ifelse(class=="developed", 1, 0)),
                                        "perm"= transpose(as.list(do.call(CJ, rep(list(0:1), .N))))),
                                    keyby=firmID]
    # add firm attributes
    portfolio_permutations[firms[.(firmIDs)], on="firmID", c("i_horizon","t_horizon","sPressure","free_capital"):=
                                                                .(i_horizon, t_horizon, sPressure, capital-cost)]

    # a well's class can only increase (ie underdeveloped --> developed, developed -/-> underdeveloped)
    #    a 1 represents an additonal cost (ie developing an underdeveloped well),
    #    a 0 represents status-quo (ie an underdeveloped well that stays that way)
    portfolio_permutations[, "perm":= .(Map("-", perm, class))]
    portfolio_permutations <- portfolio_permutations[!sapply(Map("<", perm, 0), any)]

    # calculate the additional cost associated with excercizing each option
    portfolio_permutations[, "cost":=
            wells[unique(wellIDs), sapply(Map("*", .(green_add_oCost + green_fCost / unique(i_horizon)), perm), sum)],
        by=firmID]
    #    eliminate those which are out of budget
    portfolio_permutations <- portfolio_permutations[cost < free_capital]

    # determine which configurations meet the green threshold
    #    by calculating proportion of gas (that would be) flared to the total gas production
    portfolio_permutations[, "per_flared":= sapply(Map("-", 1, Map("+", class, perm)), weighted.mean,
                                                    wells[unique(wellIDs), gas_MCF]), by=firmID]
    #    and checking if it meets the green market threshold
    portfolio_permutations[, "meets_thresh":= per_flared < Params$threshold]

    # calculate revenue at given by exercising each option
    #portfolio_permutations[lengths(costs)>0,]
    # base revenue
    portfolio_permutations[, "gas_MCF":= sapply(lapply(Map("+", class, perm), "*",
                                                        wells[unique(wellIDs), gas_MCF]), sum), by=firmID]
    portfolio_permutations[, "add_gas_MCF":= .SD[,"gas_MCF"] - .SD[1]$gas_MCF, by=firmID]

    portfolio_permutations[, c("revenue", "best", "add_cost", "cost_harm"):= .(NA_real_, NA, NA_real_, NA)]

    return(portfolio_permutations)

}###--------------------    END OF FUNCTION build_permutations      --------------------###

optimize_strategy <- function(portfolio_permutations, SRoR) {
    #determines if the possible harm of social pressure outweighs the cost of mitigating the externality
    # minimum cost configuration
    # MIN GREEN CONFIG, MIN CONFIG
    # GREEN CONFIG AVOIDS HARM, OTHER CHEAPEST
    # choose between max profit portfolios with and without mitigation
    # best_options <- portfolio_permutations[, .SD[which.max(revenue - cost)],
    #                                         keyby=.(firmID, meets_thresh), .SDcols=c("wellIDs","revenue","cost")]
    portfolio_permutations[, "best":= (revenue-cost)==max(revenue - cost), keyby=.(firmID, meets_thresh)]
    # if the possible harm outweighs the cost, exercise the mitigation option
    #    change in cost less change in revenue
    #    possible harm from social pressure over "t_horizon"
    portfolio_permutations[(best), "add_cost":= ifelse(.N>1, diff(cost), 0), by=firmID]
    portfolio_permutations[(best), "cost_harm":= ifelse(.N>1, ((add_cost * (1-SRoR)) - (diff(revenue) * t_horizon)) <
                                                                ((sPressure / SRoR) * t_horizon), TRUE), by=firmID]

}###--------------------    END OF FUNCTION optimize_strategy       --------------------###
