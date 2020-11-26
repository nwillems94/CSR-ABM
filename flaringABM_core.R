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

calc_total_pressure <- function(Ai) {
    # Calculate the total social pressure

    return(Ai)
}###--------------------    END OF FUNCTION calc_total_pressure     --------------------###


dist_social_pressure <- function(agents, method="even", focus=1) {
    # Determine what proportion of the total social pressure is allocated to each agent
    A <- calc_total_pressure(Params$Activism)

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

calc_debits <- function(dt_f, dt_w, t) {
    # join firm and well attributes
    dt_e <- dt_f[dt_w, on="firmID"]

    # baseline operating costs
    dt_f[dt_e[,sum(baseline_oCost), by=firmID], on="firmID", "cost":= V1]

    # additional mitigating operating costs
    dt_f[dt_e[(!is.na(t_switch)),
        sum(green_add_oCost), by=firmID], on="firmID", "add_cost":= V1]

    # additional costs from paying off fixed mitigation expenses
    dt_f[dt_e[(t_switch + i_horizon > t),
            sum(green_fCost / i_horizon), by=firmID], on="firmID", "add_cost":= add_cost + V1]

}###--------------------    END OF FUNCTION calc_debits             --------------------###


calc_credits <- function(dt_f, dt_p, t) {
    # capital based on cash and reserves
    dt_f[, "capital":= calc_capital_equivC(dt_f)]

    # determine industry revenues
    industry_revenue <- calc_revenueC(dt_f, t)
    dt_f[, c("gas_revenue"):= industry_revenue$gas_revenue]

    # options projections based on current markets
    dt_p[dt_f, on="firmID", "revenue":=
            gas_revenue + (add_gas_MCF * with(industry_revenue, ifelse(meets_thresh, prices$green, prices$dirty)))]

}###--------------------    END OF FUNCTION calc_credits            --------------------###


build_permutations <- function(firmIDs) {
    portfolio_permutations <- wells[firmID %in% firmIDs][,
                                        .("wellIDs"= .(wellID),
                                        "class"= .(ifelse(class=="developed", 1, 0)),
                                        "perm"= transpose(as.list(do.call(CJ, rep(list(0:1), .N))))),
                                    keyby=firmID]
    # add firm attributes
    portfolio_permutations[firms[.(firmIDs)], on="firmID", c("i_horizon", "t_horizon", "sPressure", "free_capital"):=
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

    # calculate revenue given by exercising each option
    # base revenue
    portfolio_permutations[, "gas_MCF":= sapply(lapply(Map("+", class, perm), "*",
                                                        wells[unique(wellIDs), gas_MCF]), sum), by=firmID]
    portfolio_permutations[, "add_gas_MCF":= .SD[, "gas_MCF"] - .SD[1]$gas_MCF, by=firmID]

    # determine which configurations meet the green threshold
    #    by calculating proportion of gas (that would be) flared per unit of oil production
    portfolio_permutations[, "flaring_intensity":= (wells[unique(wellIDs), sum(gas_MCF)] - gas_MCF) /
                                                        wells[unique(wellIDs), sum(oil_BBL)], by=firmID]
    #    and checking if it meets the green market threshold
    portfolio_permutations[, "meets_thresh":= flaring_intensity < Params$threshold]

    portfolio_permutations[, c("revenue", "best"):= .(NA_real_, NA)]

    return(portfolio_permutations)

}###--------------------    END OF FUNCTION build_permutations      --------------------###



#*
#*** FIRM ACTIVITIES ***#
#*

optimize_strategy <- function(dt_p) {
    #determines if the possible harm of social pressure outweighs the cost of mitigating the externality
    # minimum cost configuration
    # MIN GREEN CONFIG, MIN CONFIG
    # GREEN CONFIG AVOIDS HARM, OTHER CHEAPEST
    # choose between max profit portfolios with and without mitigation
    dt_p[, "best":= (revenue-cost)==max(revenue - cost), keyby=.(firmID, meets_thresh)]
    # if the possible harm outweighs the cost, exercise the mitigation option
    #    change in cost less change in revenue
    #    possible harm from social pressure over "t_horizon"
    dt_p[(best), "best":= ifelse(.N>1, ((diff(cost) * (1-Params$SRoR)) - (diff(revenue) * t_horizon)) <
                                            ((sPressure / Params$SRoR) * t_horizon), TRUE), by=firmID]

}###--------------------    END OF FUNCTION optimize_strategy       --------------------###
