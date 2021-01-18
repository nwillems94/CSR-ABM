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


dist_social_pressure <- function(dt_f, method="even", focus=1) {
    # Determine what proportion of the total social pressure is allocated to each agent
    A <- calc_total_pressure(Params$Activism)
    dt_f[, "sPressure":= 0]

    if (method=="even") {
        dt_f[mitigation!=1, "sPressure":= A / .N]
    } else if (method=="focused") {
        dt_f[(firmID %in% focus) & mitigation!=1, "sPressure":= A / .N]
    } else if (method=="gas_output") {
        dt_f[mitigation!=1, "sPressure":= A * gas_output / sum(gas_output)]
    }
    # pressure on leader firms
    else if (method=="leaders") {
        dt_f[market_value > quantile(market_value, 2/3), "sPressure":= A / .N]
    }

}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###



#*
#*** FIRM VALUATION ***#
#*

calc_debits <- function(dt_f, dt_w, ti) {
    # join firm and well attributes
    dt_e <- dt_f[dt_w, on="firmID"]

    # baseline operating costs
    dt_f[dt_e[status=="producing", sum(baseline_oCost), by=firmID], on="firmID", "cost_O":= V1]

    # additional mitigating operating costs
    dt_f[dt_e[(!is.na(t_switch)) & status=="producing", sum(green_add_oCost), by=firmID],
            on="firmID", "cost_M":= V1]

    # baseline capital expenditures yet to be paid off
    dt_f[, "cost_CE":= 0]
    dt_f[dt_e[(t_found + i_horizon > ti), sum(baseline_fCost) / i_horizon, by=firmID],
            on="firmID", "cost_CE":= V1]

    # additional capital expenditures from paying off fixed mitigation expenses
    dt_f[dt_e[(t_switch + i_horizon > ti), sum(green_fCost / i_horizon), by=firmID],
            on="firmID", "cost_CE":= cost_CE + V1]

}###--------------------    END OF FUNCTION calc_debits             --------------------###


calc_credits <- function(dt_f, ti) {
    # capital based on cash and reserves
    dt_f[, "capital":= calc_capital_equivC(dt_f)]

    # determine industry revenues
    industry_revenue <- calc_revenueC(dt_f, ti)
    dt_f[, "green_gas_output":= industry_revenue$green_units]
    dt_f[, "gas_revenue":= industry_revenue$gas_revenue]
    dt_f[, "oil_revenue":= oil_output * Params$oil_price]

    return(industry_revenue)

}###--------------------    END OF FUNCTION calc_credits            --------------------###


build_permutations <- function(firmIDs) {
    dt_p <- wells[firmID %in% firmIDs][,
                                        .("wellIDs"= .(wellID),
                                        "class"= .(ifelse(class=="developed", 1, 0)),
                                        "perm"= transpose(as.list(do.call(CJ, rep(list(0:1), .N))))),
                                    keyby=firmID]
    # add firm attributes
    dt_p[firms[.(firmIDs)], on="firmID", c("i_horizon", "t_horizon", "sPressure", "free_capital"):=
                                        .(i_horizon, t_horizon, sPressure, capital - cost_O - cost_M - cost_CE)]

    # a well's class can only increase (ie underdeveloped --> developed, developed -/-> underdeveloped)
    #    a 1 represents an additonal cost (ie developing an underdeveloped well),
    #    a 0 represents status-quo (ie an [under]developed well that stays that way)
    dt_p[, "perm":= .(Map("-", perm, class))]
    dt_p <- dt_p[!sapply(Map("<", perm, 0), any)]

    # calculate the additional cost associated with excercizing each option over the time horizon
    dt_p[, "cost_M_add":= wells[first(wellIDs), sapply(Map("*", .(green_add_oCost), perm), sum)], by=firmID]
    dt_p[, "cost_CE_add":= wells[first(wellIDs), sapply(Map("*", .(green_fCost), perm), sum)] / i_horizon, by=firmID]

    # calculate revenue given by exercising each option
    # base revenue
    dt_p[, "gas_MCF":= sapply(lapply(Map("+", class, perm), "*", wells[first(wellIDs), gas_MCF]), sum), by=firmID]
    dt_p[, "add_gas_MCF":= .SD[, "gas_MCF"] - .SD[1]$gas_MCF, by=firmID]

    # determine which configurations meet the green threshold
    #    by calculating proportion of gas (that would be) flared per unit of oil production
    dt_p[, "flaring_intensity":= (wells[first(wellIDs), sum(gas_MCF)] - gas_MCF) /
                                    wells[first(wellIDs), sum(oil_BBL)], by=firmID]
    #    and checking if it meets the green market threshold
    dt_p[, "meets_thresh":= flaring_intensity < Params$threshold]

    dt_p[, c("gas_revenue", "best", "economical"):= .(NA_real_, NA, NA)]

    setkey(dt_p, firmID, meets_thresh)

    return(dt_p)

}###--------------------    END OF FUNCTION build_permutations      --------------------###



#*
#*** FIRM ACTIVITIES ***#
#*

find_imitators <- function(dt_f) {
    # determine who is an imitator based on oil market share
    imitators <- dt_f[, .("firmID"= firmID, "market_share"= oil_output / sum(oil_output))]
    imitators[, "position":= cut(rank(market_share, ties.method="first"),
                                breaks=3, labels=c("follower", NA_character_, "leader"))]

    if (imitators[position=="leader", .N] != imitators[position=="follower", .N]) {
        imitators[position=="follower", "position":= replace(position, which.max(market_share), NA_character_)]
    }
    imitators <- dt_f[sample(imitators[position=="follower"]$firmID)][
                        (dt_f[sample(imitators[position=="leader"]$firmID)]$mitigation==1)][
                            runif(.N) < Params$prob_m]$firmID

    return(imitators)

}###--------------------    END OF FUNCTION find_imitators          --------------------###


optimize_strategy <- function(dt_p, dt_f) {
    # determine the max profit portfolios with and without mitigation
    #    of those which are in budget
    dt_p[, "best":= FALSE]
    dt_p[(cost_M_add + cost_CE_add < free_capital | (cost_M_add + cost_CE_add)==0),
            "best":= replace(best, which.max(gas_revenue - cost_M_add), TRUE), by=.(firmID, meets_thresh)]

    # is gas capture economical even without social pressure?
    dt_p[best==TRUE, "economical":= ifelse(.N>1, diff(cost_M_add) < diff(gas_revenue)*t_horizon, NA), by=firmID]

    # imitators will mitigate even if it is not strictly more economical
    imitators <- find_imitators(dt_f)
    #    (as long as they can afford it)
    imitators <- dt_p[best==TRUE][firmID %in% imitators, .N, by=firmID][N>1]$firmID
    dt_p[(firmID %in% imitators) & meets_thresh==FALSE, "best":= FALSE]
    dt_f[, "imitator":= ifelse((firmID %in% imitators), TRUE, FALSE)]

    # if the possible harm outweighs the cost, exercise the mitigation option
    #    change in cost less change in revenue
    #    possible harm from social pressure
    dt_p[best==TRUE, "best":= if(.N>1)
        ifelse((diff(cost_M_add) * (1-Params$SRoR) - diff(gas_revenue)) < (sPressure / Params$SRoR),
                meets_thresh, !meets_thresh), by=firmID]
    # firms participating in exploration activities do no new development
    dt_p[firmID %in% dt_f[do_e==TRUE]$firmID, "best":= sapply(Map("==", perm, 0), all)]

}###--------------------    END OF FUNCTION optimize_strategy       --------------------###


do_development <- function(dt_f, dt_w, dt_p, devs, ti) {
    ## Update well attributes
    # update well classes to reflect new development
    dt_w[.(dt_p[best==TRUE][firmID %in% devs, unlist(Map("[", wellIDs, lapply(perm, as.logical)))]),
            c("class", "t_switch"):= .("developed", ti)]
    ## Update firm attributes
    # whether they are mitigating and if
    #    they are doing so because of simple economics (besides social pressure)
    dt_f[dt_p[best==TRUE], on="firmID", c("mitigation","economizer"):=
        .(as.numeric(meets_thresh), meets_thresh & ifelse(is.na(economical), economizer, economical))]

    # gas output from development
    dt_f[dt_w[firmID %in% devs & class=="developed" & status=="producing", .(sum(gas_MCF)), by=.(firmID)],
        on="firmID", "gas_output":= .(V1)]

}###--------------------    END OF FUNCTION do_development          --------------------###


do_exploration <- function(dt_f, dt_w, ti) {
    new_discs <- dt_f[(do_e==TRUE) & (runif(.N) < Params$prob_e)]$firmID
    prev_discs <- unique(dt_w[status=="stopped"]$firmID)

    # progress wells from previous turns
    #    newly discovered wells are undeveloped
    #    in the following time step, they progress to underdeveloped with stopped production
    #    at this point, firms can decide whether to fully develop them
    #    in the following time step, the wells begin production
    dt_w[status=="stopped", "status":= .("producing")]
    dt_w[class=="undeveloped", c("class", "status"):= .(ifelse(gas_MCF==0, "developed", "underdeveloped"), "stopped")]

    # probabilistically discover new wells
    dt_w[sample(which(is.na(firmID)), length(new_discs)),
        c("firmID", "class", "t_found"):= .(new_discs, "undeveloped", ti)]

    ## Update firm attributes
    # gas output from exploration
    dt_f[dt_w[firmID %in% prev_discs & status=="producing" & class=="developed", .(sum(gas_MCF)), by=.(firmID)],
            on="firmID", "gas_output":= .(V1)]
    # additional oil output from exploration
    dt_f[dt_w[firmID %in% prev_discs & status=="producing" & class!="undeveloped", .(sum(oil_BBL)), by=.(firmID)],
            on="firmID", c("oil_output","oil_revenue"):= .(V1, V1 * Params$oil_price)]

}###--------------------    END OF FUNCTION do_exploration          --------------------###
