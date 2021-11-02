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
    a <- calc_total_pressure(ti$Activism) / ti$SRoR
    dt_f[, "sPressure":= 0]

    if (method=="even") {
        dt_f[behavior=="flaring", "sPressure":= a / .N]
    } else if (method=="focused") {
        dt_f[(firmID %in% focus) & behavior=="flaring", "sPressure":= a / .N]
    } else if (method=="gas_output") {
        dt_f[behavior=="flaring", "sPressure":= a * gas_output / sum(gas_output)]
    } else if (method=="oil_output") {
        dt_f[behavior=="flaring", "sPressure":= a * oil_output / sum(oil_output)]
    }
    # pressure on leader firms
    else if (method=="leaders") {
        dt_f[market_value > quantile(market_value, 2/3), "sPressure":= a / .N]
    }

}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###



#*
#*** FIRM VALUATION ***#
#*

calc_debits <- function(dt_f, dt_l) {
    # join firm and lease attributes
    dt_e <- dt_f[dt_l, on="firmID"]

    # baseline operating costs
    dt_f[dt_e[status=="producing", sum(baseline_oCost), by=firmID], on="firmID", "cost_O":= V1]

    # additional mitigating operating costs
    dt_f[dt_e[(!is.na(t_switch)) & status=="producing", sum(green_add_oCost), by=firmID],
            on="firmID", "cost_M":= V1]

    # baseline capital expenditures yet to be paid off
    dt_f[, "cost_CE":= 0]
    dt_f[dt_e[(t_found + i_horizon > ti$time), sum(baseline_fCost) / i_horizon, by=firmID],
            on="firmID", "cost_CE":= V1]

    # additional capital expenditures from paying off fixed mitigation expenses
    dt_f[dt_e[(t_switch + i_horizon > ti$time), sum(green_fCost / i_horizon), by=firmID],
            on="firmID", "cost_CE":= cost_CE + V1]

}###--------------------    END OF FUNCTION calc_debits             --------------------###


calc_credits <- function(dt_f) {
    # determine industry revenues
    industry_revenue <- calc_revenueC(dt_f, ti)
    dt_f[, "green_gas_output":= industry_revenue$green_units]
    dt_f[, "gas_revenue":= industry_revenue$gas_revenue]
    dt_f[, "oil_revenue":= oil_output * ti$oil_price]

    return(industry_revenue)

}###--------------------    END OF FUNCTION calc_credits            --------------------###


build_permutations <- function(firmIDs) {
    dt_p <- leases[firmID %in% firmIDs,
                        .("leaseIDs"= .(leaseID),
                            "classes"= .(ifelse(class=="developed", 1, 0)),
                            # permutations of development vectors where a lease's class can only increase
                            # (ie underdeveloped --> developed, developed -/-> underdeveloped)
                            #    a 1 represents an additonal cost (ie developing an underdeveloped lease),
                            #    a 0 represents status-quo (ie an [under]developed lease that stays that way)
                            "perm"= class_permutationsC(class)),
                    keyby=firmID]

    # add firm attributes
    dt_p[firms, on="firmID", c("i_horizon", "t_horizon", "sPressure", "free_capital"):=
                                .(i_horizon, t_horizon, sPressure, cash - cost_O - cost_M - cost_CE)]

    # calculate the additional cost associated with exercising each option over the time horizon
    dt_p[, "cost_M_add":= leases[first(leaseIDs), sapply(lapply(perm, `*`, green_add_oCost), sum)], by=firmID]
    dt_p[, "cost_CE_add":= leases[first(leaseIDs), sapply(lapply(perm, `*`, green_fCost), sum)] / i_horizon, by=firmID]

    # calculate revenue given by exercising each option
    # base revenue
    dt_p[, "gas_MCF":= sapply(lapply(Map(`+`, classes, perm), `*`, leases[first(leaseIDs), gas_MCF]), sum), by=firmID]
    dt_p[, "add_gas_MCF":= .SD[, "gas_MCF"] - .SD[1]$gas_MCF, by=firmID]

    # determine which configurations meet the green threshold
    #    by calculating proportion of gas (that would be) flared per unit of oil production
    dt_p[, "flaring_intensity":= (leases[first(leaseIDs), sum(gas_MCF)] - gas_MCF) /
                                    leases[first(leaseIDs), sum(oil_BBL)], by=firmID]
    #    and checking if it meets the green market threshold
    dt_p[, "meets_thresh":= flaring_intensity < ti$threshold]

    dt_p[, c("gas_revenue", "best", "economical", "imitation"):= .(NA_real_, NA, NA, NA)]

    setkey(dt_p, firmID, meets_thresh)

    return(dt_p)

}###--------------------    END OF FUNCTION build_permutations      --------------------###



#*
#*** FIRM ACTIVITIES ***#
#*

find_imitators <- function(dt_f, success_metric="sales") {
    # determine who is an imitator - less successful followers mimic more successful leaders
    # following Leary & Roberts "success" can be defined in terms of sales (market share), profit, or market_value
    imitators <- dt_f[, .(firmID, behavior, activity, "weight"= log(1+get(success_metric)))][
                            # agents doing exploration cannot imitate
                            (activity!="exploration") &
                            # moderate rate of imitation
                            (runif(.N) < ti$prob_m) &
                            # less sucessful agents are more likely to imitate
                            (runif(.N) > weight / max(weight)) &
                            # more successful agents are more likely to be imitated
                            sapply(seq(.N), function(x)
                                # the most succeessful agent has no one to imitate
                                if (weight[x]==max(weight)) FALSE
                                # agents imitate as or more successful agents (besides themselves)
                                else sample(behavior[-x], 1, prob= pmax(weight[-x] - 0.95*weight[x], 0))!="flaring"),
                        firmID]
    return(imitators)

}###--------------------    END OF FUNCTION find_imitators          --------------------###


optimize_strategy <- function(dt_p, dt_f) {
    # determine the max profit portfolios with and without mitigation
    #    of those which are in budget
    dt_p[, "best":= FALSE]
    dt_p[(cost_M_add + cost_CE_add < free_capital | (cost_M_add + cost_CE_add)==0),
            "best":= replace(best, which.max(gas_revenue - cost_M_add), TRUE), by=.(firmID, meets_thresh)]

    # is gas capture economical even without social pressure?
    dt_p[best==TRUE, "economical":= if (.N==2) calc_netm_costC(.SD, ti$SRoR) < 0
                                    else if (meets_thresh==TRUE) dt_f[firmID==.BY]$behavior=="economizing"
                                    else NA, by=firmID]
    # imitators will mitigate even if it is not strictly more economical
    imitators <- find_imitators(dt_f)
    # imitators who are already planning to begin mitigating or only have one option are not really imitators
    imitators <- dt_p[(firmID %in% imitators) & best==TRUE,
                        ifelse(.N==2, calc_netm_costC(.SD, ti$SRoR) < sPressure, TRUE), by=firmID][
                            V1==TRUE, setdiff(imitators, firmID)]

    dt_p[best==TRUE, "imitation":=  if (.N==2) (.BY %in% imitators)
                                    else if (meets_thresh==TRUE) dt_f[firmID==.BY]$behavior=="imitating"
                                    else NA, by=firmID]

    dt_p[(firmID %in% imitators) & meets_thresh==FALSE, "best":= FALSE]

    # if the possible threat outweighs the cost, exercise the mitigation option
    dt_p[best==TRUE, "best":= if (.N>1)
            ifelse(calc_netm_costC(.SD, ti$SRoR) < sPressure, meets_thresh, !meets_thresh), by=firmID]
    # firms participating in exploration activities do no new development
    dt_p[firmID %in% dt_f[activity=="exploration"]$firmID, "best":= sapply(lapply(perm, `==`, 0), all)]

}###--------------------    END OF FUNCTION optimize_strategy       --------------------###


do_development <- function(dt_f, dt_l, dt_p, devs) {
    ## Update lease attributes
    # update lease classes to reflect new development
    dt_l[.(dt_p[best==TRUE][firmID %in% devs, unlist(Map(`[`, leaseIDs, lapply(perm, as.logical)))]),
            c("class", "t_switch"):= .("developed", ti$time)]
    ## Update firm attributes
    # whether they are mitigating and if
    #    they are doing so because of simple economics (besides social pressure)
    dt_f[dt_p[best==TRUE], on="firmID", "behavior":= ifelse(meets_thresh==TRUE, "mitigating", "flaring")]
    dt_f[dt_p[(best & meets_thresh & economical) == TRUE], on="firmID", "behavior":= "economizing"]
    dt_f[dt_p[(best & meets_thresh & imitation) == TRUE], on="firmID", "behavior":= "imitating"]

    # gas output from development
    dt_f[dt_l[firmID %in% devs & class=="developed" & status=="producing", .(sum(gas_MCF)), by=.(firmID)],
        on="firmID", "gas_output":= .(V1)]

}###--------------------    END OF FUNCTION do_development          --------------------###


do_exploration <- function(dt_f, dt_l) {
    new_discs <- dt_f[(activity=="exploration") & (runif(.N) < ti$prob_e)]$firmID
    prev_discs <- unique(dt_l[status=="stopped"]$firmID)

    # progress leases from previous turns
    #    newly discovered leases are undeveloped
    #    in the following time step, they progress to underdeveloped with stopped production
    #    at this point, firms can decide whether to fully develop them
    #    in the following time step, the leases begin production
    dt_l[status=="stopped", "status":= .("producing")]
    dt_l[class=="undeveloped", c("class", "status"):= .(ifelse(gas_MCF==0, "developed", "underdeveloped"), "stopped")]

    # probabilistically discover new leases
    dt_l[sample(which(is.na(firmID)), length(new_discs)),
        c("firmID", "class", "t_found"):= .(new_discs, "undeveloped", ti$time)]

    ## Update firm attributes
    # gas output from exploration
    dt_f[dt_l[firmID %in% prev_discs & status=="producing" & class=="developed", .(sum(gas_MCF)), by=.(firmID)],
            on="firmID", "gas_output":= .(V1)]
    # additional oil output from exploration
    dt_f[dt_l[firmID %in% prev_discs & status=="producing" & class!="undeveloped", .(sum(oil_BBL)), by=.(firmID)],
            on="firmID", c("oil_output","oil_revenue"):= .(V1, V1 * ti$oil_price)]

}###--------------------    END OF FUNCTION do_exploration          --------------------###
