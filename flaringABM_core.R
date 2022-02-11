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


dist_social_pressure <- function(dt_f, ti, method="even", focus=1) {
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

calc_opEx <- function(dt_l) {
    # determine operation costs based on lease outputs
    # MCF to BOE Conversion: https://petrowiki.spe.org/Glossary:Barrels_of_oil_equivalent
    dt_l[,  {csgd_developed_MCF= ifelse(class %in% "developed", csgd_MCF, 0);
            opEx_pBOE= opEx / (oil_BBL + cond_BBL + (gas_MCF + csgd_developed_MCF)/6);
            .(cbind("oil"=oil_BBL+cond_BBL, "csgd"=csgd_developed_MCF, "gas"=gas_MCF) *
                (sapply(c(1, 1/6, 1/6), `*`, opEx_pBOE) + cbind(opEx_pBBL, opEx_pMCF, opEx_pMCF)))}]
}###--------------------    END OF FUNCTION calc_opEx               --------------------###


calc_debits <- function(dt_f, dt_l) {
    # baseline operating costs
    dt_f[dt_l[status=="producing", sum(opEx_oil + opEx_gas), by=firmID], on="firmID", "cost_O":= V1]

    # additional mitigating operating costs
    dt_f[dt_l[class=="developed" & status=="producing", sum(opEx_csgd), by=firmID], on="firmID", "cost_M":= V1]

    # capital expenditures which are paid off over the lease lifetime
    dt_f[, "cost_CE":= 0]
    dt_f[dt_l[status!="retired", sum(capEx / lifetime), by=firmID], on="firmID", "cost_CE":= V1]

}###--------------------    END OF FUNCTION calc_debits             --------------------###


calc_credits <- function(dt_f, ti) {
    # determine industry revenues
    industry_revenue <- calc_revenueC(dt_f, ti)
    dt_f[, "green_gas_output":= industry_revenue$green_units]
    dt_f[, "gas_revenue":= industry_revenue$gas_revenue]
    dt_f[, "oil_revenue":= oil_output * ti$oil_price]

    return(industry_revenue)

}###--------------------    END OF FUNCTION calc_credits            --------------------###

build_permutations <- function(dt_l, market, ti) {
    dt_p <- dt_l[order(opEx_pMCF),
                    .("leaseIDs"= .(leaseID[class=="underdeveloped"]), "prod_BBL"= sum(oil_BBL)),
                keyby=firmID][lengths(leaseIDs)>0]

    # develop leases which would have an operating cost below the conventional market price for gas
    dt_p[, "base_perm":= dt_l[leaseIDs, .(.(as.numeric(opEx_pMCF<market$prices$dirty)))], by=firmID]

    # lowest cost per MCF which meets the green threshold
    # calculate additional gas capture (wrt base) necessary to meet green threshold
    dt_p[, "K":= dt_l[leaseIDs, sum((1-base_perm[[1]]) * csgd_MCF)] - (ti$threshold * prod_BBL), by=firmID]
    dt_p[, "green_perm":= .(ifelse(K<=0,  base_perm,
                                Map(pmax, base_perm, dt_l[leaseIDs, shift(cumsum(csgd_MCF)<K, fill=1)]))), by=firmID]

    # calculate the additional cost and output (wrt base) associated with exercising the green option
    dt_p[, "cost_M_add":= dt_l[leaseIDs, sum((green_perm[[1]] - base_perm[[1]]) * opEx_pMCF * csgd_MCF)], by=firmID]
    # capital expenditures for gas capture are assumed to be borne by midstream firms
    dt_p[, "cost_CE_add":= 0]

    dt_p[, c("base_csgd_MCF", "green_csgd_MCF"):=
        dt_l[leaseIDs, lapply(.(base_perm, green_perm), function(x) sum(x[[1]] * csgd_MCF))], by=firmID]


    # project revenue based on past market conditions
    dt_p[, "add_gas_revenue":= (green_csgd_MCF * (market$prices$dirty + market$green_coeff)) -
                                (base_csgd_MCF * (market$prices$dirty + ifelse(K>0, 0, market$green_coeff)))]

    setkey(dt_p, firmID)

    return(dt_p)

}###--------------------    END OF FUNCTION build_permutations      --------------------###



#*
#*** FIRM ACTIVITIES ***#
#*

find_imitators <- function(dt_f, ti, success_metric="sales") {
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


optimize_strategy <- function(dt_f, dt_l, market, ti) {
    dt_p <- build_permutations(dt_l[firmID %in% dt_f[activity=="development"]$firmID][status!="retired"], market, ti)

    # update market conditions
    dt_p[dt_f, on="firmID", "sPressure":= sPressure]

    # is casinghead gas capture economical even without social pressure?
    dt_p[, "economical":= (sapply(Map(`==`, base_perm, green_perm), all) | cost_M_add<=0)]
    dt_p[, "option":= ifelse(economical==TRUE, "green", NA_character_)]


    # if the possible threat outweighs the cost, exercise the mitigation option
    dt_p[is.na(option), "option":= ifelse((cost_M_add * (1-ti$SRoR)) - add_gas_revenue < sPressure, "green", "grey")]

    # imitators will mitigate even if it is not strictly more economical
    imitators <- find_imitators(dt_f, ti)
    dt_p[option=="grey", "imitation":= (firmID %in% imitators) & (economical==FALSE)]
    dt_p[imitation==TRUE, "option":= "green"]


    # firms participating in exploration activities do no new development
    dt_p[firmID %in% dt_f[activity=="exploration"], "option":= NA]

    # optimal decision for which assets to developed
    dt_p[!is.na(option), "devIDs":= Map(`[`, leaseIDs,
                                            lapply(ifelse(option=="green", green_perm, base_perm), as.logical))]

    return(dt_p)
}###--------------------    END OF FUNCTION optimize_strategy       --------------------###


do_development <- function(dt_f, dt_l, dt_p, ti) {
    # which firms are upgrading assets
    devs <- dt_p[!is.na(option)][lengths(devIDs)>0, firmID]

    ## Update lease attributes
    # update lease classes to reflect new development
    dt_l[.(dt_p[firmID %in% devs, unlist(devIDs)]), c("class", "t_switch"):= .("developed", ti$time)]

    ## Update firm attributes
    # whether they are mitigating and if they are doing so because of simple economics (besides social pressure)
    dt_f[dt_p, on="firmID", "behavior":= ifelse(option=="green", "mitigating", "flaring")]
    dt_f[dt_p[economical==TRUE], on="firmID", "behavior":= "economizing"]
    dt_f[dt_p[imitation==TRUE], on="firmID", "behavior":= "imitating"]

    # gas output from development
    dt_l[firmID %in% devs, sprintf("opEx_%s", c("oil","csgd","gas")):= calc_opEx(.SD)]
    dt_f[dt_l[firmID %in% devs, .SD[(class=="developed") & (status=="producing"), sum(gas_MCF+csgd_MCF)], by=firmID],
        on="firmID", "gas_output":= V1]
    dt_f[dt_l[firmID %in% devs, .SD[(class=="underdeveloped") & (status=="producing"), sum(csgd_MCF)], by=firmID],
        on="firmID", "gas_flared":= V1]


}###--------------------    END OF FUNCTION do_development          --------------------###


do_exploration <- function(dt_f, dt_l, ti) {
    new_discs <- dt_f[(activity=="exploration") & (runif(.N) < ti$prob_e)]$firmID
    new_output <- unique(dt_l[status=="stopped" | (lifetime + t_found - ti$time)==0]$firmID)

    # progress leases from previous turns
    #    newly discovered leases are undeveloped
    #    in the following time step, they progress to underdeveloped with stopped production
    #    at this point, firms can decide whether to fully develop them
    #    in the following time step, the leases begin production
    dt_l[status=="stopped", "status":= "producing"]
    dt_l[class=="undeveloped", c("class", "status"):= .(ifelse(csgd_MCF==0, "developed", "underdeveloped"), "stopped")]
    # retire leases at the end of their lifetime
    dt_l[(lifetime + t_found - ti$time) == 0, "status":= "retired"]

    # probabilistically discover new leases
    dt_l[sample(which(is.na(firmID)), length(new_discs)),
        c("firmID", "class", "t_found"):= .(new_discs, "undeveloped", ti$time)]

    ## Update firm attributes
    # gas output from exploration
    dt_f[dt_l[firmID %in% new_output, .SD[(status=="producing") & (class=="developed"), sum(gas_MCF+csgd_MCF)], by=firmID],
            on="firmID", "gas_output":= .(V1)]
    dt_f[dt_l[firmID %in% new_output, .SD[(status=="producing") & (class=="underdeveloped"), sum(csgd_MCF)], by=firmID],
            on="firmID", "gas_flared":= V1]
    # additional oil output from exploration
    dt_f[dt_l[firmID %in% new_output, .SD[(status=="producing") & (class!="undeveloped"), sum(oil_BBL+cond_BBL)], by=firmID],
            on="firmID", c("oil_output","oil_revenue"):= .(V1, V1 * ti$oil_price)]

    dt_f[firmID %in% new_output, "behavior":= ifelse((gas_flared/oil_output) > ti$threshold, "flaring",
                                                        ifelse(behavior=="flaring", "economizing", behavior))]

}###--------------------    END OF FUNCTION do_exploration          --------------------###
