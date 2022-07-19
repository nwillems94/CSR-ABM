# THIS SCRIPT CONTAINS THE DECISION MAKING FUNCTIONALITY OF THE flaringABM
# VERBS (e.g., calc_debits) MODIFY (data.table's) IN-PLACE, NOUNS RETURN OBJECTS
#
#
library(data.table)



#*
#*** SOCIAL PRESSURE ***#
#*

total_pressure <- function(Ai) {
    # Calculate the total social pressure

    return(Ai)
}###--------------------    END OF FUNCTION total_pressure          --------------------###


dist_social_pressure <- function(dt_f, ti, method="even", focus=1) {
    # Determine what proportion of the total social pressure is allocated to each agent
    a <- total_pressure(ti$Activism)
    dt_f[, "sPressure":= 0]

    if (method=="even") {
        dt_f[behavior=="flaring", "sPressure":= a / .N]
    } else if (method=="flaring") {
        dt_f[behavior=="flaring", "sPressure":= a * gas_flared / sum(gas_flared)]
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

    # the effect on market value can be up to 20% [King and Soule, 2007](zotero://select/items/0_TWS6EB4J)
    dt_f[, "sPressure":= pmin(sPressure, market_value*0.2, na.rm=TRUE)]

}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###



#*
#*** FIRM VALUATION ***#
#*

lease_opEx <- function(dt_l) {
    # determine operation costs based on lease outputs
    # MCF to BOE Conversion: https://petrowiki.spe.org/Glossary:Barrels_of_oil_equivalent
    dt_l[,  # amount of casinghead gas produced
            {csgd_developed_MCF= ifelse(class %in% "developed", csgd_MCF, 0);
            # operating cost in BOE terms
            opEx_pBOE= opEx / (oil_BBL + cond_BBL + (gas_MCF + csgd_developed_MCF)/6);
            # breakdown of cost by output
            .(cbind("oil"=oil_BBL+cond_BBL, "csgd"=csgd_developed_MCF, "gas"=gas_MCF) *
                (sapply(c(1, 1/6, 1/6), `*`, opEx_pBOE) + cbind(opEx_pBBL, opEx_pMCF, opEx_pMCF)))}]
}###--------------------    END OF FUNCTION lease_opEx              --------------------###


calc_debits <- function(dt_f, dt_l) {
    # baseline operating costs
    dt_f[dt_l[, .SD[status=="producing", sum(opEx_oil + opEx_gas)], by=firmID], on="firmID", "cost_O":= V1]

    # additional mitigating operating costs
    dt_f[dt_l[, .SD[class=="developed" & status=="producing", sum(opEx_csgd)], by=firmID], on="firmID", "cost_M":= V1]

    # capital expenditures which are paid off over the lease lifetime
    dt_f[dt_l[, .SD[status!="retired", sum(capEx / lifetime)], by=firmID], on="firmID", "cost_CE":= V1]
    dt_f[dt_l[, .SD[!is.na(t_switch) & status!="retired", sum(capEx_csgd / (lifetime + t_found - t_switch))], by=firmID],
        on="firmID", "cost_CE":= cost_CE + V1]

}###--------------------    END OF FUNCTION calc_debits             --------------------###


calc_credits <- function(dt_f, market) {
    dt_f[, "gas_revenue":= grey_gas_sold * market$p_grey + green_gas_sold * market$p_green]
    dt_f[, "oil_revenue":= oil_output * (market$p_grey * market$p_oil_mult)]

}###--------------------    END OF FUNCTION calc_credits            --------------------###



#*
#*** FIRM ACTIVITIES ***#
#*
market_price <- function(dt_l, demand_schedule, market_segment) {
    # market price from intersection of supply and demand
    price <- dt_l[market==market_segment][order(opEx_pMCF)][,
                    # supply curve
                    {"p"= c(0,opEx_pMCF); "q"=c(0,cumsum(gas_MCF+csgd_MCF));
                    # price willing to be paid at every point of supply curve
                    "p_demand"= demand_schedule(q)[, get(paste0("p_", market_segment))];
                    # intersection of supply and demand
                    p_demand[nafill(which(p_demand <= p)[1], fill=.N+1)]}]

    return(price)

}###--------------------     END OF FUNCTION market_price           --------------------###

market_allocation <- function(dt_l, demand_schedule, firmIDs, mp_grey) {

    max_profit <- dt_l[(market=="green") & (firmID %in% firmIDs)][order(opEx_pMCF)][,
                    .("leaseIDs"= c(NA_integer_, leaseID), "cutoff"= c(0, opEx_pMCF)), by=firmID]

    # Cournot price in the green market at each level of allocation
    max_profit[, "cp_green":= sapply(1:.N, function(x) market_price(dt_l[!.(leaseIDs[1:x])], demand_schedule, "green")),
            by=firmID]

    # revenue given output allocation between markets
    max_profit[, "revenue":= dt_l[firmID==.BY][order(opEx_pMCF),
                                    c(gas_MCF+csgd_MCF) %*% sapply(1:length(leaseIDs), function(x)
                                                                ifelse((opEx_pMCF>cutoff[x]) & (opEx_pMCF<=cp_green[x]),
                                                                    cp_green, ifelse(opEx_pMCF<=mp_grey, mp_grey, 0)))],
            by=firmID]

    return(max_profit[, .SD[which.max(revenue)], by=firmID])

}###--------------------     END OF FUNCTION market_allocation      --------------------###

clear_gas_markets <- function(dt_f, dt_l, dt_m, demand_schedule, ti) {
    # consider all developed producing leases
    dt_l[, "market":= ifelse((class=="developed") & (status=="producing") & (gas_MCF>0 | csgd_MCF>0),
                        replace(market, is.na(market) | market=="none", "grey"), NA)]
    dt_l[dt_f[behavior=="flaring"], on="firmID", "market":= replace(market, !is.na(market), "grey")]

    if(ti$market_prop_green>0) {
        # market conditions under previous market allocations
        if(dt_l[market=="green", .N==0]) {
            # first approximation allocates all eligible leases to the green market
            dt_l[dt_f[behavior!="flaring"], on="firmID", "market":= replace(market, !is.na(market), "green")]
        }

        # clear the green market first
        mp0_green <- market_price(dt_l, demand_schedule, "green")
        # supply not purchased in the green market falls back to the grey market
        dt_l[(market=="green") & (opEx_pMCF > mp0_green), "market":= "grey"]
        # clear grey market with remaining supply
        mp0_grey <- market_price(dt_l, demand_schedule, "grey")
        dt_l[(market=="grey") & (opEx_pMCF > mp0_grey), "market":= "none"]

        dt_l[dt_f[behavior!="flaring"], on="firmID", "market":= replace(market, market=="none", "green")]

        # determine how to allocate production between green and grey markets
        if (dt_l[market=="green", .N] > 0) {
            optimal_allocation <- market_allocation(dt_l, demand_schedule, dt_f$firmID, mp0_grey)
            dt_l[optimal_allocation, on="firmID",
                "market":= replace(market, (market=="green") & (opEx_pMCF<=cutoff), "grey"), by=.EACHI]
        }
    }

    # determine market prices based on agent allocations
    # clear green market first
    mp_green <- market_price(dt_l, demand_schedule, "green")
    # supply not purchased in the green market falls back to the grey market
    dt_l[(market=="green") & (opEx_pMCF > mp_green), "market":= "grey"]
    # clear grey market with remaining supply
    mp_grey <- market_price(dt_l, demand_schedule, "grey")
    dt_l[(market=="grey") & (opEx_pMCF > mp_grey), "market":= "none"]

    # increment remaining gas production
    dt_l[market %in% c("green", "grey"), "ERR_MCF":= ERR_MCF - (gas_MCF+csgd_MCF)]

    # defer production to later
    dt_l[market=="none", "lifetime":= lifetime+1]

    # update firm outputs
    dt_f[dt_l[, .SD[market=="grey", sum(gas_MCF+csgd_MCF)], by=firmID], on="firmID", "grey_gas_sold":= V1]
    dt_f[dt_l[, .SD[market=="green", sum(gas_MCF+csgd_MCF)], by=firmID], on="firmID", "green_gas_sold":= V1]
    dt_f[, "gas_output":= grey_gas_sold + green_gas_sold]

    # store current market prices and quantities
    dt_m[.(ti$time), c("p_grey", "p_green"):= .(mp_grey, mp_green)]
    dt_m[.(ti$time), "q_grey":= dt_f[, sum(grey_gas_sold)]]
    dt_m[.(ti$time), "q_green":= dt_f[, sum(green_gas_sold)]]
    dt_m[.(ti$time), "q_oil":= dt_f[, sum(oil_output)]]

}###--------------------    END OF FUNCTION clear_gas_markets       --------------------###


imitators <- function(dt_f, ti, success_metric="sales") {
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

}###--------------------    END OF FUNCTION imitators               --------------------###


optimal_strategy <- function(dt_f, dt_l, dt_m, demand_schedule, ti) {
    # evaluate the economics of pending leases and make green development decisions
    firmIDs <- union(dt_l[(status=="pending") & (class=="underdeveloped"), firmID],
                    dt_f[activity=="development", firmID])

    if(dt_l[firmID %in% firmIDs][((lifetime + t_found - 1) > ti$time) & (class=="underdeveloped"), .N]==0) {
        return(setNames(data.table(matrix(nrow=0, ncol=5)),
                    c("firmID", "option", "economical", "imitation", "devIDs")))
    }

    # average cost = current opEx including additional capEx required for casinghead gas capture
    ## additional (e.g., infrastructure) cost are assumed to be borne by midstream firms
    AC_pMCF <- dt_l[, opEx_pMCF + nafill(capEx_csgd / ERR_MCF, fill=0)]

    dt_p <- dt_l[order(AC_pMCF)][firmID %in% firmIDs][(lifetime + t_found - 1) > ti$time,
                    .("leaseIDs"= .(leaseID[class=="underdeveloped"]),
                    # current oil output
                    "prod_BBL"= sum(oil_BBL),
                    # current saftey flaring
                    "sopf_MCF"= sum(sopf_MCF),
                    # historical (pessimistic) grey gas market price
                    "p_grey"= min(tail(dt_m[time<=ti$time]$p_grey, 12), na.rm=TRUE),
                    "base_perm"=list(), "green_perm"=list()),
                keyby=firmID][lengths(leaseIDs)>0]
    # firms not doing development only consider pending leases
    dt_p[dt_f[activity=="exploration"], on="firmID",
        "leaseIDs":= .(.(dt_l[(firmID==.BY) & (status=="pending") & (class=="underdeveloped"), leaseID])), by=.EACHI]

    # develop leases which would have an average cost below the conventional market price for gas
    dt_p[, "base_perm":= cbind(dt_l, AC_pMCF)[leaseIDs, .(.(as.numeric(AC_pMCF<p_grey)))], by=firmID]

    # for firms doing development, calculate additional gas capture (wrt base) necessary to meet green threshold
    dt_p[, "K":= sopf_MCF + dt_l[leaseIDs, (1-base_perm[[1]]) %*% csgd_MCF] - (ti$threshold * prod_BBL), by=firmID]
    dt_p[sopf_MCF > (ti$threshold * prod_BBL), "K":= NA]

    dt_p[dt_f[activity=="exploration"], on="firmID", "green_perm":= base_perm, by=.EACHI]
    dt_p[lengths(green_perm)<lengths(base_perm),
        "green_perm":= .(ifelse(K<=0 | is.na(K), base_perm,
                            Map(pmax, base_perm, dt_l[leaseIDs, shift(cumsum(csgd_MCF)<K, fill=1)]))), by=firmID]

    # calculate the additional cost and revenue associated with exercising the green option
    #   no need to calculate where base and green permutations are the same
    dt_p[sapply(Map(`==`, base_perm, green_perm), all), c("cost_M_add", "gas_revenue_add"):= 0]

    if(dt_p[is.na(gas_revenue_add), .N]>0) {
        dt_p[is.na(cost_M_add), "cost_M_add":=
                dt_l[leaseIDs, (green_perm[[1]] - base_perm[[1]]) %*%
                            ((opEx_pMCF * ERR_MCF) + nafill(capEx_csgd, fill=0))],
            by=firmID]

        # green revenue
        if(ti$market_prop_green==0) {
            dt_p[is.na(gas_revenue_add), "green_revenue":=
                dt_l[(firmID==.BY) & ((lifetime + t_found - 1) > ti$time),
                                # account for existing and green leases
                                (nafill(green_perm[[1]][match(leaseID, leaseIDs[[1]])], fill=1) * ERR_MCF) %*%
                                # account for whether gas is sold
                                (p_grey * (opEx_pMCF<=p_grey))], by=firmID]
        } else {
            is_green <- dt_l[, (leaseID %in% dt_p[, unlist(leaseIDs)[as.logical(unlist(green_perm))]]) |
                                ((class=="developed") & ((lifetime + t_found - 1) > ti$time))]

            dt_p[is.na(gas_revenue_add), c("cutoff","cp_green"):=
                    market_allocation(dt_l[, replace(.SD, "market", ifelse(is_green & (firmID==.BY), "green", market))],
                                        demand_schedule, .BY, p_grey)[, .(cutoff, cp_green)], by=firmID]

            dt_p[is.na(gas_revenue_add), "green_revenue":=
                dt_l[(firmID==.BY) & ((lifetime + t_found - 1) > ti$time),
                                # account for existing and green leases
                                (nafill(green_perm[[1]][match(leaseID, leaseIDs[[1]])], fill=1) * ERR_MCF) %*%
                                # account for where gas is sold
                                (ifelse((opEx_pMCF>cutoff) & (opEx_pMCF<=cp_green), cp_green,
                                        ifelse(opEx_pMCF<=p_grey, p_grey, 0)))], by=firmID]
        }

        # calculate revenue under base scenario
        dt_p[(K>0) & is.na(gas_revenue_add), "base_revenue":=
            dt_l[(firmID==.BY) & ((lifetime + t_found - 1) > ti$time),
                            # account for existing and base leases
                            (nafill(base_perm[[1]][match(leaseID, leaseIDs[[1]])], fill=1) * ERR_MCF) %*%
                            # account for whether gas is sold
                            (p_grey * (opEx_pMCF<=p_grey))], by=firmID]

        # additional revenue from exercising the green option
        dt_p[is.na(gas_revenue_add), "gas_revenue_add":= green_revenue - base_revenue]
    }

    # update market conditions weighted by investment horizon
    dt_p[dt_f, on="firmID", "sPressure":= sPressure * max(dt_l[firmID==.BY, lifetime + t_found - ti$time - 1]), by=.EACHI]

    # is casinghead gas capture economical even without social pressure?
    dt_p[, "economical":= ((K<=0) | (gas_revenue_add > cost_M_add))]
    dt_p[, "option":= fifelse(economical==TRUE, "green", NA_character_, na=NA_character_)]
    dt_p[dt_f[activity=="exploration"], on="firmID", "option":= replace(option, is.na(option), "grey")]

    # if the possible threat outweighs the cost, exercise the mitigation option
    dt_p[is.na(option), "option":= ifelse((cost_M_add * (1-ti$SRoR)) - gas_revenue_add < sPressure, "green", "grey")]

    # imitators will mitigate even if it is not strictly more economical
    dt_p[option=="grey", "imitation":= (firmID %in% imitators(dt_f, ti)) & (economical==FALSE)]
    dt_p[imitation==TRUE, "option":= "green"]

    # optimal decision for which assets to developed
    dt_p[!is.na(option), "devIDs":= Map(`[`, leaseIDs,
                                            lapply(ifelse(option=="green", green_perm, base_perm), as.logical))]

    return(dt_p)

}###--------------------    END OF FUNCTION optimal_strategy        --------------------###

do_development <- function(dt_f, dt_l, dt_p, ti) {
    ## Update lease attributes
    # update lease classes to reflect new development
    dt_l[.(dt_p[, unlist(devIDs)]), c("class", "t_switch"):= .("developed", ti$time)]
    # update the break down of operating costs
    dt_l[.(dt_p[, unlist(devIDs)]), sprintf("opEx_%s", c("oil","csgd","gas")):= lease_opEx(.SD)]

    ## Update firm attributes
    # whether they are mitigating and if they are doing so because of simple economics (besides social pressure)
    dt_f[dt_p, on="firmID", "behavior":= ifelse(option=="green", "mitigating", "flaring")]
    dt_f[dt_p[economical==TRUE], on="firmID", "behavior":= "economizing"]
    dt_f[dt_p[imitation==TRUE], on="firmID", "behavior":= "imitating"]

}###--------------------    END OF FUNCTION do_development          --------------------###


do_exploration <- function(dt_f, dt_l, ti) {
    retiring_leases <- dt_l[(lifetime + t_found + 1) == ti$time]$leaseID
    new_discs <- dt_l[dt_f[(activity=="exploration")], on="firmID",
                    .SD[leaseID %in% retiring_leases, sum(oil_BBL + gas_MCF/6)], by=.EACHI][,
                        sample(firmID, sum(runif(.N) < ti$prob_e * dt_l[.(retiring_leases), sum(oil_BBL+cond_BBL)]),
                                        prob=V1+10, replace=TRUE)]
    # agents with no assets will discover new ones
    new_discs <- c(new_discs, setdiff(dt_f[(activity=="exploration") & (gas_output+oil_output==0)]$firmID, new_discs))

    new_output <- unique(dt_l[union(retiring_leases, which(status=="pending"))]$firmID)

    # new discovered leases begin as undeveloped, then progress to underdeveloped with pending production
    #  (at this point, firms can decide whether to fully develop them), then finally begin production
    dt_l[status=="pending", "status":= "producing"]
    dt_l[class=="undeveloped", c("class", "status"):= .(ifelse(csgd_MCF==0, "developed", "underdeveloped"), "pending")]
    # retire leases at the end of their lifetime
    dt_l[.(retiring_leases), "status":= "retired"]

    # probabilistically discover new leases
    if (length(new_discs)>0) {
        # prevalence of oil and gas leases among new_discs
        weights <- dt_l[firmID %in% new_discs, .SD[status!="retired", .N], by=.(firmID, OIL_GAS_CODE)][,
                        sum(V1*table(new_discs)[as.character(firmID)]), by=OIL_GAS_CODE][, setNames(V1, OIL_GAS_CODE)]
        weights[setdiff(c("O","G"), names(weights))] <- 0
        weights <- if(sum(weights)==0) replace(weights, c("O","G"), 1) else weights

        new_leases <- dt_l[is.na(firmID)][sample(.N, length(new_discs), prob=weights[OIL_GAS_CODE])]

        dt_l[.(new_leases[oil_BBL==0][order(-gas_MCF)]$leaseID), c("firmID", "class", "t_found"):=
                .(dt_f[.(new_discs)][order(-production_MCF)[1:length(leaseID)], firmID], "undeveloped", ti$time)]
        dt_l[.(new_leases[oil_BBL>0][order(-oil_BBL)]$leaseID), c("firmID", "class", "t_found"):=
                .(dt_f[.(new_discs)][order(-production_BBL)[1:length(leaseID)], firmID], "undeveloped", ti$time)]
    }

}###--------------------    END OF FUNCTION do_exploration          --------------------###
