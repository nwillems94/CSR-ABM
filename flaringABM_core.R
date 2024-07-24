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


dist_social_pressure <- function(dt_f, ti, focus=1) {
    # Determine what proportion of the total social pressure is allocated to each agent
    a <- total_pressure(ti$Activism)
    dt_f[, "sPressure":= 0]

    if (ti$strategy=="even") {
        dt_f[behavior=="flaring", "sPressure":= a / .N]
    } else if (ti$strategy=="flaring") {
        dt_f[behavior=="flaring", "sPressure":= a * gas_flared / sum(gas_flared)]
    } else if (ti$strategy=="focused") {
        dt_f[(firmID %in% focus) & behavior=="flaring", "sPressure":= a / .N]
    } else if (ti$strategy=="gas_output") {
        dt_f[behavior=="flaring", "sPressure":= a * gas_output / sum(gas_output)]
    } else if (ti$strategy=="oil_output") {
        dt_f[behavior=="flaring", "sPressure":= a * oil_output / sum(oil_output)]
    } else if (ti$strategy=="top") {
        dt_f[(behavior=="flaring") &
                (gas_flared > quantile(gas_flared, 0.9)),
            "sPressure":= a * gas_flared / sum(gas_flared)]
    }
    # pressure on leader firms
    else if (ti$strategy=="leaders") {
        dt_f[(behavior=="flaring") &
                (market_value > quantile(market_value, 0.9)),
            "sPressure":= a * market_value / sum(market_value)]
    } else if (ti$strategy=="shock") {
        if (between(ti$time, 1, 12)) {
            dt_f[(behavior=="flaring") &
                    (oil_output > quantile(oil_output, 0.9)),
                "sPressure":= a * oil_output / sum(oil_output)]
        }
    }

    # the effect on market value can be up to 20% [King and Soule, 2007](zotero://select/items/0_TWS6EB4J)
    dt_f[, "sPressure":= pmin(sPressure, market_value*0.2, na.rm=TRUE)]

}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###



#*
#*** FIRM VALUATION ***#
#*

lease_costs <- function(dt_l) {
    # determine operation costs based on lease outputs
    # MCF to BOE Conversion: https://petrowiki.spe.org/Glossary:Barrels_of_oil_equivalent
    dt_l[,  # amount of casinghead gas produced
            {csgd_developed_MCF= fifelse(class=="developed", csgd_MCF, 0, na=0);
            # operating cost per BOE actually produced
            BOE_base= oil_BBL + cond_BBL + gas_MCF/6;
            opEx_pBOE_mod= opEx_pBOE * BOE_emp / (BOE_base + csgd_developed_MCF/6);
            # capital expenditures paid off over lease lifetime
            capEx_pBOE= capEx / lifetime / BOE_base;
            capEx_csgd_pMCF= nafill(capEx_csgd / (lifetime + t_found - t_switch) / csgd_developed_MCF, fill=0);
            #   production
            .(cbind("oil"=oil_BBL+cond_BBL, "csgd"=csgd_developed_MCF, "gas"=gas_MCF) *
            #   fixed operating costs
                (cbind(opEx_pBOE_mod,       opEx_pBOE_mod/6,            opEx_pBOE_mod/6) +
            #   plus variable operating costs
                cbind(opEx_pBBL,            opEx_pMCF,                  opEx_pMCF) +
            #   plus capital costs
                cbind(capEx_pBOE,           capEx_csgd_pMCF,            capEx_pBOE/6)))}]

}###--------------------    END OF FUNCTION lease_costs             --------------------###


calc_debits <- function(dt_f, dt_l) {
    # baseline cost of production
    dt_f[dt_l[, .SD[(is.na(market) | market!="none") & (status=="producing"),
                    sum(cost_oil + cost_gas)], by=firmID], on="firmID", "cost_P":= cost_P+V1]

    # additional costs from mitigating
    dt_f[dt_l[, .SD[(market!="none") & (status=="producing") & (class=="developed"),
                    sum(cost_csgd)], by=firmID], on="firmID", "cost_M":= cost_M+V1]

}###--------------------    END OF FUNCTION calc_debits             --------------------###


calc_credits <- function(dt_f, market) {
    dt_f[, "gas_revenue":= gas_revenue + (grey_gas_sold * market$p_grey + green_gas_sold * market$p_green)]
    dt_f[, "oil_revenue":= oil_revenue + (oil_output * (market$p_grey * market$p_oil_mult))]

}###--------------------    END OF FUNCTION calc_credits            --------------------###



#*
#*** FIRM ACTIVITIES ***#
#*
imitators <- function(dt_f, dt_p, ti, success_metric="sales") {
    # determine who is an imitator - less successful followers mimic more successful leaders
    # following Leary & Roberts "success" can be defined in terms of sales (market share), profit, or market_value
    observations <- dt_f[, {"weight"= 1+get(success_metric);
                            # agents doing exploration cannot imitate
                            "I"= which(activity=="development");
                            .("observer"= firmID[I], "weight"= weight[I], "imitation"= NA_character_,
                            # more successful agents are more likely to be imitated
                            "observed"= sapply(I, function(x) sample(behavior[-x], 1, prob=weight[-x])))}]

    ## agents considering staying grey may instead copy other green firms
    observations[dt_p[(option=="grey") & !is.na(K)], on=c(observer="firmID"),
            "imitation":= replace(imitation, observed %in% c("economizing", "mitigating", "imitating"), "green")]
    ## agents considering going green may change its mind (as long as it's not happening because of standard development)
    if (!((ti$strategy=="shock") & between(ti$time, 1, 12))) {
        observations[dt_p[(option=="green") & (K>0)], on=c(observer="firmID"),
            "imitation":= replace(imitation, observed=="flaring", "grey")]
    }

    # moderate rate of imitation, less sucessful agents are more likely to imitate
    return(observations[sample(.N, sum(runif(.N)<ti$prob_m), prob=1/weight)][!is.na(imitation)]$observer)

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
                    "p_green"= min(tail(dt_m[time<=ti$time]$p_green, 12), na.rm=TRUE),
                    "base_perm"=list(), "green_perm"=list()),
                keyby=firmID][lengths(leaseIDs)>0]

    # add firm attributes
    dt_p[dt_f, on="firmID", "activity":= activity]

    # firms not doing development only consider pending leases
    dt_p[activity=="exploration",
        "leaseIDs":= .(.(dt_l[(firmID==.BY) & (status=="pending") & (class=="underdeveloped"), leaseID])), by=firmID]

    # develop leases which would have an average cost below the conventional market price for gas
    dt_p[, "base_perm":= cbind(dt_l, AC_pMCF)[leaseIDs, .(.(as.numeric(AC_pMCF<p_grey)))], by=firmID]

    # for firms doing development, calculate additional gas capture (wrt base) necessary to meet green threshold
    dt_p[, "K":= sopf_MCF + dt_l[leaseIDs, (1-base_perm[[1]]) %*% csgd_MCF] - (ti$threshold * prod_BBL), by=firmID]
    dt_p[sopf_MCF > (ti$threshold * prod_BBL), "K":= NA]

    dt_p[activity=="exploration", "green_perm":= base_perm, by=firmID]

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
            # allocate assets assuming pessimistic prices
            dt_p[is.na(gas_revenue_add), "green_revenue":=
                dt_l[(firmID==.BY) & ((lifetime + t_found - 1) > ti$time),
                                # account for existing and green leases
                                (nafill(green_perm[[1]][match(leaseID, leaseIDs[[1]])], fill=1) * ERR_MCF) %*%
                                # account for where gas is sold
                                (ifelse((opEx_pMCF>p_grey) & (opEx_pMCF<=p_green), p_green,
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
    dt_p[activity=="exploration", "option":= replace(option, is.na(option), "grey")]

    # if the possible threat outweighs the cost, exercise the mitigation option
    dt_p[is.na(option), "option":= ifelse((cost_M_add * (1-ti$SRoR)) - gas_revenue_add < sPressure, "green", "grey")]

    # imitators will ignore economics and copy a the behavior of a successful firm
    # reverse decision because of imitation
    dt_p[, "imitation":= (firmID %in% imitators(dt_f, dt_p, ti))]
    dt_p[imitation==TRUE, "option":= unname(c("grey"="green", "green"="grey")[option])]
    # only register grey-to-green as imitators
    dt_p[(imitation==TRUE) & (option=="grey"), "imitation":= FALSE]


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
    dt_l[.(dt_p[, unlist(devIDs)]), sprintf("cost_%s", c("oil","csgd","gas")):= lease_costs(.SD)]

    ## Update firm attributes
    # whether they are mitigating and if they are doing so because of simple economics (besides social pressure)
    dt_f[dt_p, on="firmID", "behavior":= ifelse(option=="green", "mitigating", "flaring")]
    dt_f[dt_p[economical==TRUE], on="firmID", "behavior":= "economizing"]
    dt_f[dt_p[imitation==TRUE], on="firmID", "behavior":= "imitating"]

}###--------------------    END OF FUNCTION do_development          --------------------###


do_exploration <- function(dt_f, dt_l, ti) {
    retiring_leases <- dt_l[(lifetime + t_found + 1) == ti$time]$leaseID

    # new discovered leases begin as undeveloped, then progress to underdeveloped with pending production
    #  (at this point, firms can decide whether to fully develop them), then finally begin production
    dt_l[status=="pending", "status":= "producing"]
    dt_l[class=="undeveloped", c("class", "status"):= .(ifelse(csgd_MCF==0, "developed", "underdeveloped"), "pending")]
    # retire leases at the end of their lifetime
    dt_l[.(retiring_leases), "status":= "retired"]

    N_discs <- dt_l[, dcast(.SD, firmID~OIL_GAS_CODE, fun=function(x) sum(x!="retired"), value.var="status")][
                    dt_f[activity=="exploration"], on="firmID"][,
                    sum(runif(.N) < ti$prob_e * dt_l[.(retiring_leases), sum(oil_BBL+cond_BBL + (gas_MCF+csgd_MCF)/6)])
                    - sum(((O==0) & (production_BBL>0)) + ((G==0) & (production_MCF>0)))]

    new_discs <- dt_l[status=="retired"][dt_f[activity=="exploration"], on="firmID"][,
                        -sample(-firmID, max(ceiling(N_discs), 0), prob= 1 + (leaseID %in% retiring_leases))]

    # agents with no assets will discover new ones
    new_discs <- dt_l[, dcast(.SD, firmID~OIL_GAS_CODE, fun=function(x) sum(x!="retired"), value.var="status")][
                    dt_f[activity=="exploration"], on="firmID",
                    c(new_discs, setdiff(firmID[(O==0) & (production_BBL>0)], new_discs),
                                setdiff(firmID[(G==0) & (production_MCF>0)], new_discs))]

    # probabilistically discover new leases
    if (length(new_discs)>0) {
        # prevalence of oil and gas leases among new_discs
        weights <- dt_l[status=="retired"][.(new_discs), on="firmID", allow.cartesian=TRUE, nomatch=0,
                        sum(1 + (leaseID %in% retiring_leases)), keyby=.(OIL_GAS_CODE)][
                       .(c("O","G")), setNames(nafill(V1, fill=0), OIL_GAS_CODE)]

        weights <- dt_l[is.na(firmID), .N, by=OIL_GAS_CODE][, weights[OIL_GAS_CODE] / N]

        # assign newly discovered leases to firms
        new_leases <- dt_l[is.na(firmID)][sample(.N, length(new_discs), prob=weights[OIL_GAS_CODE])]

        dt_l[.(new_leases$leaseID), c("class", "t_found"):= .("undeveloped", ti$time)]
        dt_l[.(new_leases[OIL_GAS_CODE=="G"][order(-capEx), leaseID]),
            "firmID":= dt_f[.(new_discs)][, "shift":= ifelse(production_MCF>0, exp(mean(log(1+production_MCF))), 0)][,
                            -sample(-firmID, length(leaseID), prob= 1 + production_MCF + shift)]]
        dt_l[.(new_leases[OIL_GAS_CODE=="O"][order(-capEx), leaseID]),
            "firmID":= dt_f[.(new_discs)][, "shift":= ifelse(production_BBL>0, exp(mean(log(1+production_BBL))), 0)][,
                            -sample(-firmID, length(leaseID), prob= 1 + production_BBL + shift)]]

    }

}###--------------------    END OF FUNCTION do_exploration          --------------------###



#*
#*** MARKETS ***#
#*
market_price <- function(dt_l, demand_schedule, market_segment) {
    # market price from intersection of supply and demand
    price <- dt_l[market==market_segment][order(opEx_pMCF)][,
                    # supply curve
                    {"p"= c(0, opEx_pMCF); "q"= c(0, cumsum(gas_MCF+csgd_MCF));
                    # price willing to be paid at every point of supply curve
                    "p_demand"= demand_schedule(q)[, get(paste0("p_", market_segment))];
                    # intersection of supply and demand
                    p_demand[nafill(which(p_demand <= p)[1], fill=.N+1)]}]

    return(price)

}###--------------------     END OF FUNCTION market_price           --------------------###


market_allocation <- function(dt_l, demand_schedule, firmIDs, mp_grey) {

    q_green <- dt_l[market=="green", sum(gas_MCF+csgd_MCF)]
    b_green <- demand_schedule(0)$p_green

    q_shift <- 0
    if (b_green < mp_grey) {
        # leave the green market if the max(mp_green) < mp_grey
        q_shift <- -Inf
    } else if (q_green > 0) {
        # upper bound on q_shift by assuming supply curve is vertical at intersection with demand
        q_shift <- -q_green * (1 - (mp_grey - b_green) / (demand_schedule(q_green/100)$p_green - b_green) / 100)
    }

    max_profit <- dt_l[firmID %in% firmIDs][order(opEx_pMCF),
                        .SD[!is.na(market) & (opEx_pMCF<=mp_grey)][,
                        .("leaseIDs"= c(NA_integer_, leaseID), "cutoff"= c(0, opEx_pMCF))], by=firmID]

    max_profit[, "sequence":= seq(.N), by=firmID]
    if (q_shift<0) {
        # green price lower than grey price, need to remove q_shift or more units from green to grey
        max_profit[dt_l[market=="green"], on=c(leaseIDs="leaseID"), "MCF":= gas_MCF+csgd_MCF]
        max_profit[, "sequence":= replace(sequence, cumsum(nafill(MCF, fill=0)) < -q_shift, NA), by=firmID]
    } else if (q_shift>0) {
        # green price higher than grey price, can add at most q_shift units from grey to green
        max_profit[dt_l[market=="grey"], on=c(leaseIDs="leaseID"), "MCF":= gas_MCF+csgd_MCF]
        max_profit[, "sequence":= replace(sequence, rev(cumsum(rev(nafill(MCF, fill=0)))) > q_shift, NA), by=firmID]
    }
    max_profit[, "sequence":= replace(sequence, .N, .N), by=firmID]

    max_profit[, "cp_green":= if(.N==1) market_price(dt_l, demand_schedule, "green") else NA_real_, by=firmID]

    # Cournot price in the green market at each level of allocation
    max_profit[is.na(cp_green), "cp_green":=
        dt_l[, replace(.SD, "market", ifelse((firmID==.BY) & !is.na(market), "green", market))[,
            sapply(sequence, function(x)
                if (is.na(x)) NA else market_price(.SD[!.(leaseIDs[1:x])], demand_schedule, "green"))]], by=firmID]

    max_profit[, "cp_green":= replace(cp_green, (cp_green<=mp_grey) & (sequence<.N), NA), by=firmID]

    # revenue given output allocation between markets
    max_profit[!is.na(cp_green), "revenue":=
        dt_l[(firmID==.BY)][order(opEx_pMCF),
            c(gas_MCF+csgd_MCF) %*% sapply(1:length(leaseIDs), function(x)
                                        ifelse((opEx_pMCF>cutoff[x]) & (opEx_pMCF<=cp_green[x]),
                                                cp_green[x], ifelse(opEx_pMCF<=mp_grey, mp_grey, 0)))], by=firmID]

    return(max_profit[!is.na(revenue), .SD[which.max(revenue)], by=firmID])

}###--------------------     END OF FUNCTION market_allocation      --------------------###


clear_gas_markets <- function(dt_f, dt_l, dt_m, demand_schedule, ti) {
    # consider all developed producing leases
    dt_l[, "market":= ifelse((class=="developed") & (status=="producing") & (gas_MCF>0 | csgd_MCF>0),
                        replace(market, is.na(market) | market=="none", "grey"), NA)]
    dt_l[dt_f[behavior=="flaring"], on="firmID", "market":= replace(market, !is.na(market), "grey")]

    if (ti$market_prop_green>0) {
        # market conditions under previous market allocations
        mp0_grey <- market_price(dt_l, demand_schedule, "grey")
        dt_l[(market=="grey") & (opEx_pMCF > mp0_grey), "market":= "none"]

        # developing firms determine how to allocate production between green and grey markets
        optimal_allocation <- dt_f[(behavior!="flaring") & (activity=="development"),
                                    market_allocation(dt_l[!is.na(market)], demand_schedule, firmID, mp0_grey)]

        # determine whether current allocation is above or below the target setpoint
        optimal_allocation[dt_l[, max(c(0, opEx_pMCF[market=="grey"])), by=firmID], on="firmID", "error":= V1 - cutoff]

        # take one step towards the target setpoint
        ## move lowest opEx lease out of green market or
        dt_l[optimal_allocation[error < 0], on="firmID",
                "market":= replace(market, which.max(rank(-opEx_pMCF) * (market=="green")), "grey"), by=.EACHI]
        ## move highest opEx lease into green market
        dt_l[optimal_allocation[error > 0], on="firmID",
                "market":= replace(market, which.max(rank(opEx_pMCF) * (market!="green")), "green"), by=.EACHI]
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


demand_sample <- function(prop_green, sample_set, p_low, p_high) {
    while (NROW(sample_set) < 1) {
        sample_set <- historical_market[runif(.N) < (0.5/.N), .(year, month)]
    }
    cat("Generating demand function from:", paste(sample_set[[2]], sample_set[[1]], collapse=", "), "\n")

    # price elasticity of demand
    ep_grey  <- -0.18

    p0 <- historical_market[sample_set, mean(p)]
    q0 <- historical_market[sample_set, mean(q)]
    # the historic qauntity demanded is
    #    the fraction of gas produced in Texas (q_TX)
    #    the fraction of Texas gas produced by "associated" operators (~95%)
    #    the fraction of those operators' gas production actually assigned to firms (frac)
    q0p <- historical_market[sample_set, mean(q_TX * 0.95 * frac)]

    # slope of the inverse demand function given ep
    m <- (p0 / q0) * (1 / ep_grey)

    # price (y) and quantity (x) intercept of grey demand curve
    b <- p0 - (m * q0p)
    q_int_grey <- -(b / m) * (1-prop_green)

    # green demand represents a rotation of the demand curve about the quantity intercept
    #    [Sedjo & Swallow 2002](zotero://select/items/0_GGH3Y8UX)
    # green electricity consumers pay a premium of [7-30%](./inputs/market_history.html)
    premium <- runif(1, p_low, p_high) * (((m * q0p) + b) / (prop_green * (m * q0p) + b))
    b_green <- premium * b

    # shift green demand curve to account for max market size
    q_int_green <- max(-(b / m) * prop_green, 1e-10)

    return(function(q) {
                p_grey=  nafill(approx(c(0, q_int_grey),  c(b,        0), q)$y, fill=0)
                p_green= nafill(approx(c(0, q_int_green), c(b_green,  0), q)$y, fill=0)

                return(data.table(q, "p_grey"= if (prop_green==1) 0 else p_grey,
                                    "p_green"= if (prop_green==0) 0 else p_green))
    })
}###--------------------      END OF FUNCTION demand_sample         --------------------###



#*
#*** UTILITIES ***#
#*
recover_outputs <- function(refID) {
    # recreate CSV outputs from database file

    file_paths <- c(sprintf("%s/CSR-ABM/outputs/processed/all_states_%s.sqlite",
                                Sys.getenv("WORK"),  refID),
                    sprintf("./logs/param_log_%s-%%s.csv", refID),
                    sprintf("./outputs/%1$s_states_%2$s-%%s.csv",
                        c("agent", "lease", "market"), refID),
                    sprintf("./outputs/demand_function_%s-%%s.rds", refID))

    # check if all files exist or need to be recovered from DB
    inputs_exists <- lapply(file_paths[2:5], function(x)
                list.files(path=dirname(x), gsub("%s", ".*", basename(x))))
    if (all(lengths(inputs_exists)>0)) {
        print("Input files found")
        return()
    }

    # exit with error if db does not exist
    if (!file.exists(file_paths[1])) {
        print("No input files found")
        q()
    }

    # setup DB connection
    db <- DBI::dbConnect(RSQLite::SQLite(), file_paths[1])

    lookup_col <- function(db, s_key, i_key) {
        data_sql <- "SELECT string_key FROM string_lookup
                    WHERE column_name='%s' AND integer_key=%s"
        return(DBI::dbGetQuery(db, sprintf(data_sql, s_key, i_key)))
    }


    # MODEL PARAMETERS
    ref_params <- DBI::dbGetQuery(db, "SELECT * FROM params WHERE model=1")
    setDT(ref_params)
    ref_params[, "RunID_dummy":= RunID]
    ref_params[, fwrite(.SD, sprintf(file_paths[2], .BY)), by=RunID_dummy]


    # FIRMS
    data_sql <- "SELECT * FROM %2$s 
                LEFT JOIN %1$s ON %1$s.firmID=%2$s.firmID 
                        AND %1$s.RunID=%2$s.RunID 
                        AND %1$s.model=%2$s.model 
                    WHERE %2$s.model=1 
                        AND %2$s.time IN (%3$d, %3$d-1, (SELECT MIN(t0) FROM params WHERE model=1)-1)"
    firms <- DBI::dbGetQuery(db, sprintf(data_sql,
                                        "agent_info",
                                        "agent_states",
                                        0))
    setDT(firms)

    # format columns
    firms[, which(duplicated(names(firms))) := NULL]
    firms[, c("model", "gas_flared_calc", "oil_prod", "gas_prod"):= NULL]
    for (col in c("behavior", "activity")) {
        firms[, c(col):= as.character(get(col))]
        firms[, c(col):= as.character(lookup_col(db, col, .BY)),
            by=get(col)]
    }
    rm(data_sql)
    firms[, "RunID_dummy":= RunID]
    firms[, fwrite(.SD, sprintf(file_paths[3], .BY)), by=RunID_dummy]


    # LEASES
    ## include changed leases needed to rebuild initial portfolio_options
    data_sql <- "SELECT * FROM (
                    SELECT *, 
                        ROW_NUMBER() OVER 
                            (PARTITION BY RunID, leaseID ORDER BY time DESC) 
                            AS row_num
                    FROM %2$s
                    WHERE model=1 AND time<=%3$s) AS st
                LEFT JOIN %1$s ON %1$s.leaseID=st.leaseID 
                        AND %1$s.RunID=st.RunID 
                        AND %1$s.model=st.model 
                    WHERE st.row_num<=2
                        OR st.time < (SELECT MIN(t0) FROM params WHERE model=1)"

    leases <- DBI::dbGetQuery(db, sprintf(data_sql, "lease_info",
                                                    "lease_states",
                                                    0))
    setDT(leases)

    # format columns
    leases[, which(duplicated(names(leases))) := NULL]
    leases[, c("model","dw","net_dw","t_last","row_num"):= NULL]
    for (col in c("area", "market", "status", "class")) {
        leases[, c(col):= as.character(get(col))]
        leases[, c(col):= as.character(lookup_col(db, col, .BY)),
                by=get(col)]
    }
    leases[class!="developed", "t_switch":= NA_integer_]
    leases[t_found>time, c("t_found","firmID"):= NA_integer_]
    leases[(class!="developed") |
            (status!="producing") |
            (gas_MCF==0 & csgd_MCF==0), "market":= NA_character_]
    rm(data_sql)

    setorder(leases, RunID, leaseID, time)

    leases[, "RunID_dummy":= RunID]
    leases[, fwrite(.SD, sprintf(file_paths[4], .BY)), by=RunID_dummy]


    # MARKET HISTORY
    ## recover market states from DB
    market_history <- DBI::dbGetQuery(db, "SELECT * FROM market_states WHERE model=1")
    setDT(market_history)
    market_history[, c("model", "q_stored", "frac"):= NULL]

    market_history[, "RunID_dummy":= RunID]
    market_history[, fwrite(.SD, sprintf(file_paths[5], .BY)), by=RunID_dummy]


    # DEMAND FUNCTION
    market_shares <- fread("./inputs/processed/firm_market_shares.csv")

    historical_market_data <- fread("./inputs/processed/historical_NG_demand.csv",
                                integer64="numeric")

    for (i in leases[, unique(RunID)]) {
        historical_market_data[
            market_shares[, sum(OPER_GAS_PROD_VOL+OPER_CSGD_PROD_VOL),
                    by=.("year"=CYCLE_YEAR, "month"=month.abb[CYCLE_MONTH])],
                on=c("year","month"),
            "frac":= leases[(time==min(time)) & !is.na(firmID) & (RunID==i),
                        6*sum(BOE_emp - oil_BBL - cond_BBL)] / V1]

        # recreate a demand schedule
        demand <-
            setRefClass("demand_function",
                fields =  list(historical_market= "data.table"),
                methods = list(new_schedule= demand_sample)
            )$new(historical_market=
                na.omit(historical_market_data[, .SD, keyby=.(year, month)]))

        # set default function arguments
        assign("prices",
            c(ref_params[RunID==i, .(p_low, p_high)][1]),
            envir=demand)
        with(environment(demand$new_schedule),
            formals(new_schedule) <- c(alist(prop_green=, sample_set=list()), prices))
        rm("prices", envir=demand)

        saveRDS(demand, sprintf(file_paths[6], i))
        historical_market_data[, "frac":= NULL]
        rm(demand)
    }


    # CLEAN UP
    # terminate connection and optimize future queries
    DBI::dbExecute(db, "PRAGMA analysis_limit=1000;")
    DBI::dbExecute(db, "PRAGMA optimize;")
    DBI::dbDisconnect(db)

}###--------------------     END OF FUNCTION recover_outputs        --------------------###
