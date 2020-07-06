# THIS SCRIPT CONTAINS ALL THE RUN-TIME FUNCTIONALITY OF THE flaringABM
#
#
#

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
    Ai <- rep(0L, nrow(agents))

    if (method=="even") {
        Ai[agents[,"mitigation"]!=1] <- A / sum(agents[,"mitigation"]!=1)
    } else if (method=="focused") {
       Ai[focus] <- A/length(focus)
    }
    agents[,"sPressure"] <- Ai

    return(agents)
}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###



#*
#*** MARKETS ***#
#*
calc_market_quantity <- function(time) {
    #determine the number of agents in each of the green/dirty markets
    #assume a constant size
    #qg <- qg * time / Params$tf / 100
    qg <- Params$green_size_rate * time
    qd <- Params$market_size - qg

    return(list("dirty"=qd, "green"=qg))
}###--------------------    END OF FUNCTION calc_market_quantity    --------------------###

dist_market_quantity <- function(agents, total_units) {
    distribution <- data.frame("nunits"=0,"max_units"=agents[,"units"])
    excess <- total_units

    while (excess>0 & any(with(distribution, nunits < max_units))) {
        distribution[,"nunits"] <- with(distribution,
                                        nunits + (excess / sum(nunits < max_units) * (nunits < max_units)))
        distribution[,"nunits"] <- pmin(distribution[,"nunits"], distribution[,"max_units"])

        excess <- total_units - sum(distribution[,"nunits"])
    }

    return(distribution[,"nunits"])
}###--------------------    END OF FUNCTION dist_market_quantity    --------------------###

calc_market_price <- function(pd, pg) {
    #determine the prices in the green/dirty markets
    #assume a constant price with a premium in the green market

    return(list("dirty"=pd, "green"=pg))
}###--------------------    END OF FUNCTION calc_market_price       --------------------###



#*
#*** FIRM VALUATION ***#
#*
calc_cost <- function(agents, time, switch_time) {
    #Determine the cost at "time", assuming the firm transitions to the green market at "switch_time"
    if (missing(switch_time)) {
        switch_time <- agents[,"t_switch"]
    }
    #assume the fixed cost is paid off equally over i_horizon years
    cost <- with(agents, ifelse(!is.na(switch_time) & (time - switch_time < i_horizon),
                                green_fCost/i_horizon, 0))
    cost <- cost + agents[,"oCost"] + ifelse(is.na(switch_time), 0, agents[,"green_add_oCost"])

    # more code/logic needed to consider mitigation on [0,1]

    return(cost)
}###--------------------    END OF FUNCTION calc_cost               --------------------###

calc_revenue <- function(agents, time) {
    #determine the revenues for firms in each market
    #assume all firms sell the same quantity
    prices      <- calc_market_price(Params$market_price_dirty, Params$market_price_green)
    total_units <- calc_market_quantity(time)

    green_units <- dist_market_quantity(within(agents, units[mitigation==0] <- 0),  total_units$green)
    if (sum(agents$units < green_units) > 0) {
        print("WARNING")
    }
    dirty_units <- dist_market_quantity(transform(agents, units=units-green_units), total_units$dirty)

    revenues <- prices$green * green_units + prices$dirty * dirty_units

    return(revenues)
}###--------------------    END OF FUNCTION calc_revenue            --------------------###

calc_market_value <- function(agents, SRoR, time) {
    #determines if the possible harm of social pressure outweighs the cost of mitigating the externality

    cost        <- calc_cost(agents, time, NA)                              #baseline cost
    add_cost    <- calc_cost(agents, time) - calc_cost(agents, time, NA)    #additional cost of mitigation
    revenue     <- calc_revenue(agents, time)                                     #revenue

    # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
    # (profit in this formulation does not capture the additional cost of mitigating the externality [cost*xi])
    market_value <- (revenue - cost - add_cost) +                   #Net cash flow
                    (add_cost*SRoR - (agents[,"sPressure"] / SRoR)) #Net social value

    return(market_value)
}###--------------------    END OF FUNCTION calc_market_value       --------------------###

check_affordability <- function(buyers) {
    #check if each agent can afford the fixed cost of mitigation
    #naively assume there is no borrowing available and the cost is paid as a lump sum
    afford <- buyers[,"green_fCost"] < buyers[,"capital"]

    return(afford)
}###--------------------    END OF FUNCTION check_affordability     --------------------###

optimize_strategy <- function(agents, SRoR, time) {
    #determines if the possible harm of social pressure outweighs the cost of mitigating the externality

    #consider the additional cost starting mitigation at "time" over "t_horizon"
    cost <- sapply(1:nrow(agents), function(z)
                    sum(sapply(time + 1:agents[z,"t_horizon"], function(y)
                                calc_cost(agents[z,], y, time) - calc_cost(agents[z,], y, NA))))
    #offset a portion of the cost with possible additional revenue
    #revenue if this agent starts mitigating - current revenues
    revenue <- sapply(1:nrow(agents), function(z) calc_revenue(within(agents, mitigation[z] <- 1), time)[z]) -
                    calc_revenue(agents, time)
    cost <- (cost * (1-SRoR)) - (revenue * agents[,"t_horizon"])

    #consider the possible harm from social pressure over "t_horizon"
    harm <- (agents[,"sPressure"] / SRoR) * agents[,"t_horizon"]

    #if the cost outweighs the possible harm and the agent has sufficient capital, they start mitigating
    new_mitigators <- (cost < harm) & check_affordability(agents) & (agents[,"mitigation"]==0)
    agents[new_mitigators, "mitigation"] <- 1
    agents[new_mitigators, "t_switch"] <- time + 1

    return(agents)
}###--------------------    END OF FUNCTION optimize_strategy       --------------------###
