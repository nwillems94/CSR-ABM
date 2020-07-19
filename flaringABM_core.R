# THIS SCRIPT CONTAINS ALL THE RUN-TIME FUNCTIONALITY OF THE flaringABM
#
#
#
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
    Ai <- rep(0L, nrow(agents))

    if (method=="even") {
        Ai[agents[,"mitigation"]!=1] <- A / sum(agents[,"mitigation"]!=1)
    } else if (method=="focused") {
        focus <- intersect(focus, which(agents[,"mitigation"]!=1))
        Ai[focus] <- A/length(focus)
    } else if (method=="capacity") {
        Ai[agents[,"mitigation"]!=1] <- A * with(subset(agents, mitigation!=1), capacity / sum(capacity))
    }
    agents[,"sPressure"] <- Ai

    return(agents)
}###--------------------    END OF FUNCTION dist_social_pressure    --------------------###


#*
#*** FIRM VALUATION ***#
#*
calc_market_value <- function(agents, SRoR, time) {
    #determines if the possible harm of social pressure outweighs the cost of mitigating the externality

    cost        <- calc_costC(agents, time, NA)                              #baseline cost
    add_cost    <- calc_costC(agents, time) - calc_costC(agents, time, NA)   #additional cost of mitigation
    revenue     <- calc_revenueC(agents, time)                               #revenue

    # market_value = profit + dprofit - Ai/SRoR - cost*xi + cost*xi*SRoR
    # (profit in this formulation does not capture the additional cost of mitigating the externality [cost*xi])
    market_value <- (revenue - cost - add_cost) +                   #Net cash flow
                    (add_cost*SRoR - (agents[,"sPressure"] / SRoR)) #Net social value

    return(market_value)
}###--------------------    END OF FUNCTION calc_market_value       --------------------###


optimize_strategy <- function(agents, SRoR, time) {
    #determines if the possible harm of social pressure outweighs the cost of mitigating the externality

    #consider the additional cost starting mitigation at "time" over "t_horizon"
    cost <- sapply(1:nrow(agents), function(z)
                    sum(sapply(time + 1:agents[z,"t_horizon"], function(y)
                                calc_costC(agents[z,], y, time) - calc_costC(agents[z,], y, NA))))
    #offset a portion of the cost with possible additional revenue
    #revenue if this agent starts mitigating - current revenues
    revenue <- sapply(1:nrow(agents), function(z) calc_revenueC(within(agents, mitigation[z] <- 1), time)[z]) -
                    calc_revenueC(agents, time)
    cost <- (cost * (1-SRoR)) - (revenue * agents[,"t_horizon"])

    #consider the possible harm from social pressure over "t_horizon"
    harm <- (agents[,"sPressure"] / SRoR) * agents[,"t_horizon"]

    #if the cost outweighs the possible harm and the agent has sufficient capital, they start mitigating
    new_mitigators <- (cost < harm) & check_affordabilityC(agents) & (agents[,"mitigation"]==0)
    agents[new_mitigators, "mitigation"] <- 1
    agents[new_mitigators, "t_switch"] <- time + 1

    return(agents)
}###--------------------    END OF FUNCTION optimize_strategy       --------------------###
