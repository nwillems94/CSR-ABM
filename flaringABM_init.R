library(data.table)

### ASSIGN LEASE ATTRIBUTES ###
leases <- data.table("leaseID"=1:Params$nleases, key="leaseID",
                    "firmID"=NA_integer_, "class"=NA_character_, "status"=NA_character_)

## RESOURCE
leases[, "oil_BBL":= sample.int(40, size=.N, replace=TRUE) + 10]
leases[, "gas_MCF":= sample.int(400, size=.N, replace=TRUE) + 10]

## COSTS
# standard fixed cost without mitigation
leases[, "baseline_fCost":= pmax(rnorm(.N, mean=5000, sd=1000), 1000)]

# standard operating cost without mitigation
leases[, "baseline_oCost":= pmin(pmax(rnorm(.N, mean=500, sd=20), 100), oil_BBL*Params$oil_price)]

# addtional fixed cost of mitigation
leases[, "green_fCost":= pmax(rnorm(.N, mean=3000, sd=1000), 1000)]

# addtional operating cost with mitigation
leases[, "green_add_oCost":= pmax(rnorm(.N, mean=400, sd=200), 100)]

# time at which lease switches from underdeveloped to developed
leases[, "t_switch"] <- NA_integer_

### ASSIGN AGENT ATTRIBUTES ###
# initialize firms
firms <- data.table("firmID"= 1:Params$nagents, key= "firmID",
                    # how much oil (BBL) and gas (MCF) does the firm produce each time step
                    "oil_output"= NA_real_, "oil_revenue"= NA_real_,
                    "gas_output"= NA_real_, "green_gas_output"= NA_real_, "gas_revenue"= NA_real_,
                    # Valuations; costs include Operating, Mitigation, Capital Expenditures
                    "cash"= NA_real_, "market_value"= NA_real_,
                    "cost_O"= NA_real_, "cost_M"= NA_real_, "cost_CE"= NA_real_, "sPressure"= NA_real_,
                    # time horizons for decision making and over which investments are paid off
                    "t_horizon"= NA_real_, "i_horizon"= NA_real_,
                    # activities: exploration, development; behaviors: flaring, mitigating, economizing, imitating
                    "activity"= NA_character_, "behavior"= NA_character_,
                    "time"= NA_real_)

## TIME SCALES
firms[, "t_horizon":= 5] #time horizon for decision making
firms[, "i_horizon":= 4] #time horizon over which investments are paid off

# randomly assign firms to leases
leases[sample(.N, 4*nrow(firms), replace=FALSE), "firmID":= sample(nrow(firms), 4*nrow(firms), replace=TRUE)]
#  redistribute so every firm has at least 1 lease
lease_count <- sapply(firms$firmID, function(x) leases[firmID==x, .N])
while (min(lease_count)==0) {
    leases[firmID==which.max(lease_count), "firmID"][1] <- which.min(lease_count)
    lease_count <- sapply(firms$firmID, function(x) leases[firmID==x, .N])
}
# assign lease classes and status
# class:    undeveloped- discovered only
#           underdeveloped- lease with flaring
#           developed- oil only or lease with gas capture
# status:   producing
#           stopped
leases[!is.na(firmID), c("class", "status"):= .(ifelse(gas_MCF>0, "underdeveloped", "developed"), "producing")]

# assume firms have had assets long enough that all baseline fixed costs are paid off
leases[!is.na(firmID), "t_found":= Params$t0 - max(firms$i_horizon) - 1]
firms[, "cost_CE":= 0]

firms[leases[!is.na(firmID), .(sum(oil_BBL), sum(gas_MCF * ifelse(class=="developed",1,0))), by=firmID], on="firmID",
        c("oil_output", "oil_revenue", "gas_output"):= .(V1, V1 * Params$oil_price, V2)]

### DETERMINE INTIAL MARKET CONDITIONS ###
firms[, "time":= Params$t0-1]
leases[, "time":= Params$t0-1]

firms[leases[, sum(baseline_oCost), by=firmID], on="firmID", "cost_O":= V1]
# start with no firms capturing gas
firms[, c("cost_M", "green_gas_output"):= 0]
firms[, "gas_revenue":= gas_output * Params$market_price_dirty]

# assume firms have enough cash to cover their baseline operating costs
firms[, "cash":= 2 * (cost_O + cost_M) + sample.int(40, size=.N, replace=TRUE)]
firms[, c("sales","profit"):= 0]

# initially there is no social pressure, and no firms are mitigating
firms[, "behavior":= "flaring"]
firms[, "sPressure":= 0]
firms[, "market_value":= (oil_revenue - cost_O)]
