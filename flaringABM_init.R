library(data.table)

### ASSIGN WELL ATTRIBUTES ###
wells <- data.table("wellID"=1:Params$nwells, key="wellID",
                    "firmID"=NA_integer_, "class"=NA_character_, "status"=NA_character_)

## RESOURCE
wells[, "oil_BBL":= sample.int(40, size=.N, replace=TRUE) + 10]
wells[, "gas_MCF":= sample.int(40, size=.N, replace=TRUE) + 10]

## COSTS
# standard fixed cost without mitigation
wells[, "baseline_fCost":= pmax(rnorm(.N, mean=5000, sd=1000), 1000)]

# standard operating cost without mitigation
wells[, "baseline_oCost":= pmin(pmax(rnorm(.N, mean=500, sd=20), 100), oil_BBL*Params$oil_price)]

# addtional fixed cost of mitigation
wells[, "green_fCost":= pmax(rnorm(.N, mean=3000, sd=1000), 1000)]

# addtional operating cost with mitigation
wells[, "green_add_oCost":= pmax(rnorm(.N, mean=400, sd=200), 100)]

# time at which well switches from underdeveloped to developed
wells[, "t_switch"] <- NA_integer_

### ASSIGN AGENT ATTRIBUTES ###
# initialize firms, none of whom are under social pressure or mitigating
firms <- data.table("firmID"=1:Params$nagents, key="firmID",
                    "mitigation"=0, "economizer"=NA, "sPressure"=0,
                    "capital"=NA_real_, "market_value"=NA_real_)

## TIME SCALES
firms[, "t_horizon":= 5] #time horizon for decision making
firms[, "i_horizon":= 4] #time horizon over which investments are paid off

firms[, "do_e"] <- NA #are agents conducting exploration in this time step?

# randomly assign firms to wells
wells[sample(.N, 4*nrow(firms), replace=FALSE), "firmID":= sample(nrow(firms), 4*nrow(firms), replace=TRUE)]
#  redistribute so every firm has at least 1 well
well_count <- sapply(firms$firmID, function(x) wells[firmID==x, .N])
while (min(well_count)==0) {
    wells[firmID==which.max(well_count), "firmID"][1] <- which.min(well_count)
    well_count <- sapply(firms$firmID, function(x) wells[firmID==x, .N])
}
# assign well classes and status
# class:    undeveloped- discovered only
#           underdeveloped- well with flaring
#           developed- oil only or well with gas capture
# status:   producing
#           stopped
wells[!is.na(firmID), c("class", "status"):= .(ifelse(gas_MCF>0, "underdeveloped", "developed"), "producing")]

# Attributes from Hartley 2013: zotero://select/items/1_IKQGEEBK
# oil & gas reserves as a proxy for     upstream capital
firms[, c("oil_reserves", "gas_reserves"):= 1]

# refining capacity as a proxy for      downstream capital
firms[, "ref_capacity":= sample.int(40, size=.N, replace=TRUE) + 10]

firms[, c("gas_output", "oil_output"):= wells[!is.na(firmID), .(sum(gas_MCF * ifelse(class=="developed",1,0)),
                                                                sum(oil_BBL)), keyby=.(firmID)][,-"firmID"]]
firms[, c("green_gas_output", "gas_revenue"):= .(0, NA_real_)]

## COSTS
firms[firms[wells, on="firmID"][, sum(baseline_oCost), by=firmID], on="firmID", "cost":= V1]
firms[, "add_cost":= 0]

# assume firms have had assets long enough that all baseline fixed costs are paid off
wells[!is.na(firmID), "t_found":= Params$t0 - max(firms$i_horizon) - 1]

### DETERMINE INTIAL MARKET CONDITIONS ###
# assume firms have enough cash to cover their baseline operating costs
firms[, "cash":= 2*cost]
firms[, "capital":= calc_capital_equivC(firms)]
# initially there is no social pressure, and no firms are mitigating
firms[, "market_value":= ((oil_output * Params$oil_price) - cost)]

portfolio_permutations <- build_permutations(firms$firmID)

industry_revenue <- with(Params, list("prices"= list("dirty"= market_price_dirty),
                                    "green_coeff"= (market_price_green - market_price_dirty) * market_prop_green[1]))
options_changed <- c()
