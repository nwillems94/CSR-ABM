library(data.table)

### ASSIGN WELL ATTRIBUTES ###
wells <- data.table("wellID"=1:Params$nwells, "firmID"=NA_integer_, "class"=NA_character_, "status"=NA_character_)

## RESOURCE
wells[,"oil_BBL":= sample.int(40, size=.N, replace=TRUE) + 10]
wells[,"gas_MCF":= sample.int(40, size=.N, replace=TRUE) + 10]

## COSTS
# standard operating cost without mitigation
wells[,"baseline_oCost":= pmax(rnorm(.N, mean=50, sd=2), 10)]

# addtional fixed cost of mitigation
wells[,"green_fCost":= pmax(rnorm(.N, mean=500, sd=100), 100)]

# addtional operating cost with mitigation
wells[,"green_add_oCost":= pmax(rnorm(.N, mean=15, sd=5), 1)]



### ASSIGN AGENT ATTRIBUTES ###
# initialize firms, none of whom are under social pressure or mitigating
firms <- data.table("firmID"=1:Params$nagents, "mitigation"=0, "sPressure"=0, "cash"=0, "market_value"=NA_real_)

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
wells[!is.na(firmID), c("class","status"):= .(ifelse(gas_MCF>0, "underdeveloped", "developed"), "producing")]

# Attributes from Hartley 2013: zotero://select/items/1_IKQGEEBK
# oil & gas reserves as a proxy for     upstream capital
firms[, c("oil_reserves", "gas_reserves"):= 1]

# refining capacity as a proxy for      downstream capital
firms[,"ref_capacity":= sample.int(40, size=.N, replace=TRUE) + 10]

firms[,"gas_output":= wells[!is.na(firmID), .(sum(gas_MCF)), by=.(firmID)][order(firmID),"V1"]]

## COSTS
# standard operating cost without mitigation
firms[,"baseline_oCost":= wells[!is.na(firmID), .(sum(baseline_oCost)), by=.(firmID)][order(firmID),"V1"]]

# addtional fixed cost of mitigation
firms[,"green_fCost":= wells[!is.na(firmID), .(sum(green_fCost)), by=.(firmID)][order(firmID),"V1"]]

# addtional operating cost with mitigation
firms[,"green_add_oCost":= wells[!is.na(firmID), .(sum(green_add_oCost)), by=.(firmID)][order(firmID),"V1"]]

firms[,"oCost":= baseline_oCost]

## TIME SCALES
firms[,"t_horizon":= 5] #time horizon for decision making
firms[,"i_horizon":= 4] #time horizon over which investments are paid off

firms[,"t_switch"] <- NA_integer_ #time at which agent switches into the green market
firms[,"do_e"] <- NA #are agents conducting exploration in this time step?
