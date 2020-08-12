# Assign agent attributes

# initialize firms, none of whom are under social pressure or mitigating
firms <- data.frame("id"=1:100, "mitigation"=0, "sPressure"=0, "cash"=0, "market_value"=NA)

# Attributes from Hartley 2013: zotero://select/items/1_IKQGEEBK
# oil & gas reserves as a proxy for     upstream capital 
firms[,"oil_reserves"] <- 1
firms[,"gas_reserves"] <- 1

# refining capacity as a proxy for      downstream capital
firms[,"ref_capacity"] <- sample.int(40, size=nrow(firms), replace=TRUE) + 10


firms[,"gas_output"] <- sample.int(40, size=nrow(firms), replace=TRUE) + 10

## COSTS
# standard operating cost without mitigation
firms[,"baseline_oCost"] <- pmax(rnorm(nrow(firms), mean=50, sd=2), 10)

# addtional fixed cost of mitigation
firms[,"green_fCost"] <- pmax(rnorm(nrow(firms), mean=500, sd=100), 100)

# addtional operating cost with mitigation
firms[,"green_add_oCost"] <- pmax(rnorm(nrow(firms), mean=15, sd=5), 1)

firms[,"oCost"] <- firms[,"baseline_oCost"]

## TIME SCALES
firms[,"t_horizon"] <- 5 #time horizon for decision making
firms[,"i_horizon"] <- 4 #time horizon over which investments are paid off

firms[,"t_switch"] <- NA #time at which agent switches into the green market
