### ASSIGN WELL ATTRIBUTES ###
generate_wells <- function() {
    wells <- data.frame("wellID"=1:400)

    ## RESOURCE
    wells[,"oil_BBL"] <- sample.int(40, size=nrow(wells), replace=TRUE) + 10
    wells[,"gas_MCF"] <- sample.int(40, size=nrow(wells), replace=TRUE) + 10

    ## COSTS

    # standard operating cost without mitigation
    wells[,"baseline_oCost"] <- pmax(rnorm(nrow(wells), mean=50, sd=2), 10)

    # addtional fixed cost of mitigation
    wells[,"green_fCost"] <- pmax(rnorm(nrow(wells), mean=500, sd=100), 100)

    # addtional operating cost with mitigation
    wells[,"green_add_oCost"] <- pmax(rnorm(nrow(wells), mean=15, sd=5), 1)

    return(wells)
}



### ASSIGN AGENT ATTRIBUTES ###
generate_firms <- function(wells) {
    # initialize firms, none of whom are under social pressure or mitigating
    firms <- data.frame("firmID"=1:100, "mitigation"=0, "sPressure"=0, "cash"=0, "market_value"=NA)

    # randomly assign firms to wells
    well2firm <- sample(nrow(firms), nrow(wells), replace=TRUE)
    firms[,"wellID"] <- sapply(firms[,"firmID"], function(x)
                                paste(
                                    which(well2firm==x),
                                    collapse = ";"))
    # redistribute so every firm has at least 1 well
    while (min(sapply(firms["wellID"], nchar))==0) {
        max_wells <- which.max(sapply(firms["wellID"], function(x) lengths(gregexpr(";", x))))
        firms[which.min(sapply(firms["wellID"], nchar)), "wellID"] <- sub(";.*", "", firms[max_wells, "wellID"])
        firms[max_wells, "wellID"] <- sub(".*?;", "", firms[max_wells, "wellID"])
    }

    length(strsplit(paste(firms[,"wellID"], collapse = ";"), ";")[[1]])
    length(unique(strsplit(paste(firms[,"wellID"], collapse = ";"), ";")[[1]]))

    # Attributes from Hartley 2013: zotero://select/items/1_IKQGEEBK
    # oil & gas reserves as a proxy for     upstream capital 
    firms[,"oil_reserves"] <- 1
    firms[,"gas_reserves"] <- 1

    # refining capacity as a proxy for      downstream capital
    firms[,"ref_capacity"] <- sample.int(40, size=nrow(firms), replace=TRUE) + 10

    firms[,"gas_output"] <- sapply(strsplit(firms[,"wellID"], ";"), function(x) sum(subset(wells, wellID %in% x, gas_MCF)))
    if (sum(firms[,"gas_output"]) != sum(wells[,"gas_MCF"])) {
        warning("well gas ouputs do not match firms")
    }


    ## COSTS
    # standard operating cost without mitigation
    firms[,"baseline_oCost"] <-
        sapply(strsplit(firms[,"wellID"], ";"), function(x) sum(subset(wells, wellID %in% x, baseline_oCost)))

    # addtional fixed cost of mitigation
    firms[,"green_fCost"] <-
        sapply(strsplit(firms[,"wellID"], ";"), function(x) sum(subset(wells, wellID %in% x, green_fCost)))

    # addtional operating cost with mitigation
    firms[,"green_add_oCost"] <-
        sapply(strsplit(firms[,"wellID"], ";"), function(x) sum(subset(wells, wellID %in% x, green_add_oCost)))

    firms[,"oCost"] <- firms[,"baseline_oCost"]

    ## TIME SCALES
    firms[,"t_horizon"] <- 5 #time horizon for decision making
    firms[,"i_horizon"] <- 4 #time horizon over which investments are paid off

    firms[,"t_switch"] <- NA #time at which agent switches into the green market

    return(firms)
}
