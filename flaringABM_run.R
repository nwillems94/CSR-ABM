library(data.table)

source("flaringABM_core.R")
source("flaringABM_init.R")


Params <<- list(
    "t0" = -5,
    "tf" = 20,
    # Environmental Variables
    "Activism" = 200,
    "SRoR" = 0.8,
    # Market conditions
    "market_price_dirty" = 10,
    "market_price_green" = 10 * 1.16, # from Kitzmueller & Shimshack [5,16,20]% zotero://select/items/0_PGHV5RK7
    "oil_price" = 20,
    "capital_assets" = "upstream"
)

for (Run in 1:20) {
    cat(Run,":  ")
    # Initialize agents, save their initial state
    wells <- generate_wells(1000)
    firms <- generate_firms(100, wells)
    Params$market_size <- sum(firms$gas_output)
    Params$green_size_rate <- with(Params, market_size / (1 + nrow(firms)/5) / (tf-t0))

    firms[,"RunID":=Run]
    if (Run==1) {
        fwrite(Params, file="logs/param_log.csv")
        fwrite(firms[, "time":=NA_integer_], file="outputs/agent_states.csv")
    }

    # step through time
    for (t in Params$t0:Params$tf) {
        cat(t,", ")
        # calculate the social pressure on each firm (begins at t=0)
        if (t>0) {
            dist_social_pressure(firms)
        }

        # run the markets and update firm capital
        firms[, "cash":= cash + calc_revenueC(firms, t) - calc_costC(firms, t)]
        firms[, "market_value":= calc_market_value(firms, Params$SRoR, t)]

        # randomly assign either exploration or development activities
        firms[,"do_e":= runif(nrow(firms)) > 0.5]
        # 10% chance a firm finds a new well
        done_e <- sort(firms[do_e==TRUE & runif(.N)>0.9]$firmID)
        wells[sample(which(is.na(firmID)), length(done_e)), "firmID":= done_e]
        # update cost figures based on new acquisition
        firms[firmID %in% done_e, grep("Cost", names(wells), value=TRUE):=
            wells[firmID %in% done_e, lapply(.SD, sum), keyby=firmID, .SDcols=grep("Cost",names(wells))][,-"firmID"]]
        firms[firmID %in% done_e, "oCost":= baseline_oCost]

        #optimize market value
        firms[firmID %in% firms[do_e==FALSE][optimize_strategy(firms[do_e==FALSE], Params$SRoR, t)]$firmID,
                c("mitigation","t_switch"):= .(1, t + 1)]

        #output states
        fwrite(firms[, "time":=t], file="outputs/agent_states.csv", append=TRUE)
    }
    cat("\n")
}
    


# analytics
library(ggplot2)

agent_states <- fread("outputs/agent_states.csv")
agent_states$RunID <- as.factor(agent_states$RunID)

progress <- ggplot(agent_states, aes(x=time, color=RunID)) +
                geom_step(aes(y=mitigation), stat="summary", fun="sum")
print(progress)