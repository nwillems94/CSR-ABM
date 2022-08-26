args <- commandArgs(trailingOnly=TRUE)
args <- args[substr(args, 1, 2)!="--"]
options(warn=1) # elevate all warnings to errors


## Load libraries
library(data.table)
library(future)
library(future.apply)
plan("multisession")

## Stage run
source("./flaringABM_main.R")

jobID <- format(Sys.time(), "%m%d%H%M")
logOuts <- sprintf("./logs/param_log_%s-%%s.csv", jobID)
agentOuts <- sprintf("./outputs/agent_states_%s-%%s.csv", jobID)
leaseOuts <- sprintf("./outputs/lease_states_%s-%%s.csv", jobID)
marketOuts <- sprintf("./outputs/market_states_%s-%%s.csv", jobID)

cat("jobID:", jobID, "\n")

## Set model parameters and interpret command line arguments
Params <<- list(
    "refID" = NA,
    # TxRRC data shows about 2000 firms producing gas alongside oil operating in any given year since 2010
    "nagents" = 2000,
    "t0" = -60,
    "tf" = 60,
    # Environmental Variables
    "Activism" = 3e5,
    # Market conditions
    "SRoR" = 0.1,   # social rate of return:
                    #     0: no social satisfaction from holding shares
                    #     1: shareholding is a perfect substitutes for activist contributions
    "threshold" = 0.05, # max units of gas "green" firms can flare per unit of oil produced
    # Activities
    "prop_e" = 11/12, # what proportion of firms engage in exploration activities in a given time step
    "prob_m" = 1      # probability that a follower will mimic a leader if they observe them mitigating
)
Params$Activism <- c(rep(0, -Params$t0), rep(Params$Activism, Params$tf + 1))
Params$SRoR <- c(rep(0, -Params$t0), rep(Params$SRoR, Params$tf + 1))
Params$prob_m <- c(rep(0, -Params$t0), rep(Params$prob_m, Params$tf + 1))
# from OShaughnessy et al. 3% of electricity sales green zotero://select/items/0_HW2MXA38
Params$market_prop_green <- with(Params, c(rep(0, -t0),
                                        seq(from=0, to=1.5, length.out= 1 + (tf %/% 2)),
                                        rep(1.5, tf - (tf %/% 2)))) * 0.12

# Oil price multiplier based on historical distribution
daily_prices <- fread("./inputs/processed/daily_prices.csv")
Params$oil_price_mean <- daily_prices[year>2010, mean(log(ratio_BBLpMCF))]
Params$oil_price_sd   <- daily_prices[year>2010,   sd(log(ratio_BBLpMCF))]

# update parameters from command-line arguments
for (update_arg in strsplit(args, "=")) {
    print("Updating model parameters:")

    if (is.null(Params[[update_arg[1]]])) {
        print(sprintf("Unknown parameter: %s     ... exiting", update_arg[1]))
        q()
    }

    print(update_arg)
    if (is.numeric(Params[[update_arg[1]]])) {
        Params[[update_arg[1]]] <- ifelse(Params[[update_arg[1]]]==0, 0, as.numeric(update_arg[2]))
    } else {
        Params[update_arg[1]] <- update_arg[2]
    }
}

rm(args, update_arg, daily_prices)

## Start model runs given parameters
run_time <- Sys.time()
future_lapply(1:32, function(Run) {
    source("./flaringABM_core.R")
    # setup logging
    sink(sprintf("./logs/run_log_%s-%s.txt", jobID, Run))
    flaringABM_main(Params, jobID, Run)
})
cat("\n", gsub("Time difference of", "All runs complete in", capture.output(Sys.time() - run_time)), "\n")
print(warnings())

