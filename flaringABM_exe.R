args <- commandArgs(trailingOnly=TRUE)
args <- args[substr(args, 1, 2) != "--"]
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
    "refID"= NA,
    "nruns"= NA_real_,
    # TxRRC data shows about 2000 firms producing gas alongside oil
    #    operating in any given year since 2010
    "nagents"= 2000,
    "t0"= -60,
    "tf"= 60,
    ## Environmental Variables ##
    "Activism"= 5.5e5,
    "strategy"= "oil_output",
    ## Market conditions ##
    "SRoR"= 0.5,   # social rate of return:
                    #     0: no social satisfaction from holding shares
                    #     1: shareholding is a perfect substitutes for activist contributions
    # max units of gas "green" firms can flare per unit of oil produced
    "threshold"= 0.05,
    # New York City residential gas consumption is about ~0.75% of national
    "market_prop_green"= 0.0075,
    # green electricity consumers pay a premium of [7-30%](./inputs/market_history.html)
    "p_low"= 1.07, "p_high"= 1.3,
    ## Activities ##
    # what proportion of firms engage in exploration activities in a given time step
    "prop_e"= 11 / 12,
    # probability that a follower will mimic a leader if they observe them mitigating
    "prob_m"= 0.25,
    # are reported empirical flared volumes accurate
    "reporting"= "accurate"
)

# update parameters from command-line arguments
for (update_arg in strsplit(args, "=")) {
    print("Updating model parameters:")

    if (is.null(Params[[update_arg[1]]])) {
        print(sprintf("Unknown parameter: %s     ... exiting", update_arg[1]))
        q()
    }

    print(update_arg)
    if (is.numeric(Params[[update_arg[1]]])) {
        Params[[update_arg[1]]] <- as.numeric(update_arg[2])
    } else {
        Params[update_arg[1]] <- update_arg[2]
    }
}

# social influences begin at time 0
Params$Activism <- c(rep(0, -Params$t0), rep(Params$Activism, Params$tf + 1))
Params$SRoR <- c(rep(0, -Params$t0), rep(Params$SRoR, Params$tf + 1))
Params$prob_m <- c(rep(0, -Params$t0), rep(Params$prob_m, Params$tf + 1))

Params$market_prop_green <- with(Params,
    c(rep(0, -t0),
        seq(from=0, to=1, length.out=1 + (tf %/% 2)),
        rep(1, tf - (tf %/% 2))) * market_prop_green)

# Oil price multiplier based on historical distribution
daily_prices <- fread("./inputs/processed/daily_prices.csv")
Params$oil_price_mean <- daily_prices[year > 2010, mean(log(ratio_BBLpMCF))]
Params$oil_price_sd   <- daily_prices[year > 2010,   sd(log(ratio_BBLpMCF))]

rm(args, update_arg, daily_prices)

## Start model runs given parameters
run_time <- Sys.time()
future_lapply(1:Params$nruns, function(Run) {
    source("./flaringABM_core.R")
    # setup logging
    sink(sprintf("./logs/run_log_%s-%s.txt", jobID, Run))
    on.exit(sink())
    flaringABM_main(Params, jobID, Run)
})
cat("\n",
    gsub("Time difference of", "All runs complete in",
        capture.output(Sys.time() - run_time)),
    "\n")
print(warnings())

