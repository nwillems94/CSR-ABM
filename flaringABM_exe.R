args <- commandArgs(trailingOnly=TRUE)
args <- args[substr(args, 1, 2)!="--"]


## Load libraries
library(data.table)
library(Rcpp)
library(future)
library(future.apply)
plan("multisession")


## Stage run
source("./flaringABM_main.R")

jobID <- format(Sys.time(), "%m%d%H%M")
logOuts <- sprintf("./logs/param_log_%s-%%s.csv", jobID)
agentOuts <- sprintf("./outputs/agent_states_%s-%%s.csv", jobID)
leaseOuts <- sprintf("./outputs/lease_states_%s-%%s.csv", jobID)

cat("jobID:", jobID, "\n")

## Set model parameters and interpret command line arguments
Params <<- list(
    "refID" = NA,
    # TxRRC data shows about 2000 firms producing gas alongside oil operating in any given year since 2010
    "nagents" = 2000,
    "t0" = -25,
    "tf" = 100,
    # Environmental Variables
    "Activism" = 1e6,
    # Market conditions
    "SRoR" = 0.5,   # social rate of return:
                    #     0: no social satisfaction from holding shares or contributing to the activist
                    #     1: shareholding & activist contributions are perfect substitutes for personal giving
    "threshold" = 0.5, # max units of gas "green" firms can flare per unit of oil produced
    "market_price_dirty" = 2,
    "market_price_green" = 2*1.16, # from Kitzmueller & Shimshack 16[5,20]% zotero://select/items/0_PGHV5RK7
    "oil_price" = 60,
    # Activities
    "prop_e" = 11/12, # what proportion of firms engage in exploration activities in a given time step
    "prob_m" = 1  # probability that a follower will mimic a leader if they observe them mitigating
)
Params$SRoR <- c(rep(0, -Params$t0), rep(Params$SRoR, Params$tf + 1))
# from OShaughnessy et al. 3% of electricity sales green zotero://select/items/0_HW2MXA38
Params$market_prop_green <- with(Params, c(rep(0, -t0),
                                        seq(from=0, to=1.5, length.out= 1 + (tf %/% 2)),
                                        rep(1.5, tf - (tf %/% 2)))) * 0.12

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

rm(args, update_arg)

## Start model runs given parameters
run_time <- Sys.time()
future_lapply(1:20, function(Run) {
    source("./flaringABM_core.R")
    flaringABM_main(Params, jobID, Run)
})
cat("\n", gsub("Time difference of", "All runs complete in", capture.output(Sys.time() - run_time)), "\n")
print(warnings())

