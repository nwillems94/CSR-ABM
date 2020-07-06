source("flaringABM_core.R")


Params <<- list(
    "t0" = -5,
    "tf" = 20,
    # Environmental Variables
    "Activism" = 200,
    "SRoR" = 0.8,
    #Market conditions
    "market_price_dirty" = 10,
    "market_price_green" = 10 * 1.05
)

for (Run in 1) {
    print(Run)
    # Initialize agents, save their initial state
    source("flaringABM_init.R")
    Params$market_size <- sum(firms[,"units"])
    Params$green_size_rate <- with(Params, market_size / (1 + nrow(firms)/5) / (tf-t0))

    firms[,"RunID"] <- Run
    if (Run==1) {
        write.csv(Params, file="logs/param_log.csv", row.names=FALSE)
        write.csv(cbind(firms, "time"=Params$t0-1), file="outputs/agent_states.csv", row.names=FALSE)
    }

    # step through time
    for (t in Params$t0:Params$tf) {
        cat(t,", ")
        # calculate the social pressure on each firm (begins at t=0)
        if (t>0) {
            firms <- dist_social_pressure(firms)
        }
        # run the markets and update firm capital
        firms[,"capital"] <- firms[,"capital"] + calc_revenue(firms, t) - calc_cost(firms, t)
        firms[,"market_value"] <- calc_market_value(firms, Params$SRoR, t)

        #optimize market value
        firms <- optimize_strategy(firms, Params$SRoR, t)

        #output states
        write.table(cbind(firms,"time"=t), "outputs/agent_states.csv", sep = ",",
                    col.names = FALSE, row.names=FALSE, append = TRUE)
    }
    cat("\n")
}
    


# analytics
library(ggplot2)

agent_states <- read.csv("outputs/agent_states.csv")
agent_states$mitigation <- as.factor(agent_states$mitigation)
#summary(agent_states)
#nrow(agent_states)


mitigators <- aggregate(id~RunID+time+mitigation, data=agent_states, length, drop=FALSE)
mitigators <- transform(mitigators, "num"=replace(id, is.na(id), 0), RunID=as.factor(RunID))
progress <- ggplot(subset(mitigators, mitigation==1), aes(x=time, y=num, color=RunID)) +
                geom_step(alpha=0.4)
print(progress)