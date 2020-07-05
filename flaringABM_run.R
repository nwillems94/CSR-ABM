source("code/flaringABM_core.R")


Params <<- list(
    "t0" = -5,
    "tf" = 20,
    # Environmental Variables
    "Activism" = 200,
    "SRoR" = 0.8,
    #Market conditions
    "market_size_dirty" = 1000,
    "market_size_green" = 1000 * 0.5,
    "market_price_dirty" = 10,
    "market_price_green" = 10 * 1.05
)

# Initialize agents, save their initial state
source("code/flaringABM_init.R")
write.csv(Params, file="code/logs/param_log.csv", row.names=FALSE)
write.csv(cbind(firms, "time"=Params$t0-1), file="code/outputs/agent_states.csv", row.names=FALSE)

# step through time
for (t in Params$t0:Params$tf) {
    cat(t,", ")
    # calculate the social pressure on each firm (begins at t=0)
    if (t>0) {
        firms <- distribute_pressure(firms)
    }
    # run the markets and update firm capital
    firms[,"capital"] <- firms[,"capital"] + calc_revenue(firms) - calc_cost(firms, t)
    firms[,"market_value"] <- calc_market_value(firms, Params$SRoR, t)

    #optimize market value
    firms <- optimize_strategy(firms, Params$SRoR, t)

    #output states
    write.table(cbind(firms,"time"=t), "code/outputs/agent_states.csv", sep = ",",
                col.names = FALSE, row.names=FALSE, append = T)
}


# analytics
library(ggplot2)

agent_states <- read.csv("code/outputs/agent_states.csv")
agent_states$mitigation <- as.factor(agent_states$mitigation)
#summary(agent_states)
#nrow(agent_states)


mitigators <- aggregate(id~time+mitigation, data=agent_states, length, drop=FALSE)
mitigators <- transform(mitigators, "num"=replace(id, is.na(id), 0))
progress <- ggplot(subset(mitigators, mitigation==1), aes(x=time, y=num)) +
                geom_line()
print(progress)