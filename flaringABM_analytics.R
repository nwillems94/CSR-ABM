library(ggplot2)

agent_states <- read.csv("outputs/agent_states-var.csv")

agent_states <- merge(agent_states,
                    with(subset(agent_states, time==max(time)), cbind(RunID, id, "switcher"=!is.na(t_switch))),
                    by=c("RunID","id"))

#agent_states$mitigation <- as.factor(agent_states$mitigation)
agent_states$switcher <- as.factor(agent_states$switcher)
agent_states$RunID <- as.factor(agent_states$RunID)


#summary(agent_states)
#nrow(agent_states)

# Number of mitigators over time
ggplot(agent_states, aes(x=time, color=RunID)) +
    geom_step(aes(y=mitigation), stat="summary", fun.y="sum")


#Attributes of mitigators and non mitigators
ggplot(agent_states, aes(x=time, y=capital, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")
ggplot(agent_states, aes(x=time, y=market_value, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")

ggplot(agent_states, aes(x=RunID, y=market_value, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")

ggplot(agent_states, aes(x=RunID, y=baseline_oCost, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")
ggplot(agent_states, aes(x=RunID, y=green_add_oCost, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")
ggplot(agent_states, aes(x=RunID, y=green_fCost, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")

ggplot(agent_states, aes(x=RunID, y=units, fill=switcher)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")



costs <- with(aggregate(cbind(baseline_oCost, green_add_oCost, green_fCost) ~ RunID+switcher, data=agent_states, mean),
                cbind(RunID, switcher, stack(data.frame(baseline_oCost, green_add_oCost, green_fCost))))
ggplot(costs, aes(x=RunID, y=values, fill=switcher)) +
    geom_bar(stat="identity", position="dodge") +
    facet_wrap(~ind, ncol=1, scale="free_y")
