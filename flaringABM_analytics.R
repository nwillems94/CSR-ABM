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


capital <- ggplot(agent_states, aes(x=time, y=capital, fill=mitigation)) +
            geom_bar(stat="summary", fun.y="mean", position="dodge")
print(capital)

ggplot(agent_states, aes(x=time, y=capital, color=is.na(t_switch))) +
            geom_line()

ggplot(agent_states, aes(x=time, y=market_value, color=is.na(t_switch))) +
            geom_line()


distr <- with(subset(agent_states, time==max(agent_states$time)),
                cbind(mitigation, stack(data.frame(dCost,fCost,oCost))))

ggplot(distr, aes(x=ind, y=values, fill=mitigation)) +
    geom_bar(stat="summary", fun.y="mean", position="dodge")

# dCost <- ggplot(agent_states, aes(x=time, y=dCost, color=mitigation)) +
#             geom_bar(stat="summary", fun.y="mean", position="dodge")


# oCost <- ggplot(agent_states, aes(x=time, y=oCost, color=mitigation)) +
#             geom_bar(stat="summary", fun.y="mean", position="dodge")

# fCost <- ggplot(agent_states, aes(x=time, y=fCost, color=mitigation)) +
#             geom_bar(stat="summary", fun.y="mean", position="dodge")

# print(grid.arrange(dCost, oCost, fCost, ncol=1))