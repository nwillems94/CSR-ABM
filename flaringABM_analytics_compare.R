library(ggplot2)
library(data.table)

jobIDs <- c("02121202", # complete
            #"02121250", # no social variables (SRoR,Activism,=0)
            "03091220", # no shareholder valuation
            "02121223", # no product differentiation (market_prop_green = 0)
            "02121242", # no stakeholder activism (A=0)
            "02121318", # no peer effects (prob_m =0)
            "02121338", # lower threshold
            "02121612", # activist targeting according to gas output
            "02121644") # activist targeting according to oil output
agentOuts <- sapply(jobIDs, function(x) sprintf("outputs/agent_states_%s.csv", x))
wellOuts  <- sapply(jobIDs, function(x) sprintf("outputs/well_states_%s.csv", x))

#pdf(sprintf("graphics/plots_%s.pdf", jobID))

## Compare model components
agent_states <- rbind(fread(agentOuts[1])[,"model":= "complete"],
                    #fread(agentOuts[2])[,"model":= "no social factors"],
                    fread(agentOuts[2])[,"model":= "no shareholder\nvaluation"],
                    fread(agentOuts[3])[,"model":= "no differentiation"],
                    fread(agentOuts[4])[,"model":= "no stakeholder\nactivism"],
                    fread(agentOuts[5])[,"model":= "no imitation"])
well_states <- rbind(fread(wellOuts[1])[,"model":= "complete"],
                    #fread(wellOuts[2])[,"model":= "no social factors"],
                    fread(wellOuts[2])[,"model":= "no shareholder\nvaluation"],
                    fread(wellOuts[3])[,"model":= "no differentiation"],
                    fread(wellOuts[4])[,"model":= "no stakeholder\nactivism"],
                    fread(wellOuts[5])[,"model":= "no imitation"])

well_states[, c("RunID","model"):= .(as.factor(RunID), as.factor(model))]
agent_states[, c("RunID","model"):= .(as.factor(RunID), as.factor(model))]

# calculate gas flared by each firm per time step
agent_states[
    well_states[status=="producing" & class=="underdeveloped", sum(gas_MCF), by=.(model, RunID, time, firmID)],
        on=c("model", "RunID", "time", "firmID"), "gas_flared":= V1]
agent_states[!is.na(time) & is.na(gas_flared), "gas_flared":= 0]



main <- ggplot(agent_states[, sum(gas_flared) / sum(oil_output), by=.(model, RunID, time)]) +
    geom_smooth(aes(x=time, y=V1, color=model)) +
    labs(title="Relative impact of model components", x="Time", y="Total Flaring Intensity", color="") +
    theme(legend.position="bottom")

# main

## Compare green market thresholds
agent_states <- rbind(fread(agentOuts[1])[,"threshold":= "0.5"],
                    fread(agentOuts[6])[,"threshold":= "0.1"])
well_states <- rbind(fread(wellOuts[1])[,"threshold":= "0.5"],
                    fread(wellOuts[6])[,"threshold":= "0.1"])

well_states[, c("RunID","threshold"):= .(as.factor(RunID), as.factor(threshold))]
agent_states[, c("RunID","threshold"):= .(as.factor(RunID), as.factor(threshold))]

# calculate gas flared by each firm per time step
agent_states[
    well_states[status=="producing" & class=="underdeveloped", sum(gas_MCF), by=.(threshold, RunID, time, firmID)],
        on=c("threshold", "RunID", "time", "firmID"), "gas_flared":= V1]
agent_states[!is.na(time) & is.na(gas_flared), "gas_flared":= 0]


inset <- ggplot(agent_states[, sum(gas_flared) / sum(oil_output), by=.(threshold, RunID, time)]) +
    geom_smooth(aes(x=time, y=V1, group=threshold)) +
    labs(x="Time", y="Total Flaring Intensity") +
    annotate("label", x=40, y=5.1, label="Threshold=0.1") +
    annotate("label", x=-5, y=4.4, label="Threshold=0.5") +
    theme(axis.title=element_blank())

# plot with inset
main +
    annotation_custom(grob = ggplotGrob(inset), xmin = 24, xmax = 64, ymin = 5.35, ymax = 6.39)
    annotation_custom(grob = ggplotGrob(inset), xmin = 24, xmax = 64, ymin = 5.4, ymax = 6.44)

## Compare activist strategies
agent_states <- rbind(fread(agentOuts[1])[,"targeting":= "uniform"],
                    fread(agentOuts[7])[,"targeting":= "gas output"],
                    fread(agentOuts[8])[,"targeting":= "oil output"])
well_states <- rbind(fread(wellOuts[1])[,"targeting":= "uniform"],
                    fread(wellOuts[7])[,"targeting":= "gas output"],
                    fread(wellOuts[8])[,"targeting":= "oil output"])

well_states[, c("RunID","targeting"):= .(as.factor(RunID), as.factor(targeting))]
agent_states[, c("RunID","targeting"):= .(as.factor(RunID), as.factor(targeting))]

# calculate gas flared by each firm per time step
agent_states[
    well_states[status=="producing" & class=="underdeveloped", sum(gas_MCF), by=.(targeting, RunID, time, firmID)],
        on=c("targeting", "RunID", "time", "firmID"), "gas_flared":= V1]
agent_states[!is.na(time) & is.na(gas_flared), "gas_flared":= 0]

ggplot(agent_states[, sum(gas_flared) / sum(oil_output), by=.(targeting, RunID, time)]) +
    geom_smooth(aes(x=time, y=V1, group=targeting)) +
    labs(x="Time", y="Total Flaring Intensity", title="Flaring intensity under various activist strategies") +
    annotate("label", x=40, y=4.5, label="Uniform") +
    annotate("label", x=30, y=4.25, label="Oil output") +
    annotate("label", x=20, y=4.0, label="Gas output")


dev.off()