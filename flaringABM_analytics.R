library(ggplot2)
library(data.table)

# read in data
jobID <- "02121202"
pdf(sprintf("graphics/plots_%s.pdf", jobID))
agentOuts <- sprintf("outputs/agent_states_%s.csv", jobID)
wellOuts <- sprintf("outputs/well_states_%s.csv", jobID)

agent_states <- fread(agentOuts)
well_states <- fread(wellOuts)
well_states[, RunID:= as.factor(RunID)]

agent_states[, "mitigator":= any(behavior=="mitigating"), by=.(RunID,firmID)]
agent_states[, RunID:= as.factor(RunID)]

# calculate gas flared by each firm per time step
agent_states[
    well_states[status=="producing" & class=="underdeveloped", sum(gas_MCF), by=.(RunID, time, firmID)],
        on=c("RunID", "time", "firmID"), "gas_flared":= V1]
agent_states[!is.na(time) & is.na(gas_flared), "gas_flared":= 0]

# Number of mitigators over time
ggplot(agent_states[behavior!="flaring", .N, keyby=.(RunID, time, behavior)][
            CJ(RunID, time, behavior, unique=TRUE), .(RunID, time, behavior, "N"=replace(N, is.na(N), 0))]) +
        geom_step(aes(x=time, y=N, color=RunID)) +
        facet_grid(behavior~., scales="free_y") +
        labs(x="Time", y="# of firms in green market")

#Attributes of mitigators and non mitigators
ggplot(agent_states, aes(x=time, y=market_value, fill=behavior!="flaring")) +
    geom_bar(stat="summary", fun="mean", position="dodge") +
    labs(fill="Mitigator", x="Time", y="Market Value")


## does optimizing market value lead to optimizing flaring intensity?
ggplot(agent_states[time>0], aes(x=market_value, y=gas_flared/oil_output)) +
    geom_point(aes(color=mitigator), alpha=0.2, shape=1) +
    #geom_point(aes(shape=mitigation, color=time)) +
    labs(x="Market Value", y="Flaring Intensity")
    #labs(x="Market Value / Profit", y="Flaring Intensity")

# density of firms
ggplot(agent_states[time>0],#[, "gas_flared":= gas_flared + runif(.N)/100],
        aes(x=market_value, y=gas_flared/oil_output, color=behavior!="flaring")) +
    stat_density_2d(geom="point", shape=1, aes(size=after_stat(ndensity)), n=50, contour = FALSE) +
    scale_radius(range = c(-1, 6), guide=NULL) +
    coord_cartesian(xlim=c(-500, 2500), ylim = c(0, 20)) +
    labs(title="Density of firms", x="Market Value", y="Flaring Intensity", color="Mitigating") +
    theme(legend.position="bottom")

ggplot(agent_states[time>0], aes(x=market_value, y=gas_flared/oil_output)) +
    geom_density_2d_filled(show.legend=FALSE) +
    #lims(x=c(-1000,2000), y=c(0,15)) +
    #coord_cartesian(xlim=c(-1000,2000), ylim=c(0,15)) +
    coord_cartesian(xlim=c(-600,2100), ylim=c(0,15), expand=FALSE) +
    annotate("segment", x=-200, xend=2100, y=0.5, yend=0.5, color="white", linetype=2) +
    #annotate("text", x=-700, y=0.5, label="Green market threshold", color="white") +
    annotate("text", x=-400, y=0.5, label="Green market\nthreshold", color="white") +
    labs(title="Density of all firms", x="Market Value", y="Flaring Intensity")

# density of firms not flaring
ggplot(agent_states[time>0 & behavior!="flaring"], aes(x=market_value)) +
        geom_density(aes(color=behavior)) +
        labs(title="Density of non flaring firms", x="Market Value", color="Behavior:") +
        theme(legend.position=c(0.8,0.8), legend.background=element_rect(fill="lightgrey",color="black"))


# does increasing demand for green product decrease gas flared
ggplot(agent_states, aes(x=time, y=gas_flared)) +
    geom_line(stat="summary", fun="sum", aes(group=RunID), alpha=0.5) +
    geom_line(stat="summary", fun="sum", color="green", aes(y=gas_flared/length(unique(RunID)))) +
    labs(x="Time", y="Total Gas Flared (MCF)")

ggplot(agent_states[, .(sum(green_gas_output), sum(gas_flared)/sum(oil_output)), by=.(RunID, time)]) +
    geom_smooth(aes(x=V1, y=V2)) +
    labs(x="Green Market Size (MCF)", y="Total Flaring Intensity")

dev.off()