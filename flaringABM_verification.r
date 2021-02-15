library(ggplot2)
library(data.table)

# read in data
jobID <- "02121202"
pdf(sprintf("graphics/verification_%s.pdf", jobID))
logOuts <- sprintf("logs/param_log_%s.csv", jobID)
agentOuts <- sprintf("outputs/agent_states_%s.csv", jobID)
wellOuts <- sprintf("outputs/well_states_%s.csv", jobID)

params <- fread(logOuts)
agent_states <- fread(agentOuts)
well_states <- fread(wellOuts)
well_states[, RunID:= as.factor(RunID)]
agent_states[, RunID:= as.factor(RunID)]

## Check for flarers who are selling green gas
if (agent_states[behavior=="flaring" & green_gas_output!=0, .N] > 0) {
    print("Agents who are flaring are still participating in the green market")
}

## Agents mitigating before social pressure starts
if (agent_states[behavior!="flaring" & behavior!="economizing" & behavior!="imitating" & time<0,
                    .(min(time), max(diff(time))), by=.(RunID, firmID)][V1>params[16]$t0 | V2>1, .N]) {
    print("Agents who are not economizing are mitigating before social pressure starts")
}

## Green gas output matches specification
green_market_size <- params[, .("RunID"=.I, .SD * as.double(market_size)), .SDcols=grep("market_prop_green", names(params))]
green_market_size <- melt(green_market_size, id.vars="RunID")[,
                        "time":= as.numeric(gsub("market_prop_green","",variable)) + params$t0 - 1]
green_market_size[, c("RunID","green_gas_output","value","variable"):= .(as.factor(RunID), value, NULL, NULL)]
setorder(green_market_size, RunID, time)

ggplot(agent_states, aes(x=time, y=green_gas_output)) +
    geom_line(stat="summary", fun="sum", alpha=0.7) +
    geom_line(data=green_market_size, linetype = "dashed") +
    facet_wrap(RunID~.)
