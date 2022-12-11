args <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(rmarkdown)

jobIDs <- lapply(strsplit(args, "="), `[[`, 2)
names(jobIDs) <- gsub("\\n", "\n", sapply(strsplit(args, "="), `[[`, 1), fixed=TRUE)
print(jobIDs)

params <- fread(sprintf("./logs/params_%s.csv.gz", paste(jobIDs, collapse="-")))
market_history <- fread(sprintf("./outputs/processed/market_states_%s.csv.gz", paste(jobIDs, collapse="-")))
agent_states <- fread(sprintf("./outputs/processed/agent_states_%s.csv.gz", paste(jobIDs, collapse="-")))
db <- dbConnect(RSQLite::SQLite(), sprintf("./outputs/processed/all_states_%s.sqlite", paste(jobIDs, collapse="-")))

## Market value
if (agent_states[is.na(market_value) & (time>min(time)), .N] > 0) {
    print("Missing market values")
    print(agent_states[is.na(market_value) & (time>min(time))])
} else {
    print("Market values")
    print(agent_states[!is.na(market_value), summary(market_value)])
}
print("Percent of agents who lose money over model run")
print(agent_states[time==max(time), as.list(summary(.SD[, sum(profit<0) / .N, by=RunID]$V1)), by=model])
print(agent_states[time==max(time), as.list(summary(.SD[, sum(profit<0) / .N, by=RunID]$V1)), by=.(model,behavior)])

print("Percent of oil and gas production from bankrupt agents")
print(agent_states[time==max(time), as.list(summary(.SD[, sum(oil_output[profit<0]) / sum(oil_output), by=RunID]$V1)), by=model])
print(agent_states[time==max(time), as.list(summary(.SD[, sum(gas_output[profit<0]) / sum(gas_output), by=RunID]$V1)), by=model])


## Check for flarers who are selling green gas
if (agent_states[behavior=="flaring" & green_gas_sold!=0, .N] > 0) {
    print("Agents who are flaring are still participating in the green market")
    print(agent_states[behavior=="flaring" & green_gas_sold!=0])
}

## Agents mitigating before social pressure starts
if (agent_states[!(behavior %in% c("flaring", "economizing", "imitating")) & time<0, .N>0]) {
    print("Agents who are not economizing are mitigating before social pressure starts")
    print(agent_states[!(behavior %in% c("flaring", "economizing", "imitating")) & time<0])
}

## Model and calculated flaring intensity that dont match
if (agent_states[abs(gas_flared-gas_flared_calc) > 0.01, .N>0]) {
    print("Model gas flared does not match calculated value")
    print(agent_states[gas_flared!=gas_flared_calc, as.list(summary(gas_flared/gas_flared_calc)), by=model])
    print(agent_states[gas_flared!=gas_flared_calc])
}

## Green price below grey price
if (market_history[p_green>0][p_grey > p_green, .N>0]) {
    print("Grey price is higher than green price")
    print(market_history[p_green>0][p_grey > p_green])
}


rmarkdown::render("flaringABM_validation.Rmd", output_format="html_document",
                output_file=sprintf("%s/outputs/validation/%s.html",
                        ifelse(Sys.getenv("WORK")=="", ".", paste0(Sys.getenv("WORK"),"/flaringABM")),
                        paste(jobIDs, collapse="-")),
                intermediates_dir=sprintf("./outputs/validation/%s", paste(jobIDs, collapse="-")), quiet=TRUE)

# optimize later queries
dbExecute(db, "PRAGMA analysis_limit=1000;")
dbExecute(db, "PRAGMA optimize;")
dbDisconnect(db)
