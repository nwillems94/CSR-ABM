args <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)
library(rmarkdown)

jobIDs <- lapply(strsplit(args, "="), `[[`, 2)
names(jobIDs) <- gsub("\\n", "\n", sapply(strsplit(args, "="), `[[`, 1), fixed=TRUE)
print(jobIDs)

agent_states <- fread(sprintf("./outputs/processed/agent_states_%s.csv.gz", paste(jobIDs, collapse="-")))
db <- dbConnect(RSQLite::SQLite(), sprintf("./outputs/processed/all_states_%s.sqlite", paste(jobIDs, collapse="-")))

## Check for flarers who are selling green gas
if (agent_states[behavior=="flaring" & green_gas_output!=0, .N] > 0) {
    print("Agents who are flaring are still participating in the green market")
    print(agent_states[behavior=="flaring" & green_gas_output!=0])
}

## Agents mitigating before social pressure starts
if (agent_states[!(behavior %in% c("flaring", "economizing", "imitating")) & time<0, .N>0]) {
    print("Agents who are not economizing are mitigating before social pressure starts")
    print(agent_states[!(behavior %in% c("flaring", "economizing", "imitating")) & time<0])
}

## Model and calculated flaring intensity that dont match
if (agent_states[abs(gas_flared-gas_flared_calc) > 0.01, .N>0]) {
    print("Model gas flared does not match calculated value")
    print(agent_states[gas_flared!=gas_flared_calc])
}


rmarkdown::render("flaringABM_validation.Rmd", output_format="html_document",
                output_file=sprintf("%s/outputs/validation/%s.html",
                        ifelse(Sys.getenv("WORK")=="", ".", paste0(Sys.getenv("WORK"),"/flaringABM")),
                        paste(jobIDs, collapse="-")),
                intermediates_dir=sprintf("./outputs/validation/%s", paste(jobIDs, collapse="-")), quiet=TRUE)
