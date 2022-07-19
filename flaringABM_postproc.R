args <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(DBI)
require(RSQLite)

write_outputs <- function(db, csv, ID, append_file) {
    print(paste("Compiling model:", names(ID)))

    market_states <- fread(sprintf("./outputs/market_states_%s.csv", ID))
    lease_states <- fread(sprintf("./outputs/lease_states_%s.csv", ID))
    agent_states <- fread(sprintf("./outputs/agent_states_%s.csv", ID))

    market_states[, "model":= names(ID)]
    lease_states[, "model":= names(ID)]
    agent_states[, "model":= names(ID)]

    agent_states[
        lease_states[(status=="producing"), sum(csgd_MCF[class=="underdeveloped"]) + sum(sopf_MCF),
            by=.(model, RunID, time, firmID)],
        on=c("model", "RunID", "time", "firmID"), "gas_flared_calc":= V1]
    agent_states[!is.na(time) & is.na(gas_flared_calc), "gas_flared_calc":= 0]

    # write outputs
    fwrite(market_states, sprintf("./outputs/processed/market_states_%s.csv.gz", csv), append=append_file)
    dbWriteTable(all_states, "market_states", market_states, append=append_file)
    fwrite(agent_states, sprintf("./outputs/processed/agent_states_%s.csv.gz", csv), append=append_file)
    dbWriteTable(all_states, "agent_states", agent_states, append=append_file)
    dbWriteTable(all_states, "lease_states", lease_states, append=append_file)
}


jobIDs <- lapply(strsplit(args, "="), `[[`, 2)
names(jobIDs) <- gsub("\\n", "\n", sapply(strsplit(args, "="), `[[`, 1), fixed=TRUE)
print(jobIDs)

if("refID" %in% names(jobIDs)) {
    file_name <- gsub("^all_states_(.*).sqlite$", "\\1",
                    list.files(path="./outputs/processed/", pattern=sprintf("^all_states_%s-.*.sqlite$", jobIDs$refID)))
} else {
    file_name <- paste(jobIDs, collapse="-")
    jobIDs <- c(jobIDs, "refID"=NA)
}
if(length(file_name)>1) {
    print("Error with database name")
    q()
}

all_states <- dbConnect(RSQLite::SQLite(), sprintf("./outputs/processed/all_states_%s.sqlite", file_name))

lapply(seq(jobIDs)[-which(names(jobIDs)=="refID")], function(i) write_outputs(all_states, file_name, jobIDs[i], i!=1))

dbDisconnect(all_states)

print("Finished assembling databases")
