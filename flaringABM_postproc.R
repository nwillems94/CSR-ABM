args <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(DBI)
require(RSQLite)

jobIDs <- lapply(strsplit(args, "="), `[[`, 2)
names(jobIDs) <- gsub("\\n", "\n", sapply(strsplit(args, "="), `[[`, 1), fixed=TRUE)
print(jobIDs)


all_states <- dbConnect(RSQLite::SQLite(),
                sprintf("./outputs/processed/all_states_%s.sqlite", paste(jobIDs, collapse="-")))


print("Compiling lease states")
lease_states <- rbindlist(lapply(jobIDs, function(x) fread(sprintf("./outputs/lease_states_%s.csv", x))), idcol="model")

dbWriteTable(all_states, "lease_states", lease_states)


print("Compiling agent states")
agent_states <- rbindlist(lapply(jobIDs, function(x) fread(sprintf("./outputs/agent_states_%s.csv", x))), idcol="model")

# calculate gas flared by each firm per time step
agent_states[
    lease_states[status=="producing" & class=="underdeveloped", sum(csgd_MCF), by=.(model, RunID, time, firmID)],
        on=c("model", "RunID", "time", "firmID"), "gas_flared":= V1]
agent_states[!is.na(time) & is.na(gas_flared), "gas_flared":= 0]

rm(lease_states)

fwrite(agent_states, sprintf("./outputs/processed/agent_states_%s.csv.gz", paste(jobIDs, collapse="-")))
dbWriteTable(all_states, "agent_states", agent_states)

rm(agent_states)


dbDisconnect(all_states)

print("Finished assembling databases")
