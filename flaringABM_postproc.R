args <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(DBI)
require(RSQLite)

write_outputs <- function(db, csv, ID, append_file) {
    print(paste("Compiling model:", names(ID)))

    params_wide <- fread(sprintf("./logs/param_log_%s.csv", ID), colClasses="character")
    params_wide[, "RunID":= .I]
    params <- melt(params_wide, measure.vars=patterns("market_prop_green","prob_m","SRoR", "Activism"),
                variable="time", value.name=c("market_prop_green","prob_m","SRoR", "Activism"), variable.factor=FALSE)
    params[, names(params[, -c("refID","strategy")]):= lapply(.SD[, -c("refID","strategy")], as.numeric)]
    params[, "time":= time - 1 + t0]
    params[refID=="", "refID":= NA]

    market_states <- fread(sprintf("./outputs/market_states_%s.csv", ID))
    lease_states <- fread(sprintf("./outputs/lease_states_%s.csv", ID))
    agent_states <- fread(sprintf("./outputs/agent_states_%s.csv", ID))

    params[, "model":= names(ID)]
    market_states[, "model":= names(ID)]
    lease_states[, "model":= names(ID)]
    agent_states[, "model":= names(ID)]

    agent_states[
        lease_states[(status=="producing"), sum(csgd_MCF[class=="underdeveloped"]) + sum(sopf_MCF),
            by=.(model, RunID, time, firmID)],
        on=c("model", "RunID", "time", "firmID"), "gas_flared_calc":= V1]
    agent_states[!is.na(time) & is.na(gas_flared_calc), "gas_flared_calc":= 0]

    # write outputs
    fwrite(params, sprintf("./logs/params_%s.csv.gz", csv), append=append_file)
    fwrite(market_states, sprintf("./outputs/processed/market_states_%s.csv.gz", csv), append=append_file)
    fwrite(agent_states, sprintf("./outputs/processed/agent_states_%s.csv.gz", csv), append=append_file)

    dbBegin(db) # begin SQLite transaction and turn off autocommit
    for (dt in c("params", "market_states", "agent_states", "lease_states")) {
        if (!append_file) {
            dbCreateTable(db, dt, get(dt))
        }
        wrdt <- dbSendStatement(db, paste0("INSERT INTO ", dt, " VALUES(:", paste(names(get(dt)), collapse=", :"), ")"))
        dbBind(wrdt, get(dt))
        dbClearResult(wrdt)
        rm(dt)
    }
    dbCommit(db)
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
# turn off safety measures since all data is backed up in CSVs
dbExecute(all_states, "PRAGMA journal_mode = OFF;")
dbExecute(all_states, "PRAGMA synchronous = OFF;")

lapply(seq(jobIDs)[-which(names(jobIDs)=="refID")], function(i) write_outputs(all_states, file_name, jobIDs[i], i!=1))

# optimize later queries
dbExecute(all_states, "PRAGMA analysis_limit=1000;")
dbExecute(all_states, "PRAGMA optimize;")
dbDisconnect(all_states)

print("Finished assembling databases")
