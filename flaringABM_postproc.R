args <- commandArgs(trailingOnly=TRUE)
args <- args[substr(args, 1, 2)!="--"]

library(data.table)
library(DBI)
require(RSQLite)

write_outputs <- function(db, ID, append_file) {
    print(paste("Compiling model:", names(ID)))

    params_wide <- fread(sprintf("./logs/param_log_%s.csv", ID), colClasses="character")
    params_wide[, "RunID":= .I]
    params <- melt(params_wide, measure.vars=patterns("market_prop_green","prob_m","SRoR", "Activism"),
                variable="time", value.name=c("market_prop_green","prob_m","SRoR", "Activism"), variable.factor=FALSE)
    params[, names(params[, -c("refID","strategy","reporting")]):= lapply(.SD[, -c("refID","strategy","reporting")], as.numeric)]
    params[, "time":= time - 1 + t0]
    params[refID=="", "refID":= NA]

    market_states <- fread(sprintf("./outputs/market_states_%s.csv", ID))
    lease_states <- fread(sprintf("./outputs/lease_states_%s.csv", ID))
    agent_states <- fread(sprintf("./outputs/agent_states_%s.csv", ID))

    params[, "model":= names(ID)]
    market_states[, "model":= names(ID)]
    lease_states[, "model":= names(ID)]
    agent_states[, "model":= names(ID)]

    # calculate the amount of gas deposited and withdrawn from storage due to excess supply
    lease_states[, "dw":= any(market=="none") * fifelse(market=="none", 1, -1, na=0), by=.(model, RunID, leaseID)]
    lease_states[, "net_dw":= cumsum(dw), by=.(model, RunID, leaseID)]
    while(min(lease_states$net_dw)<0) {
        lease_states[(dw<0) & (net_dw<0), "dw":= replace(dw, which.min(time), 0), by=.(model, RunID, leaseID)]
        lease_states[, "net_dw":= cumsum(dw), by=.(model, RunID, leaseID)]
    }
    market_states[lease_states[dw!=0, sum((gas_MCF + csgd_MCF) * dw), by=.(model, RunID, time)],
        on=c("model", "RunID", "time"), "q_stored":= V1]

    demand_file <- sprintf("./outputs/demand_function_%s-%%s.rds",
                        if(is.na(jobIDs$refID)) ID else jobIDs$refID)
    market_states[, "frac":= median(readRDS(sprintf(demand_file, .BY))$historical_market$frac), by=RunID]
    setcolorder(market_states, c("time", "p_grey", "p_green", "p_oil_mult", "q_grey", "q_green", "q_stored", "q_oil",
                                "market_prop_green", "frac", "RunID", "model"))

    agent_states[
        # add verification / validation metrics to agent states
        lease_states[(status=="producing"),
                        .(sum(csgd_MCF[class=="underdeveloped"]) + sum(sopf_MCF), sum(oil_BBL), sum(gas_MCF)),
                        by=.(model, RunID, time, firmID)],
            on=c("model", "RunID", "time", "firmID"), c("gas_flared_calc", "oil_prod", "gas_prod"):= .(V1, V2, V3)]
    setnafill(agent_states, cols=c("oil_prod", "gas_prod", "gas_flared_calc"), fill=0)

    # write outputs
    cat("\tWriting CSVs")
    fwrite(params, sprintf("./logs/params_%s.csv.gz", ID))
    fwrite(market_states, sprintf("./outputs/processed/market_states_%s.csv.gz", ID))
    fwrite(agent_states, sprintf("./outputs/processed/agent_states_%s.csv.gz", ID))

    # begin SQLite transaction and turn off autocommit
    dbBegin(db)
    cat("\tUpdating lookup table")
    # save space by using lookup table for string definitions
    if (!append_file) {
        # create keyed tabled WITHOUT ROWID column
        dbExecute(db, "CREATE TABLE string_lookup 
                        (
                          column_name TEXT, 
                          string_key TEXT, 
                          integer_key INTEGER, 
                          PRIMARY KEY (column_name, string_key)
                        ) WITHOUT ROWID;")
        model_num <- 1L
    } else {
        model_num <- dbGetQuery(db, "SELECT MAX(integer_key) FROM string_lookup WHERE column_name='model'") + 1
    }

    # convert string columns to integers
    dbExecute(db, "INSERT INTO string_lookup (column_name, string_key, integer_key) VALUES (?, ?, ?);",
                   list("model", names(ID), as.integer(model_num)))
    dbExecute(db, "INSERT INTO string_lookup (column_name, string_key, integer_key) VALUES (?, ?, ?);",
                   list("jobID", ID[[1]], as.integer(model_num)))

    for (dt in c("agent_states", "lease_states", "market_states", "params")) {
        get(dt)[, "model":= NULL]
        get(dt)[, "model":= as.integer(model_num)]
    }

    for (col in c("strategy", "reporting", "activity", "behavior", "class", "status", "market", "area")) {
        dt <- if (col %in% c("strategy", "reporting")) "params" else
                if (col %in% c("activity", "behavior")) "agent_states" else "lease_states"

        # ensure factor levels match existing records
        if (append_file) {
            factor_levels <- dbGetQuery(db, paste0("SELECT * FROM string_lookup WHERE column_name='", col,
                                                    "' ORDER BY integer_key"))$string_key
        } else {
            factor_levels <- c()
        }
        get(dt)[is.na(get(col)), c(col):= ""]
        get(dt)[, c(col):= factor(get(col), levels=c(factor_levels, setdiff(get(col), factor_levels)))]

        # update lookup table
        get(dt)[, .("cname"=col, "skey"=levels(get(col)), "ikey"=seq(nlevels(get(col))))][,
            dbExecute(db, "INSERT OR IGNORE INTO string_lookup (column_name, string_key, integer_key) 
                            VALUES (:cname, :skey, :ikey);", .SD)]
        # cast factor to integer
        get(dt)[, c(col):= as.integer(get(col))]
    }

    # store repeated columns of lease and agent states in separate table
    agent_info <- agent_states[, first(.SD), by=.(model, RunID, firmID),
                                .SDcols=patterns("^production_*")]
    agent_states[, setdiff(names(agent_info), c("model", "RunID", "firmID")):= NULL]

    lease_info <- lease_states[, .SD[which.max(time)], by=.(model, RunID, leaseID, area, DISTRICT_NO, OIL_GAS_CODE),
                                .SDcols=-patterns("dw|^cost_*|^ERR_MCF$|^class$|^market$|^lifetime$")]
    lease_states[, setdiff(names(lease_info), c("model", "RunID", "leaseID", "time", "status")):= NULL]
    setnames(lease_info, "time", "t_last")

    cat("\tWriting data to db\n")
    for (dt in c("params", "market_states", "agent_info", "agent_states", "lease_info", "lease_states")) {
        keys <- c("model", "RunID", if(!grepl("info", dt)) "time",
                    if(grepl("agent", dt)) "firmID" else if(grepl("lease", dt)) "leaseID")
        if (!append_file) {
            # create keyed tabled WITHOUT ROWID column
            sql <- sub("\n)\n$",
                        sprintf(",\n  \\PRIMARY KEY(%s ASC) \n) WITHOUT ROWID;\n", paste(keys, collapse=" ASC, ")),
                        as.character(sqlCreateTable(db, dt, get(dt), row.names=FALSE)))
            dbExecute(db, sql)
        } else {
            # order columns like existing sql table
            setcolorder(get(dt), dbGetQuery(db, sprintf("PRAGMA table_info(%s)", dt))$name)
        }

        # match order of primary keys to speed up write time
        setorderv(get(dt), cols= keys)

        dbExecute(db, paste0("INSERT INTO ", dt, " VALUES(:", paste(names(get(dt)), collapse=", :"), ")"), get(dt))
        rm(dt)
    }
    if (!("demand_functions" %in% dbListTables(db))) {
        # store demand functions as blobs
        dbExecute(db, "CREATE TABLE demand_functions 
                        (
                          RunID INTEGER, 
                          fun_bin BLOB, 
                          PRIMARY KEY (RunID)
                        ) WITHOUT ROWID;")
        demand_funs <- sapply(list.files(dirname(demand_file),
                            gsub("%s.*", "*", basename(demand_file)),
                            full.names=TRUE), readRDS)
        names(demand_funs) <- gsub(".*-(.*).rds", "\\1", names(demand_funs))
        dbExecute(db,
            "INSERT INTO demand_functions (RunID, fun_bin) VALUES (?, ?);",
            list(names(demand_funs), lapply(demand_funs, serialize, NULL)))
    }
    dbCommit(db)
}


jobIDs <- lapply(strsplit(args, "="), `[[`, 2)
names(jobIDs) <- gsub("\\n", "\n", sapply(strsplit(args, "="), `[[`, 1), fixed=TRUE)
print(jobIDs)

if(!("refID" %in% names(jobIDs))) {
    jobIDs <- c(jobIDs, "refID"=NA)
}

all_states <- dbConnect(RSQLite::SQLite(),
                sprintf("%s/CSR-ABM/outputs/processed/all_states_%s.sqlite",
	                fcoalesce(Sys.getenv("WORK", unset=NA), ".."),
                    fcoalesce(jobIDs$refID,
                        paste(na.omit(jobIDs), collapse="-"))))

# turn off safety measures since all data is backed up in CSVs
dbExecute(all_states, "PRAGMA journal_mode = OFF;")
dbExecute(all_states, "PRAGMA synchronous = OFF;")

lapply(seq(jobIDs)[-which(names(jobIDs)=="refID")], function(i) write_outputs(all_states, jobIDs[i], i!=1))

# optimize later queries
dbExecute(all_states, "PRAGMA analysis_limit=1000;")
dbExecute(all_states, "PRAGMA optimize;")
dbDisconnect(all_states)

print(warnings())
print("Finished assembling databases")
