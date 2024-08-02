args <- commandArgs(trailingOnly=TRUE)

library(data.table)
library(ggplot2)
library(ggrepel)
library(grid)
library(svglite)
library(DBI)

jobIDs <- lapply(strsplit(args, "="), `[[`, 2)
names(jobIDs) <- gsub("\\n", "\n", sapply(strsplit(args, "="), `[[`, 1), fixed=TRUE)
print(jobIDs)

db <- dbConnect(RSQLite::SQLite(),
    sprintf("./outputs/processed/all_states_%s.sqlite", jobIDs$refID))

lookup_sql <- "SELECT %s FROM string_lookup WHERE column_name='%s' AND %s"

# custom theme
theme_custom <- function() {
    theme_bw(base_size=18, base_family="sans") %+replace%
    theme(  # title
            plot.title = element_text(
                            size = rel(1.25),                   # larger font size
                            face = 'bold',                      # bold typeface
                            hjust = 0.5,                        # center align
                            vjust = 1, margin = margin(b=14/2)),# other defaults
            plot.subtitle = element_text(hjust=0.5, face="italic",
                                    vjust=1, margin=margin(b=14/2)),
            # legend
            legend.background = element_rect(fill="white", color="black"),

            # transparent plot background
            plot.background = element_rect(fill="transparent", color=NA)
    )
}

# better break points for adjusted log scale
log1p_breaks <- function(x, n) { replace(axisTicks(c(0, log10(max(x))), log=TRUE, n=n), 1, 0) }

# label panels facetted by basis
basis_labeller <- function(x) {
    units <- ifelse(x=="gas", "[MCF/MCF]", "[MCF/BBL]")
    main <- tools::toTitleCase(paste(x, "basis"))
    return(paste(main, units))
}

# generate unique color pallette for model runs
colors <- dbGetQuery(db, sprintf(lookup_sql, "string_key AS model, integer_key", "model", "TRUE ORDER BY integer_key"))
setDT(colors)

colors[model %in% c("complete", "CA market", "target top", "under-reported"), "color":= "#000000"]
colors[is.na(color), "color":= palette.colors(.N, palette="Tableau 10")]
colors[, "color":= replace(color, model=="CA market", rgb(colorRamp(c(color[model=="EU market"], 1))(0.5)/255))]
colors[, "color":= replace(color, model=="target top", rgb(colorRamp(c(color[model=="target even"], 1))(0.5)/255))]
colors[, "color":= replace(color, model=="under-reported", rgb(colorRamp(c(color[model=="mis-reported"], 1))(0.5)/255))]

model_colors <- colors[, setNames(color, model)]


params <- dbGetQuery(db, "SELECT * FROM params")
setDT(params)
for (col in c("model", "strategy", "reporting")) {
    params[, c(col):= as.character(get(col))]
    params[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}
params[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

market_history <- dbGetQuery(db, "SELECT * FROM market_states")
setDT(market_history)
market_history[, "model":= as.character(model)]
market_history[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                        "string_key", "model", paste0("integer_key=", .BY)))), by=model]
market_history[, "model":= factor(model, intersect(unique(model), names(model_colors)))]



##### INITIALIZATION #####
sql <- "SELECT * FROM %2$s 
        LEFT JOIN %1$s ON %2$s.leaseID=%1$s.leaseID AND %1$s.RunID=%2$s.RunID AND %1$s.model=%2$s.model 
            WHERE %2$s.model=1 
            AND %2$s.time < (SELECT MIN(t0) FROM params WHERE model=1)"

leases <- dbGetQuery(db, sprintf(sql, "lease_info", "lease_states"))
setDT(leases)
leases[, which(duplicated(names(leases))) := NULL]

for (col in c("model", "area", "market", "status", "class")) {
    leases[, c(col):= as.character(get(col))]
    leases[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}

# gas operating costs
svglite("./graphics/lease_gas_cost.svg", height=7, width=12, bg="transparent")
ggplot(leases[gas_MCF+csgd_MCF>0], aes(x=opEx_pMCF + ifelse(csgd_MCF==0, 0, capEx_csgd / csgd_MCF / lifetime))) +
    stat_density(geom="line", aes(lty=ifelse(OIL_GAS_CODE=="O", "Oil lease", "Gas lease")), lwd=1, position="identity") +
    scale_linetype_manual(values=c("dotted","dotdash")) +
    theme_custom() + theme(legend.position=c(0.85, 0.8), legend.title=element_blank()) +
    labs(x="Average Cost per MCF", title="Gas operating costs") +
    xlim(0, 5)
dev.off()


# oil operating costs
fed_data <- fread(text="area,           year, Mean,  Min, Max
                        Eagle Ford,     2017, 29,    10,  50
                        Eagle Ford,     2021, 16.58, 7,   35
                        Delaware Basin, 2017, 33,    15,  55
                        Delaware Basin, 2021, 26.19, 7,   50
                        Midland Basin,  2017, 24,    10,  55
                        Midland Basin,  2021, 27.05, 6,   60")

svglite("./graphics/lease_oil_cost.svg", height=7, width=12, bg="transparent")
ggplot(leases[oil_BBL>0], aes(color=area)) +
    geom_density(data= function(x) {x[,"area":= ifelse(area=="Spraberry", "Midland Basin", area)]},
                aes(x=cost_oil/oil_BBL, color=area), lwd=1) +
    geom_point(aes(x=Mean, y=(0.5 + (year==2021))), data=fed_data) +
    geom_errorbarh(aes(xmin=Min, xmax=Max, y=(0.5 + (year==2021))), alpha=0.8, lty=2, lwd=1, data=fed_data, show.legend=FALSE) +
    annotate("text", x=fed_data[year==2017, max(Mean)], y=0.5, label="2017", alpha=0.7, vjust=-1) +
    annotate("text", x=fed_data[year==2021, min(Mean)], y=1.5, label="2021", alpha=0.7, vjust=-1) +
    scale_x_log10() +
    theme_custom() + theme(legend.position=c(0.85, 0.8), legend.title=element_blank()) +
    labs(x="$ / barrel", title="Oil operating costs at oil leases", color="")
dev.off()
rm(fed_data)


# distribution of firm production
svglite("./graphics/prod_joint.svg", height=7, width=12, bg="transparent")
market_shares <- fread("./inputs/processed/firm_market_shares.csv")
ggplot(melt(market_shares, id.vars="scaled_oil_BBL", measure.vars=patterns("_MCF$"), variable.name="type")) +
    stat_density_2d(geom="raster", aes(x=scaled_oil_BBL, y=value, fill=after_stat(density)), contour=FALSE) +
    stat_bin_2d(geom="point", aes(x=V1, y=V2, size=after_stat(count), color="red", fill=NULL), alpha=0.5,
        data=leases[t_found<=time, .(sum(oil_BBL), c(sum(gas_MCF), sum(csgd_MCF)),
                                    "type"=c("scaled_gas_MCF","scaled_csgd_MCF")), by=.(RunID, firmID)]) +
    scale_size_area(guide=FALSE) +
    scale_color_manual("", labels="Generated", values="red", guide=guide_legend(override.aes=list("alpha"=1))) +
    scale_fill_viridis_c("Empirical") +
    scale_x_continuous(trans=scales::log1p_trans(), breaks= function(x) {log1p_breaks(x, 5)}) +
    scale_y_continuous(trans=scales::log1p_trans(), breaks= function(x) {log1p_breaks(x, 5)}) +
    facet_grid(.~type, labeller=as_labeller(function(x) ifelse(x=="scaled_gas_MCF", "Unassociated Gas", "Casinghead Gas"))) +
    theme_custom() + theme(legend.background=element_rect(fill="white", color=NA)) + coord_cartesian(expand=FALSE) +
    labs(title="Joint distribution of firm oil and gas production", x="Production (BBL)", y="Production (MCF)")
dev.off()

rm(leases)
rm(market_shares)




############################################# WARM-START #############################################
# sample natural gas market
sql <- "SELECT %1$s.gas_MCF, %1$s.csgd_MCF, %1$s.opEx_pMCF, %2$s.market, %2$s.RunID, %2$s.model FROM %2$s 
        INNER JOIN %1$s ON %2$s.leaseID=%1$s.leaseID AND %1$s.RunID=%2$s.RunID AND %1$s.model=%2$s.model 
            AND %2$s.time=(SELECT MAX(time) FROM %2$s) 
            AND %2$s.market IN (%3$s) 
            AND %2$s.model=(%4$s) 
            AND (%1$s.gas_MCF>0 OR %1$s.csgd_MCF>0) 
        ORDER BY opEx_pMCF ASC"

gas_supply <- dbGetQuery(db, sprintf(sql, "lease_info", "lease_states",
                               sprintf(lookup_sql, "integer_key", "market", "string_key IN ('green','grey')"),
                               sprintf(lookup_sql, "integer_key", "model", "string_key='complete'")))

setDT(gas_supply)
for (col in c("model", "market")) {
    gas_supply[, c(col):= as.character(get(col))]
    gas_supply[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}
gas_supply[, "q_sup":= cumsum(gas_MCF+csgd_MCF), by=.(model, RunID, market)]
gas_supply[market_history, on=c("model","RunID"), "max_prop_green":= market_prop_green]

gas_demand <- readRDS(sprintf("./outputs/demand_function_%s-%s.rds", jobIDs$refID, 1))
# gas_demand <- readRDS(sprintf("./outputs/demand_function_%s-%s.rds", jobIDs$refID, gas_supply[, RunID[which.max(opEx_pMCF)]]))

gas_supply[, c("p_grey","p_green"):= gas_demand$new_schedule(first(max_prop_green))(q_sup)[, .(p_grey, p_green)]]
while (gas_supply[market=="green", all(p_green>opEx_pMCF)]) {
    gas_supply[, c("p_grey","p_green"):= gas_demand$new_schedule(first(max_prop_green))(q_sup)[, .(p_grey, p_green)]]
}

svglite("./graphics/NG_market_sample.svg", height=7, width=12, bg="transparent")
ggplot(gas_supply[, if (any((p_green<opEx_pMCF) & (market=="green"))) .SD, by=.(model, RunID)][RunID==min(RunID)],
        aes(x=q_sup, color=market)) +
    geom_line(aes(y=opEx_pMCF), size=0.85) +
    geom_line(aes(y=p_grey, color="grey"), linetype="dashed", size=0.85) +
    geom_line(aes(y=replace(p_green, p_green==0, NA), color="green"), linetype="dashed", size=0.85) +
    scale_color_manual(values=c("grey"="grey40", "green"="darkgreen")) +
    scale_x_continuous(labels = function(x) format(x, scientific=TRUE)) +
    labs(x="Quantity (MCF / month)", y="Price ($)", title="Sample Natural Gas Market", color="Market") +
    theme_custom() + theme(legend.position=c(0.85,0.2))
dev.off()


# flaring intensity
sql <- "SELECT SUM(gas_flared)/SUM(oil_output) AS oil, SUM(gas_flared)/SUM(gas_output) AS gas, 
                    %2$s.model, %2$s.RunID, %2$s.time FROM %2$s 
                    INNER JOIN %1$s ON %2$s.firmID=%1$s.firmID AND %1$s.RunID=%2$s.RunID AND %1$s.model=%2$s.model 
                    AND %2$s.model IN (%3$s)
                GROUP BY %2$s.model, %2$s.RunID, %2$s.time"
intensity_sql <- sprintf(sql, "agent_info", "agent_states", sprintf(lookup_sql, "integer_key", "model", "%1$s"))

flaring_intensity <- dbGetQuery(db, sprintf(intensity_sql, "string_key='complete'"))
setDT(flaring_intensity)
flaring_intensity[, "model":= as.character(model)]
flaring_intensity[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
flaring_intensity[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/market_intensity.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    geom_line(aes(group=RunID), alpha=0.25) +
    stat_summary(geom="line", fun="mean", lwd=1) +
    stat_summary(geom="ribbon", fun.data="mean_se", alpha=0.5) +
    geom_vline(xintercept=0, lty=1, color="grey") +
    annotate(x=0, y=Inf, "text", label="Warm start", hjust=1.1, vjust=1.5) +
    annotate(x=0, y=Inf, "text", label=" Forecast", hjust=-0.1, vjust=1.5) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Market evolution", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom()
dev.off()

svglite("./graphics/market_intensity_forecast.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity[time>=-1], measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    geom_line(aes(group=RunID), alpha=0.25) +
    stat_summary(geom="line", fun="mean", lwd=1) +
    stat_summary(geom="ribbon", fun.data="mean_se", alpha=0.5) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Market evolution", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom()
dev.off()
rm(flaring_intensity)


## Grey market price distribution
monthly_prices <- fread("./inputs/processed/historical_NG_demand.csv")
monthly_prices[, "Date":= as.Date(paste("01", month, year), format='%d %b %Y')]
warm_models <- params[t0<0, unique(model)]

svglite("./graphics/prices_complete.svg", height=7, width=12, bg="transparent")
boxplot(data.table(matrix(nrow=0, ncol=2)),
        boxfill=NA, border=NA, ylim=c(1,6), xaxt="n", cex.lab=18/11)
title("Empirical & modeled distribution of standard gas prices", cex.main=18/11)
title(ylab="$ / MCF", line=2.5, cex.lab=18/11)
abline(h=1:6, col = "grey", lty = "dotted")
boxplot(monthly_prices[year>2010]$p, boxwex=1.6, add=TRUE, at=1, yaxt="n")
axis(1, at=1, labels="empirical", col.axis="blue", cex.axis=18/11)
boxplot(market_history[(time<0) & (model=="complete")]$p_grey, add=TRUE, boxwex=1.5, at=2, yaxt="n")
axis(1, at=2, labels="modeled", cex.axis=18/11)
dev.off()

svglite("./graphics/prices_all.svg", height=7, width=12, bg="transparent")
boxplot(data.table(matrix(nrow=0, ncol=length(warm_models)+1)),
        boxfill=NA, border=NA, ylim=c(1,6), xaxt="n", cex.axis=18/11)
title("Empirical & modeled distribution of standard gas prices", cex.main=18/11)
title(ylab="$ / MCF", line=2.5, cex.lab=18/11)
abline(h=1:6, col = "grey", lty = "dotted")
boxplot(monthly_prices[year>2010]$p, boxwex=1.6, add=TRUE, at=1, yaxt="n")
axis(1, at=1, labels="empirical", col.axis="blue", cex.axis=18/11)
boxplot(p_grey~as.character(model), data=market_history[(time<0) & (model %in% warm_models)],
    add=TRUE, at=c(seq(length(warm_models)) + 1), yaxt="n", cex.axis=18/11)
dev.off()


############################################# FORECAST #############################################

# agent responses
sql <- "SELECT COUNT(firmID) AS N, model, RunID, time, behavior FROM %1$s 
        WHERE model=(%2$s) 
        GROUP BY model, RunID, time, behavior"
behaviors <- dbGetQuery(db, sprintf(sql, "agent_states",
                                sprintf(lookup_sql, "integer_key", "model", "string_key='complete'")))
setDT(behaviors)
for (col in c("model", "behavior")) {
    behaviors[, c(col):= as.character(get(col))]
    behaviors[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}
behaviors[, "model":= factor(model, intersect(unique(model), names(model_colors)))]


svglite("./graphics/complete_firm_types.svg", height=7, width=12, bg="transparent")
ggplot(behaviors[behavior!="flaring"], aes(x=time, y=N)) +
    geom_step(aes(group=RunID), alpha=0.25) +
    stat_summary(geom="step", fun="mean", lwd=1) +
    stat_summary(geom="ribbon", fun.data="mean_se", alpha=0.5) +
    facet_grid(behavior~., scales="free_y") +
    labs(title="Types of certified firms", x="", y="Number of firms") +
    theme_custom()
dev.off()
rm(behaviors)

# density of firms
agent_end_states <- dbGetQuery(db, "SELECT market_value, profit, oil_output, gas_output, gas_flared, behavior, model, RunID 
                                    FROM agent_states 
                                    WHERE time=(SELECT MAX(tf) FROM params)")
setDT(agent_end_states)
for (col in c("model", "behavior")) {
    agent_end_states[, c(col):= as.character(get(col))]
    agent_end_states[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}
agent_end_states[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

## market value
svglite("./graphics/complete_density.svg", height=7, width=12, bg="transparent")
ggplot(agent_end_states[(model=="complete") & (gas_flared>0)], aes(x=market_value, y=gas_flared/oil_output)) +
    geom_density_2d_filled(show.legend=FALSE) +
    geom_hline(yintercept=0.05, color="white", linetype=2, lwd=1) +
    scale_x_continuous(trans=scales::log1p_trans(), breaks= function(x) {log1p_breaks(x, 5)}) + scale_y_log10() +
    annotate("text", x=100, y=0.05, label="Certified market\nthreshold", color="white") +
    labs(title="Density of all firms", x="Market Value", y="Flaring Intensity") +
    theme_custom() + coord_cartesian(expand=FALSE)
dev.off()

svglite("./graphics/complete_distribution.svg", height=7, width=12, bg="transparent")
ggplot(agent_end_states[(behavior!="flaring") & (model=="complete")], aes(x=market_value)) +
    stat_density(aes(color=behavior), geom="line", position="identity") +
    scale_x_continuous(trans=scales::log1p_trans(), breaks= function(x) {log1p_breaks(x, 5)}) +
    labs(title="Density of non flaring firms", x="Market Value", color="Behavior:") +
    theme_custom() + theme(legend.position=c(0.85,0.8)) + coord_cartesian(expand=FALSE)
dev.off()


## bankruptcies
agent_end_states[, "BOE_output":= oil_output + gas_output/6]

svglite("./graphics/bankruptcies_all.svg", height=7, width=12, bg="transparent")
ggplot(agent_end_states[, sum(profit<0) / .N, by=.(RunID, model)]) +
    geom_boxplot(aes(x=V1, y=model, color=model), show.legend=FALSE) +
    scale_x_continuous(labels=scales::percent) +
    scale_color_manual(values=model_colors) +
    labs(title="Bankrupt firms", x="Percent of firms", y="") +
    theme_custom()
dev.off()

svglite("./graphics/bankruptcies_behavior.svg", height=7, width=12, bg="transparent")
ggplot(agent_end_states[model=="complete", sum(profit<0) / .N, by=.(RunID, behavior)]) +
    geom_boxplot(aes(x=V1, y=behavior)) +
    scale_x_continuous(labels=scales::percent) +
    labs(title="Bankrupt firms", x="Percent of firms", y="") +
    theme_custom()
dev.off()

svglite("./graphics/bankruptcies_production.svg", height=7, width=12, bg="transparent")
ggplot(agent_end_states[, sum(BOE_output[profit<0]) / sum(BOE_output), by=.(RunID, model)]) +
    geom_boxplot(aes(x=V1, y=model, color=model), show.legend=FALSE) +
    scale_x_continuous(labels=scales::percent) +
    scale_color_manual(values=model_colors) +
    labs(title="Bankrupt firms", x="Percent of production [BOE/BOE]", y="") +
    theme_custom()
dev.off()

rm(agent_end_states)

# earnings distribution
agent_states <- dbGetQuery(db, sprintf("SELECT market_value, grey_gas_sold, green_gas_sold, 
                                        gas_flared, behavior, model, RunID, time 
                                        FROM agent_states 
                                        WHERE time>0 AND model IN (%s)",
                                        sprintf(lookup_sql, "integer_key", "model",
                                            "string_key IN ('complete','CA market','EU market')")))
setDT(agent_states)
for (col in c("model", "behavior")) {
    agent_states[, c(col):= as.character(get(col))]
    agent_states[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}
agent_states[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

agent_states[market_history, on=c("model","RunID","time"),
                    "gas_revenue":= (p_grey*grey_gas_sold) + (p_green*green_gas_sold)]

svglite("./graphics/earnings_dist.svg", height=7, width=12, bg="transparent")
ggplot(agent_states) +
    geom_violin(aes(x=behavior, y=gas_revenue / (grey_gas_sold + green_gas_sold), color=model), draw_quantiles=0.5) +
    scale_x_discrete(limits=c("flaring", "economizing", "mitigating", "imitating")) +
    scale_color_manual(values=model_colors) +
    labs(title="Firm earnings for gas", x="Behavior", y="Earnings per MCF", color="") +
    theme_custom() + theme(legend.position="bottom")
dev.off()

rm(agent_states)

# shareholder validation
sql <- "SELECT * FROM %2$s 
        LEFT JOIN %1$s ON %2$s.firmID=%1$s.firmID AND %1$s.RunID=%2$s.RunID AND %1$s.model=%2$s.model 
            WHERE %2$s.model=(%3$s)
            AND %2$s.time>0"

agent_states <- dbGetQuery(db, sprintf(sql, "agent_info", "agent_states",
                                sprintf(lookup_sql, "integer_key", "model", "string_key='complete'")))
setDT(agent_states)
agent_states[, "model":= as.character(model)]
agent_states[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
agent_states[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

agent_states[params, on=c("model","RunID","time"), c("SRoR","t0"):= .(SRoR, t0)]


svglite("./graphics/shareholder_val.svg", height=7, width=12, bg="transparent")
ggplot(agent_states[(cost_M>0) & (time>0) & (market_value>0),
                        {"Ci" = cost_M*SRoR / (time - t0);
                        "MV0i" = pmax(market_value - Ci, 1e-10);
                        Ci / MV0i / cut(Ci/SRoR/MV0i, 10, labels=FALSE)}, by=.(RunID, time)]) +
    geom_boxplot(aes(x=as.factor(RunID), y=V1), outlier.shape=NA, show.legend=FALSE) +
    geom_hline(yintercept=0.18*0.19, lty=2) +
    geom_hline(yintercept=0.11+(0.18*0.19), lty=2) +
    labs(title="Effect of shareholder valuation on market value", x="RunID", y="% Increase in Market Value") +
    theme_custom() + coord_cartesian(ylim=c(0,0.5))
dev.off()
rm(agent_states)


# gas storage
svglite("./graphics/storage_dw.svg", height=7, width=12, bg="transparent")
ggplot(market_history[!is.na(q_stored) & (model=="complete")][order(time),
        .(time, "net"= cumsum(q_stored), "frac"= mean(frac)), by=.(model, RunID)], aes(x=time, y=net)) +
    geom_point(aes(group=RunID), alpha=0.25) +
    stat_summary(geom="line", fun="mean") +
    stat_summary(geom="ribbon", fun.data="mean_se", alpha=0.5) +
    geom_hline(aes(yintercept= mean(frac) * 400000 * 1000), color='darkred', linetype='dashed') +
    annotate("text", x=0, y=0.95*400000*1000*mean(market_history$frac), size=14/.pt,
            label="Scaled Working Gas Storage", color='darkred', hjust=1) +
    labs(title="Net gas stored and withdrawn", y="MCF", color="RunID") +
    theme_custom()
dev.off()


# gas premium
svglite("./graphics/certified_premium.svg", height=7, width=12, bg="transparent")
ggplot(market_history[(p_green>0) & (model=="complete")], aes(x=time, y=p_green/p_grey, group=RunID)) +
    geom_point(alpha=0.25) +
    geom_rug(alpha=0.25, color="grey40", sides='r', outside=TRUE) +
    coord_cartesian(clip='off') +
    geom_hline(yintercept=c(1.07, 1.3), color="grey40", linetype="dashed") +
    labs(title="Certified gas premium", y=expression(P[green] / P[grey]), color="RunID") +
    theme_custom() + theme(plot.margin=c(1,2,1,1) * theme_bw()$plot.margin)
dev.off()

svglite("./graphics/certified_premium_all.svg", height=7, width=12, bg="transparent")
ggplot(market_history[(p_green>0) & (model %in% c("complete","CA market", "EU market"))], aes(x=time, y=p_green/p_grey, group=RunID)) +
    geom_point(alpha=0.25) +
    geom_rug(alpha=0.25, color="grey40", sides='r', outside=TRUE) +
    coord_cartesian(clip='off') +
    geom_hline(yintercept=c(1.07, 1.3), color="grey40", linetype="dashed") +
    facet_grid(model~.) +
    labs(title="Certified gas premium", y=expression(P[green] / P[grey]), color="RunID") +
    theme_custom() + theme(strip.switch.pad.grid=unit(0.4,'cm'), strip.placement='outside')
dev.off()

svglite("./graphics/gas_prices_all.svg", height=7, width=12, bg="transparent")
ggplot(market_history[(time>0) & model %in% c("complete","CA market", "EU market"),
        melt(.SD, measure.vars=patterns("^p_g"))]) +
    geom_boxplot(aes(x=variable, y=value, color=model)) +
    scale_x_discrete(breaks=c("p_grey","p_green"), labels=c(expression(P[grey]), expression(P[green]))) +
    scale_color_manual(values=model_colors) +
    labs(title="Market prices", x="Time", y="$ / MCF", color="") +
    theme_custom() + theme(legend.position="bottom")
dev.off()


# total gas sold
svglite("./graphics/volume_sold.svg", height=7, width=12, bg="transparent")
ggplot(market_history[model %in% c("complete", "no differentiation")], aes(x=time, y=q_green+q_grey)) +
    geom_point(aes(group=RunID), alpha=0.25) +
    stat_summary(geom="line", fun="mean") +
    stat_summary(geom="ribbon", fun.data="mean_se", alpha=0.5) +
    facet_grid(model~.) +
    labs(title="Total gas sold", y="MCF") +
    theme_custom()
dev.off()


############################################# REPORTING #############################################
flaring_intensity <- dbGetQuery(db, sprintf(intensity_sql,
                                    "string_key IN ('complete' ,'under-reported', 'mis-reported')"))
setDT(flaring_intensity)
flaring_intensity[, "model":= as.character(model)]
flaring_intensity[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
flaring_intensity[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/reporting_intensity.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_vline(xintercept=0, lty=1, color="grey") +
    annotate(x=0, y=Inf, "text", label="Warm start", hjust=1.1, vjust=1.5) +
    annotate(x=0, y=Inf, "text", label=" Forecast", hjust=-0.1, vjust=1.5) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Market evolution", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()

svglite("./graphics/reporting_intensity_forecast.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity[time>=-1], measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_vline(xintercept=0, lty=1, color="grey") +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Market evolution", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()

main <-
ggplot(melt(flaring_intensity[time<0], measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Market equilibration", subtitle="Warm-start period", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")

inset <-
ggplot(flaring_intensity[between(time, -30, 0)], aes(x=time)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors, guide=NULL) + scale_fill_manual(values=model_colors, guide=NULL) +
    theme_custom() + theme(axis.title=element_blank())


gas_inset <-
layer(data = data.table(basis = "gas", value=0, time=-40),
        stat=StatIdentity, position=PositionIdentity, geom=ggplot2:::GeomCustomAnn, inherit.aes=TRUE,
        params=list(xmin=-50, xmax=0,
                    ymin=diff(layer_scales(main, j=1)$y$get_limits())/3,
                    ymax= max(layer_scales(main, j=1)$y$get_limits()),
                    grob=ggplotGrob(inset + aes(y=gas) + geom_hline(yintercept=c(0.05, 0.03), lty=2)))
    )

oil_inset <-
layer(data = data.table(basis = "oil", value=0, time=-40),
        stat=StatIdentity, position=PositionIdentity, geom=ggplot2:::GeomCustomAnn, inherit.aes=TRUE,
        params=list(xmin=-50, xmax=0,
                    ymin=diff(layer_scales(main, j=2)$y$get_limits())/3,
                    ymax= max(layer_scales(main, j=2)$y$get_limits()),
                    grob=ggplotGrob(inset + aes(y=oil) + geom_hline(yintercept=0.1, lty=2)))
    )


svglite("./graphics/reporting_intensity_inset.svg", height=7, width=12, bg="transparent")
main + gas_inset + oil_inset
dev.off()

rm(flaring_intensity, main, inset, gas_inset, oil_inset)



############################################# COMPONENTS #############################################
intensity_sql <- sub("GROUP BY", "    AND agent_states.time>=-1 \n\t    GROUP BY", intensity_sql)
flaring_intensity <- dbGetQuery(db, sprintf(intensity_sql,
                                    "string_key LIKE 'no%' OR string_key='complete'"))
setDT(flaring_intensity)
flaring_intensity[, "model":= as.character(model)]
flaring_intensity[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
flaring_intensity[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/component_intensity.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Impact of market components", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()
rm(flaring_intensity)



############################################# ACTIVISM #############################################
# activist strategy
flaring_intensity <- dbGetQuery(db, sprintf(intensity_sql,
                                    "string_key LIKE 'target%' OR 
                                    string_key IN ('complete','no stakeholder\nactivism')"))

setDT(flaring_intensity)
flaring_intensity[, "model":= as.character(model)]
flaring_intensity[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
flaring_intensity[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/activism_intensity.svg",height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Impact of activist targeting strategy", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()

flaring_intensity_der <- melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis")
flaring_intensity_der[, "der":= predict(smooth.spline(time, y=value), time, deriv=1)$y, by=.(RunID, model)]

svglite("./graphics/activism_intensity_diff.svg",height=7, width=12, bg="transparent")
ggplot(flaring_intensity_der[!is.na(der) & (time>=-1)], aes(x=time, y=der)) +
    geom_smooth(aes(color=model, fill=model), show.legend=FALSE) +
    stat_smooth(geom="line", lwd=1, aes(color=model)) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors) +
    labs(title="Impact of activist targeting strategy", x="Time", y="Derivative estimate of flaring intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()

# add annotations of flaring thresholds
labels <- flaring_intensity_der[between(time, 10, 50), .("y"=mean(value)), by=.(model, time, basis)]

labels[(model=="complete") & (time==25) & (basis=="gas"),
    "label":= "Target according\nto firm size"]
labels[(model=="no stakeholder\nactivism") & (time==20) & (basis=="gas"),
    "label":= "No activism"]
labels[(model=="target even") & (time==26) & (basis=="oil"),
    "label":= "Target industry\nbroadly"]
labels[(model=="target top") & (time==30) & (basis=="oil"),
    "label":= "Target worst\nenvironmental\nperformers"]
labels[is.na(label), "label":= ""]


top <-
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors, guide=NULL) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    geom_label_repel(data=labels, aes(x=time, y=y, label=label, fill=model),
                    color=ifelse(labels$model %in% c("complete","target top"), "white", "black"),
                    nudge_x=ifelse(labels$model %in% c("complete","target top"), -10, 10),
                    box.padding=1.5, max.overlaps=50, size=5,
                    segment.curvature=1e-20, segment.color="grey40") +
    labs(title="Impact of activist targeting strategy", subtitle="Forecast period", x="Time", y="Value") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(axis.text.x=element_blank(), axis.title.x=element_blank(),
        plot.margin=margin(l=40, b=5), axis.title.y=element_text(size=rel(0.8)))

bottom <-
ggplot(flaring_intensity_der[!is.na(der) & (time>=-1)], aes(x=time, y=der)) +
    geom_smooth(aes(color=model, fill=model), show.legend=FALSE) +
    stat_smooth(geom="line", lwd=1, aes(color=model)) +
    scale_color_manual(values=model_colors, guide=NULL) + scale_fill_manual(values=model_colors) +
    labs(x="Time", y="Derivative estimate") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(strip.background=element_blank(), strip.text=element_blank(),
        plot.margin=margin(l=40), axis.title.y=element_text(size=rel(0.8)))

svglite("./graphics/activism_intensity_long.svg", height=11, width=12, bg="transparent")
grid.draw(rbind(ggplotGrob(top), ggplotGrob(bottom)))
grid.text(label="Total Flaring Intensity", rot=90, gp=gpar(fontsize=18), x=0.02)
dev.off()


rm(flaring_intensity, flaring_intensity_der, labels, top, bottom)



############################################# MARKETS #############################################
# market threshold
flaring_intensity <- dbGetQuery(db, sprintf(intensity_sql,
                                    "string_key IN ('complete','lower threshold', 'no differentiation')"))
setDT(flaring_intensity)
flaring_intensity[, "model":= as.character(model)]
flaring_intensity[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
flaring_intensity[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/thresh_intensity.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity[model!="no differentiation"], measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Impact of certified market threshold", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()

# add annotations of flaring thresholds
labels <- melt(flaring_intensity[between(time, 10, 50)], measure.vars=c("gas","oil"), variable.name="basis")[,
            .("y"=mean(value)), by=.(model, time, basis)]

labels[params[, unique(threshold), by=model], on="model", "label":= sprintf("Threshold of\n%s MCF/BBL", V1)]
labels[time!=30, "label":= ""]
labels[!(((basis=="oil") & (model=="lower threshold")) | ((basis=="oil") & (model=="complete"))), "label":= ""]
labels[(basis=="gas") & (time==30) & (model=="no differentiation"), "label":= "No certified market"]


svglite("./graphics/thresh_intensity_alt.svg", height=6.25, width=12, bg="transparent")
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors, guide=NULL) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    geom_label_repel(data=labels, aes(x=time, y=y, label=label, fill=model),
                    color=ifelse(labels$model=="complete", "white", "black"),
                    nudge_y=ifelse(labels$model=="no differentiation", 0.01, 0),
                    nudge_x=ifelse(labels$model=="complete", -10, 10),
                    box.padding=1.5, max.overlaps=50, size=5,
                    segment.curvature=1e-20, segment.color="grey40") +
    labs(title="Impact of certified market threshold", subtitle="Forecast period", x="Time", y="Total Flaring Intensity") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom()
dev.off()

rm(flaring_intensity, labels)

# market structure
flaring_intensity <- dbGetQuery(db, sprintf(intensity_sql,
                                    "string_key IN ('complete','EU market','CA market','no differentiation')"))
setDT(flaring_intensity)
flaring_intensity[, "model":= as.character(model)]
flaring_intensity[, "model":= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                "string_key", "model", paste0("integer_key=", .BY)))), by=model]
flaring_intensity[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/structure_intensity.svg", height=7, width=12, bg="transparent")
ggplot(melt(flaring_intensity, measure.vars=c("gas","oil"), variable.name="basis"), aes(x=time, y=value)) +
    stat_summary(geom="line", fun="mean", lwd=1, aes(color=model)) +
    stat_summary(geom="ribbon", fun.data="mean_se", aes(fill=model), alpha=0.25) +
    scale_color_manual(values=model_colors) + scale_fill_manual(values=model_colors, guide=NULL) +
    geom_hline(aes(yintercept=y), lty=2, data=data.table("y"=c(0.1, 0.05, 0.03), "basis"=c("oil","gas","gas"))) +
    labs(title="Impact of certified market size", x="Time", y="Total Flaring Intensity", color="") +
    facet_wrap(.~basis, scales="free_y", labeller=as_labeller(basis_labeller)) +
    theme_custom() + theme(legend.position="bottom")
dev.off()
rm(flaring_intensity)



############################################# CONTEXT #############################################

# flaring by region
sql <- "SELECT SUM(CASE WHEN class=(%3$s) THEN csgd_MCF ELSE 0 END) + SUM(sopf_MCF) AS gas_flared,
        SUM(CASE WHEN class!=(%4$s) THEN oil_BBL+cond_BBL ELSE 0 END) AS oil_output,
        %1$s.model, %1$s.RunID, %2$s.time, %1$s.area FROM %2$s 
        INNER JOIN %1$s ON %2$s.leaseID=%1$s.leaseID AND %1$s.RunID=%2$s.RunID AND %1$s.model=%2$s.model 
            AND %1$s.model IN (%5$s) 
            AND %2$s.time IN (SELECT 0 UNION SELECT MAX(tf) FROM params WHERE model=1) 
            AND %2$s.status=(%6$s) 
        GROUP BY %1$s.model, %1$s.RunID, %2$s.time, %1$s.area"

flare_area <- dbGetQuery(db, sprintf(sql, "lease_info", "lease_states",
                                    sprintf(lookup_sql, "integer_key", "class", "string_key='underdeveloped'"),
                                    sprintf(lookup_sql, "integer_key", "class", "string_key='undeveloped'"),
                                    sprintf(lookup_sql, "integer_key", "model",
                                            "string_key IN ('complete','under-reported', 'mis-reported')"),
                                    sprintf(lookup_sql, "integer_key", "status", "string_key='producing'")))

setDT(flare_area)
for (col in c("model", "area")) {
    flare_area[, c(col):= as.character(get(col))]
    flare_area[, c(col):= as.character(dbGetQuery(db, sprintf(lookup_sql,
                                    "string_key", col, paste0("integer_key=", .BY)))), by=get(col)]
}
flare_area[, "model":= factor(model, intersect(unique(model), names(model_colors)))]

svglite("./graphics/area_complete.svg", height=7, width=12, bg="transparent")
ggplot(flare_area[, .(gas_flared / c(1, oil_output), c("Volume [MCF]", "Intensity [MCF/BBL]")),
        by=.(model, RunID, time, area)]) +
    geom_boxplot(aes(y=V1, x=area, fill=as.factor(time)), position="dodge") +
    scale_fill_manual(values=c("white", "white"), guide=NULL) +
    facet_wrap(.~V2, scales="free_y", strip.position="left") +
    labs(title="Regional differences in flaring", y=NULL, x="") +
    theme_custom() + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1),
                        strip.background = element_blank(), strip.placement = "outside")
dev.off()

svglite("./graphics/area_intensity.svg", height=7, width=12, bg="transparent")
ggplot(flare_area) +
    geom_boxplot(aes(y=gas_flared/oil_output, x=area, fill=as.factor(time)), position="dodge") +
    scale_fill_manual(values=c("white", "white"), guide=NULL) +
    facet_grid(.~model) +
    labs(title="Regional differences in flaring", y="Flaring Intensity (MCF/BBL)", x="") +
    theme_custom() + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
dev.off()

svglite("./graphics/area_volume.svg", height=7, width=12, bg="transparent")
ggplot(flare_area) +
    geom_boxplot(aes(y=gas_flared, x=area, fill=as.factor(time)), position="dodge") +
    scale_fill_manual(values=c("white", "white"), guide=NULL) +
    facet_grid(.~model) +
    labs(title="Regional differences in flaring", y="Flaring Volume (MCF)", x="") +
    theme_custom() + theme(axis.text.x=element_text(angle=45, vjust=1, hjust=1))
dev.off()

# optimize later queries
dbExecute(db, "PRAGMA analysis_limit=1000;")
dbExecute(db, "PRAGMA optimize;")
dbDisconnect(db)

# convert all fonts from Liberation Sans to Arial
setwd("./graphics/")
lapply(list.files(pattern=".svg$"), function(f) system(paste("sed -i 's/Liberation Sans/Arial/'", f)))
