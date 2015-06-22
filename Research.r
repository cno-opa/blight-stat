#Research.r
#
#Data Sources: 
#==========================
#SQL query of research completed
#https://data.nola.gov/Housing-Land-Use-and-Blight/Code-Enforcement-Active-Pipeline/8pqz-ftzc -- Code Enforcement pipeline
#==========================
#
#
packages = "C:\\RWorkingDir\\RPackages" 				             
.libPaths(packages)
setwd("O:\\Projects\\BlightStat\\Scripting")
setInternet2(TRUE)
library(grid)
library(gridExtra)
library(gtable)
require("jsonlite")
require("ggplot2")
require("dplyr")
require("lubridate")
require("zoo")
require("scales")
theme_set(theme_opa())
plotResearchTotals <- function(final.date){
	#Read in data and process dates (for now we need to use a SQL query because Socrata doesn't have a research dataset)
	#To get data, run SQL query, change date formats in Excel to "Short Date" and save as Research.csv
	#If/when this is uploaded to socrata we will need to adjust column names
	rsrch <- read.csv("Data\\Research.csv")
	rsrch$Research.Completed <- as.Date(rsrch$Research.Date,"%m/%d/%Y")

	#Filter for correct range and de-duplicate
	rsrch <- subset(rsrch,Research.Completed>=as.Date("2013-01-01") & Research.Completed <= final.date)
	rsrch$Month <- as.factor(as.yearmon(rsrch$Research.Completed))
	rsrch <- rsrch[order(rsrch$Research.Completed),]
	rsrch <- subset(rsrch,!duplicated(rsrch$CaseNo))
	
	#Summarize by month
	d <- group_by(rsrch, Month) %>%
	summarise(n = n())
	d.2015 <- d[which(d$Month=="Jan 2015"):nrow(d),]
	cat("KPI -- Total 2015 Research:", sum(d.2015$n), "\n")
	
	
	#Make plots
	theme_set(theme_opa())
	p <- lineOPA(d, "Month", "n", "Number of Cases Researched", labels = "format(n, big.mark = \",\", scientific = FALSE)")
	p <- p + geom_hline(yintercept = 4000/12, linetype = "dashed")
	p <- buildChart(p)
	p
	ggsave("Final/Research-Totals.png", plot = p, width = 7.42, height = 5.75)
}
plotResearchTotals(final.date=as.Date("2015-05-31"))


rsrchAgeBins <- function(start.date){
	start.year <- as.numeric(format(start.date, "%Y"))
	final.year <- c()
	for (i in 1:length(start.year)) {
		if(start.year[i]==2015){final.year[i] <- "Opened in 2015"}
		else if(start.year[i]==2014){final.year[i] <- "Opened in 2014"}
		else(final.year[i] <- "Opened before 2014")
	}
	final.year <- factor(final.year,levels=c("Opened before 2014", "Opened in 2014", "Opened in 2015"))
	return(final.year)
}

##Notes:
#Adjusted barOPA code and need to update on GitHub
#Need to change colors so that red is on top
plotResearchTime <- function(final.date){
	#Read in data and process dates (for now we need to use a SQL query because the Socrata dataset doesn't contain a column for the date an inspection was filed)
	#To get data, run SQL query, change date formats in Excel to "Short Date" and save as Inspections.csv
	#If/when this is uploaded to socrata we will need to adjust column names
	rsrch <- read.csv("Data\\Research.csv")
	rsrch$Research.Completed <- as.Date(rsrch$Research.Date,"%m/%d/%Y")
	rsrch$Case.Established <- as.Date(rsrch$Case.Established,"%m/%d/%Y")
	
	#Filter for research completed in the relevant date range (past 6 months)
	start.date <- round_date(seq(final.date, length = 2, by = "-6 months")[2],"month")
	rsrch <- subset(rsrch,Research.Completed>=start.date & Research.Completed <= final.date)
	
	#Group by bins of the year that cases were filed
	rsrch$Year.Opened <- rsrchAgeBins(rsrch$Case.Established)
	rsrch$Month <- as.factor(as.yearmon(rsrch$Research.Completed))
	d <- group_by(rsrch, Month, Year.Opened) %>%
	summarise(n = n())
	
	pos.1 <- positionLabels(dat = d$n[1:2], cats = 2)
	pos.2 <- positionLabels(dat = d$n[3:length(d$n)], cats = 3)
	d$pos <- c(pos.1, pos.2)

	p <- barOPA(data=d, x="Month", y="n", title="Filing Year of Cases Researched", 
		fill="Year.Opened", position="stack")
	p <- p + geom_text(aes_string(label = "n", y = "pos", color = "Year.Opened"), size = 3) + scale_colour_manual(values = c("grey77", "grey77", "grey30"), guide = FALSE)
	p <- buildChart(p)
	p
	ggsave("Final/Research-Time.png", plot = p, width = 7.42, height = 5.75)
}
plotResearchTime(final.date=as.Date("2015-05-30"))

getResearchBacklog <- function(final.date){
	#Pull down data from Socrata
	pipeline.url = "https://data.nola.gov/resource/8pqz-ftzc?$limit=20000"
	pipeline <- fromJSON(paste0(readLines(pipeline.url)))
	pipeline <- subset(pipeline, o_c=="Open")

	#Process dates and filter for newly filed cases
	pipeline$casefiled <- as.Date(pipeline$casefiled)
	p.rsrch <- subset(pipeline, stage == "2 - Title Research")
	p.rsrch <- subset(p.rsrch, casefiled >= as.Date("2013-08-30") & casefiled <= final.date) #cases before this date will be restarted
	
	#Deduplicate
	dupes <- findDupes(p.rsrch$geopin, pipeline$geopin)
	p.rsrch <- p.rsrch[-which(p.rsrch$geopin %in% dupes),]
	
	#Group by bins of the number of days to complete inspections
	p.rsrch$Year.Opened <- rsrchAgeBins(p.rsrch$casefiled)
	d <- group_by(p.rsrch, Year.Opened) %>%
	summarise(n = n())
	dates <- rep(final.date,nrow(d))
	Month <- as.factor(as.yearmon(dates))
	d <- cbind(Month, d)
	return(d)
}

fullResearchBacklog <- function(final.date){
	old.backlog <- read.csv("data\\HistoricalResearchBacklog.csv")
	old.backlog$Month <- as.Date(old.backlog$Month, "%m/%d/%Y")
	old.backlog$Month <- as.factor(as.yearmon(old.backlog$Month))
	new.backlog <- getResearchBacklog(final.date)
	
	if(as.character(old.backlog$Month[nrow(old.backlog)]) != as.character(new.backlog$Month[nrow(new.backlog)])){
		full.backlog <- rbind(old.backlog, new.backlog)
	} else{full.backlog <- old.backlog}
	
	full.backlog$Year.Opened <- factor(full.backlog$Year.Opened, levels=c("Opened before 2014", "Opened in 2014", "Opened in 2015"))
	return(full.backlog)
}

plotResearchBacklog <- function(final.date, cache){
	full.backlog <- fullResearchBacklog(final.date)
	pos.1 <- positionLabels(dat = full.backlog$n[1:2], cats = 2)
	pos.2 <- positionLabels(dat = full.backlog$n[3:length(full.backlog$n)], cats = 3)
	full.backlog$pos <- positionLabels(dat = full.backlog$n, cats = 3)

	if(cache==TRUE){write.csv(full.backlog,"data\\HistoricalResearchBacklog.csv")}
	p <- barOPA(data=full.backlog, x="Month", y="n", title="Filing Year of Open Cases", 
		fill="Year.Opened", position="stack")
	p <- p + geom_text(aes_string(label = "n", y = "pos", color = "Year.Opened"), size = 3) + scale_colour_manual(values = c("grey77", "grey77", "grey30"), guide = FALSE)
	p <- buildChart(p)
	p
	ggsave("Final/Research-Backlog.png", plot = p, width = 7.42, height = 5.75)
}
plotResearchBacklog(final.date=as.Date("2015-05-30"), cache=FALSE) #set to true to save a version of the backlog with new monthly data 
	

