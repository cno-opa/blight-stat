#Inspections.r
#
#Data Sources: 
#==========================
#https://data.nola.gov/Housing-Land-Use-and-Blight/Code-Enforcement-All-Inspections/uh5a-f7uw -- Total inspections from Socrata
#SQL query of inspections completed
#https://data.nola.gov/Housing-Land-Use-and-Blight/Code-Enforcement-Active-Pipeline/8pqz-ftzc -- Code Enforcement pipeline
#==========================
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
plotInspectionTotals <- function(final.date){
	#Plots the total number of inspections.
	#Data comes from two sources:
	#1. The CE All Inspections table on Socrata, which has all inspections that are entered into LAMA
	#2. The count of demolition inspections, which are not in LAMA and come from Code Enforcement (data is populated in Demo-Inspections.csv)
	
	#Pull down data from Socrata
	insp <- csvFromWeb("https://data.nola.gov/api/views/uh5a-f7uw/rows.csv?accessType=DOWNLOAD")
	
	#Process dates and filter for correct range
	insp$InspectionDate <- as.Date(insp$InspectionDate, "%m/%d/%Y")
	insp <- subset(insp,InspectionDate>=as.Date("2013-01-01") & InspectionDate <= final.date)
	insp$Month <- as.factor(as.yearmon(insp$InspectionDate))
	
	#Get demolition inspections
	demo.insp <- read.csv("Data\\Demo-Inspections.csv")
	
	#Summarize by month
	d <- group_by(insp, Month) %>%
	summarise(n = n())
	d$n <- d$n + demo.insp$DemoInspections
	d$target <- 1250
	
	#Get KPI: Number of 2015 Inspections
	d.2015 <- d[which(d$Month=="Jan 2015"):nrow(d),]
	cat("KPI -- Total 2015 Inspections:", sum(d.2015$n), "\n")
	
	#Make plots
	p <- lineOPA(d, "Month", "n", "Number of Inspections", labels = "format(n, big.mark = \",\", scientific = FALSE)")
	p <- p + geom_hline(yintercept = 1250, linetype = "dashed")
	p <- buildChart(p)
	ggsave("Final/Inspection-Totals.png", plot = p, width = 7.42, height = 5.75, units = "in")
}
plotInspectionTotals(final.date=as.Date("2015-05-31"))


inspAgeBins <- function(open,close){
	diffs <- close-open
	bins <- c()
	for (i in 1:length(diffs)) {
		if(diffs[i]<30){bins[i] <- "Less than 30 Days Old"}
		else if(diffs[i]>=30 & diffs[i]<=90){bins[i] <- "30-90 Days Old"}
		else(bins[i] <- "Greater than 90 Days Old")
	}
	bins <- factor(bins,levels=c("Less than 30 Days Old","30-90 Days Old","Greater than 90 Days Old"))
	return(bins)
}


##Notes:
#Adjusted barOPA code and need to update on GitHub
#Need to change colors so that red is on top
plotInspectionTime <- function(final.date){
	#Read in data and process dates (for now we need to use a SQL query because the Socrata dataset doesn't contain a column for the date an inspection was filed)
	#To get data, run SQL query, change date formats in Excel to "Short Date" and save as Inspections.csv
	#If/when this is uploaded to socrata we will need to adjust column names
	insp <- read.csv("Data\\Inspections.csv")
	insp$Inspection.Completed <- as.Date(insp$Inspection.Completed, "%m/%d/%Y")
	insp$Case.Established <- as.Date(insp$Case.Established, "%m/%d/%Y")
	
	#Filter for inspections completed in the relevant date range, and for new initial inspections
	start.date <- round_date(seq(final.date, length = 2, by = "-6 months")[2],"month")
	insp <- subset(insp,Inspection.Completed>=start.date & Inspection.Completed <= final.date)
	insp <- subset(insp,Type.of.Inspection=="Inspection")
	insp <- subset(insp,Case.Established>=as.Date("2013-08-30")) #cases before this date will be restarted
	
	#Get KPI: Average number of days to complete inspections
	insp.2015 <- subset(insp, Inspection.Completed >= as.Date("2015-01-01"))
	insp.2015 <- subset(insp, Case.Established >= as.Date("2013-01-01"))
	closed <- insp.2015$Inspection.Completed
	opened <- insp.2015$Case.Established
	days.to.complete <- sum(closed-opened)/nrow(insp.2015)
	cat("Average time to complete inspections:", days.to.complete, "\n")
	
	#Group by bins of the number of days to complete inspections
	insp$Time.to.Complete <- inspAgeBins(insp$Case.Established,insp$Inspection.Completed)
	insp$Month <- as.factor(as.yearmon(insp$Inspection.Completed))
	d <- group_by(insp, Month, Time.to.Complete) %>%
	summarise(n = n())
	
	#Make plot
	d$pos <- positionLabels(dat = d$n, cats = 3)
	fill <- c(lightBlue, darkBlue, "firebrick")
	p <- barOPA(data = d, x = "Month", y = "n", title = "Age of Completed New Inspections", 
		fill = "Time.to.Complete", position = "stack", labels = "n")
	p <- p + geom_text(aes_string(label = "n", y = "pos", color = "Time.to.Complete"), size = 3) + scale_colour_manual(values = c("grey30", "grey77", "grey77"), guide = FALSE)
	p <- p + scale_fill_manual(values = fill)
	p <- buildChart(p)
	p
	ggsave("Final/Inspection-Time.png", plot = p,  width = 7.42, height = 5.75)
}
plotInspectionTime(final.date=as.Date("2015-05-31"))

getInspectionBacklog <- function(final.date){
	#Pull down data from Socrata
	pipeline <- csvFromWeb("https://data.nola.gov/api/views/8pqz-ftzc/rows.csv?accessType=DOWNLOAD")
	
	#Process dates and filter for newly filed cases
	pipeline$CaseFiled <- as.Date(pipeline$CaseFiled, "%m/%d/%Y")
	p.insp <- subset(pipeline, Stage == "1 - Inspection")
	p.insp <- subset(p.insp, CaseFiled >= as.Date("2013-01-01") & CaseFiled <= final.date)
	
	#Deduplicate
	dupes <- findDupes(p.insp$GeoPIN, pipeline$GeoPIN)
	p.insp <- p.insp[-which(p.insp$GeoPIN %in% dupes),]
	
	#Group by bins of the number of days to complete inspections
	p.insp$Age.of.Cases <- inspAgeBins(open = p.insp$CaseFiled, close = rep(final.date, times=nrow(p.insp)))
	d <- group_by(p.insp, Age.of.Cases) %>%
	summarise(n = n())
	dates <- rep(final.date,nrow(d))
	Month <- as.factor(as.yearmon(dates))
	d <- cbind(Month, d)
	return(d)
}

fullInspectionBacklog <- function(final.date){
	old.backlog <- read.csv("data\\HistoricalInspectionBacklog.csv")
	old.backlog <- subset(old.backlog, select=c(Month, Age.of.Cases, n))
	old.backlog$Month <- as.Date(old.backlog$Month, "%m/%d/%Y")
	old.backlog$Month <- as.factor(as.yearmon(old.backlog$Month))
	
	new.backlog <- getInspectionBacklog(final.date)
	if(as.character(old.backlog$Month[nrow(old.backlog)]) != as.character(new.backlog$Month[nrow(new.backlog)])){
		full.backlog <- rbind(old.backlog, new.backlog)
	} else{full.backlog <- old.backlog}
	full.backlog$Age.of.Cases <- factor(full.backlog$Age.of.Cases, levels=c("Less than 30 Days Old","30-90 Days Old","Greater than 90 Days Old"))
	return(full.backlog)
}

plotInspectionBacklog <- function(final.date, cache){
	full.backlog <- fullInspectionBacklog(final.date)
	if(cache==TRUE){write.csv(full.backlog,"data\\HistoricalInspectionBacklog.csv")}
	full.backlog$pos <- positionLabels(dat <- full.backlog$n, cats = 3)
	fill <- c(lightBlue, darkBlue, "firebrick")
	p <- barOPA(data=full.backlog, x="Month", y="n", title="Age of Open Cases", 
		fill="Age.of.Cases", position="stack")
	p <- p + geom_text(aes_string(label = "n", y = "pos", color = "Age.of.Cases"), size = 3) + scale_colour_manual(values = c("grey30", "grey77", "grey77"), guide = FALSE)
	p <- p + scale_fill_manual(values = fill)
	p <- buildChart(p)
	p
	ggsave("Final/Inspection-Backlog.png", plot = p, width = 7.42, height = 5.75)
}
plotInspectionBacklog(final.date=as.Date("2015-05-31"), cache=FALSE) #set to true to save a version of the backlog with new monthly data 
	