# research.R
#
# Data Sources:
#==========================
#
# SQL query of research completed
# https://data.nola.gov/Housing-Land-Use-and-Blight/Code-Enforcement-Active-Pipeline/8pqz-ftzc -- Code Enforcement pipeline
# "Research.csv"
# "HistoricalResearchBacklog.csv"
#
#==========================
#
#

#TODO: collect KPIs

setInternet2(TRUE)

plotResearchTotals <- function(){
	# read in data and process dates (for now we need to use a SQL query because Socrata doesn't have a research dataset)
	# to get data, run SQL query, change date formats in Excel to "Short Date" and save as Research.csv
	# if/when this is uploaded to socrata we will need to adjust column names
	rsrch <- read.csv("./data/Research.csv")
	rsrch$Research.Completed <- as.Date(rsrch$Research.Date,"%m/%d/%Y")

	# filter for correct range and de-duplicate
	rsrch <- subset(rsrch,Research.Completed >= as.Date("2013-01-01") & Research.Completed <= final.date)
	rsrch$Month <- as.factor(as.yearmon(rsrch$Research.Completed))
	rsrch <- rsrch[order(rsrch$Research.Completed),]
	rsrch <- subset(rsrch,!duplicated(rsrch$CaseNo))

	# summarize by month
	d <- group_by(rsrch, Month) %>%
			 summarise(n = n())
	d.2015 <- d[which(d$Month=="Jan 2015"):nrow(d),]
	cat("KPI -- Total 2015 Research:", sum(d.2015$n), "\n")


	# make plots
	theme_set(theme_opa())
	p <- lineOPA(d, "Month", "n", "Number of Cases Researched", labels = "format(n, big.mark = \",\", scientific = FALSE)")
	p <- p + geom_hline(yintercept = 4000/12, linetype = "dashed")
	p <- buildChart(p)

	ggsave("./output/Research-Totals.png", plot = p, width = 7.42, height = 5.75)
}
plotResearchTotals()


rsrchAgeBins <- function(start.date){
	start.year <- as.numeric(format(start.date, "%Y"))
	final.year <- c()
	for (i in 1:length(start.year)) {
		if(start.year[i]==2015){final.year[i] <- "Opened in 2015"}
		else if(start.year[i]==2014){final.year[i] <- "Opened in 2014"}
		else(final.year[i] <- "Opened before 2014")
	}
	final.year <- factor(final.year, levels = c("Opened before 2014", "Opened in 2014", "Opened in 2015"))
	return(final.year)
}

plotResearchTime <- function(){
	# read in data and process dates (for now we need to use a SQL query because the Socrata dataset doesn't contain a column for the date an inspection was filed)
	# to get data, run SQL query, change date formats in Excel to "Short Date" and save as Inspections.csv
	# if/when this is uploaded to socrata we will need to adjust column names
	rsrch <- read.csv("./data/Research.csv")
	rsrch$Research.Completed <- as.Date(rsrch$Research.Date,"%m/%d/%Y")
	rsrch$Case.Established <- as.Date(rsrch$Case.Established,"%m/%d/%Y")

	# filter for research completed in the relevant date range (past 6 months)
	start.date <- round_date(seq(final.date, length = 2, by = "-6 months")[2],"month")
	rsrch <- subset(rsrch,Research.Completed >= start.date & Research.Completed <= final.date)

	# group by bins of the year that cases were filed
	rsrch$Year.Opened <- rsrchAgeBins(rsrch$Case.Established)
	rsrch$Month <- as.factor(as.yearmon(rsrch$Research.Completed))
	d <- group_by(rsrch, Month, Year.Opened) %>%
			 summarise(n = n())

	pos.1 <- positionLabels(dat = d$n[1:2], cats = 2)
	pos.2 <- positionLabels(dat = d$n[3:length(d$n)], cats = 3)
	d$pos <- c(pos.1, pos.2)

	p <- barOPA(data = d, x = "Month", y = "n", title = "Filing Year of Cases Researched", fill = "Year.Opened", position = "stack") +
			 geom_text(aes_string(label = "n", y = "pos", color = "Year.Opened"), size = 3) +
			 scale_colour_manual(values = c("grey77", "grey77", "grey30"), guide = FALSE)
	p <- buildChart(p)

	ggsave("./output/Research-Time.png", plot = p, width = 7.42, height = 5.75)
}
plotResearchTime()

getResearchBacklog <- function(){
	# pull down data from Socrata
	pipeline.url <- "https://data.nola.gov/api/views/8pqz-ftzc/rows.csv?accessType=DOWNLOAD"
	#pipeline.url <- "https://data.nola.gov/api/views/8pqz-ftzc/rows.json?accessType=DOWNLOAD"
	pipeline <- csvFromWeb(pipeline.url)
	names(pipeline) <- slugify(names(pipeline))
	pipeline <- subset(pipeline, o_c == "Open")

	# process dates and filter for newly filed cases
	pipeline$casefiled <- toDate(pipeline$casefiled)
	p.rsrch <- subset(pipeline, stage == "2 - Title Research")
	p.rsrch <- subset(p.rsrch, ymd(casefiled) >= ymd("2013-08-30") & ymd(casefiled) <= ymd(final.date)) #cases before this date will be restarted

	# deduplicate
	dupes <- findDupes(p.rsrch$geopin, pipeline$geopin)
	p.rsrch <- p.rsrch[-which(p.rsrch$geopin %in% dupes),]

	# group by bins of the number of days to complete inspections
	p.rsrch$Year.Opened <- rsrchAgeBins(p.rsrch$casefiled)
	d <- group_by(p.rsrch, Year.Opened) %>%
			 summarise(n = n())
	dates <- rep(final.date,nrow(d))
	Month <- as.factor(as.yearmon(dates))
	d <- cbind(Month, d)
	return(d)
}

fullResearchBacklog <- function(){
	old.backlog <- read.csv("./data/HistoricalResearchBacklog.csv")
	old.backlog$Month <- as.Date(old.backlog$Month, "%m/%d/%Y")
	old.backlog$Month <- as.factor(as.yearmon(old.backlog$Month))
	new.backlog <- getResearchBacklog()

	if(as.character(old.backlog$Month[nrow(old.backlog)]) != as.character(new.backlog$Month[nrow(new.backlog)])){
		full.backlog <- rbind(old.backlog, new.backlog)
	} else{full.backlog <- old.backlog}

	full.backlog$Year.Opened <- factor(full.backlog$Year.Opened, levels = c("Opened before 2014", "Opened in 2014", "Opened in 2015"))
	return(full.backlog)
}

plotResearchBacklog <- function(cache = FALSE){
	full.backlog <- fullResearchBacklog()
	pos.1 <- positionLabels(dat = full.backlog$n[1:2], cats = 2)
	pos.2 <- positionLabels(dat = full.backlog$n[3:length(full.backlog$n)], cats = 3)
	full.backlog$pos <- positionLabels(dat = full.backlog$n, cats = 3)

	if(cache == TRUE) {
		write.csv(full.backlog,"./data/HistoricalResearchBacklog.csv")
	}
	p <- barOPA(data = full.backlog, x = "Month", y = "n", title = "Filing Year of Open Cases", fill = "Year.Opened", position = "stack") +
			 geom_text(aes_string(label = "n", y = "pos", color = "Year.Opened"), size = 3) +
			 scale_colour_manual(values = c("grey77", "grey77", "grey30"), guide = FALSE)
	p <- buildChart(p)

	ggsave("./output/Research-Backlog.png", plot = p, width = 7.42, height = 5.75)
}
plotResearchBacklog(cache = FALSE) #set to true to save a version of the backlog with new monthly data
