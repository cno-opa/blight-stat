# hearings.R
#
# Data Sources:
#==========================
#
# https://data.nola.gov/Housing-Land-Use-and-Blight/Code-Enforcement-All-Inspections/uh5a-f7uw -- Total inspections from Socrata SQL query of research
# https://data.nola.gov/Housing-Land-Use-and-Blight/Code-Enforcement-Active-Pipeline/8pqz-ftzc -- Code Enforcement pipeline
#
#==========================
#
#

#TODO: PROFIT

setInternet2(TRUE)

# need to expand window
plotHearingTotals <- function(){
	# pull down data from Socrata
	#hrng.url = "https://data.nola.gov/resource/44ct-56tr?$limit=20000&$order=hearingdate DESC"
	#hrng <- fromJSON(paste0(readLines(hrng.url)))
	hrng <- csvFromWeb("https://data.nola.gov/api/views/44ct-56tr/rows.csv?accessType=DOWNLOAD")

	#download.file("https://data.nola.gov/api/views/44ct-56tr/rows.csv?accessType=DOWNLOAD", "file.csv")
	#hrng <- read.csv("file.csv")

	# process dates and filter for correct range
	hrng$HearingDate <- as.Date(hrng$HearingDate, "%m/%d/%Y")
	proj.date <- round_date(seq(final.date, length = 2, by = "+1 month")[2],"month")-1
	hrng.old <- subset(hrng, HearingDate >= as.Date("2013-01-01") & HearingDate <= final.date)
	hrng.old <- subset(hrng.old, Judgment != "Result Pending")

	hrng.2015 <- subset(hrng.old, HearingDate >= as.Date("2015-01-01"))
	hrng.init <-  subset(hrng.2015, CaseHistory == "Initial Hearing")
	cat("KPI -- Number of properties brought to hearing YTD", nrow(hrng.init), "\n")

	set_kpi <- function() {
		load("./data/kpi.Rdata")
		kpi <- rbind(kpi, c("Number of properties brought to hearing YTD", nrow(hrng.init)) )
		save(kpi, file = "./data/kpi.Rdata")
	}
	set_kpi()

	hrng.proj <- subset(hrng, HearingDate >= final.date & HearingDate <= proj.date)
	hrng <- rbind(hrng.old, hrng.proj)
	hrng$Month <- as.factor(as.yearmon(hrng$HearingDate))
	save(hrng, file = "./data/hearings.RData")

	# summarize by month
	d <- group_by(hrng, Month) %>%
	summarise(n = n())
	#d$labels <- c(rep("", nrow(d)-2), d$n[nrow(d)-1], paste(d$n[nrow(d)], "\n proj."))

	# make plots
	p <- lineOPA(d, "Month", "n", "Number of Hearings", labels = "n", last_label = TRUE, lab.size = 3)
	# geom_segment(aes(x = 1, xend = 12, y = 5000/12, yend = 5000/12), linetype = "dashed", color = "black") +
	# geom_segment(aes(x = 13, xend = nrow(d), y = 4000/12, yend = 4000/12) , linetype = "dashed", color = "black")

	p <- buildChart(p)
	p
	ggsave("./output/Hearing-Totals.png", plot = p, width = 7.42, height = 5.75)
}
plotHearingTotals()


plotHearingResults <- function(){
	load("./data/hearings.RData")
	hrng <- subset(hrng, HearingDate >= as.Date("2013-01-01") & HearingDate <= final.date)

	# set up columns with judgment types
	hrng$Guilty <- (grepl("Guilty", hrng$Judgment, ignore.case = TRUE) & hrng$Judgment != "Guilty: Abated" & hrng$Judgment != "Reset: Conditional Guilty") * 1
	hrng$Violations.Abated <- (grepl("compli", hrng$Judgment, ignore.case = TRUE) | hrng$Judgment == "Guilty: Abated") * 1
	hrng$Work.In.Progress <- (hrng$Judgment == "Reset: Work In Progress" | hrng$Judgment == "Reset: Conditional Guilty") * 1
	hrng$Reset.Reinspection <- (grepl("Inspect",hrng$Judgment, ignore.case = TRUE)) * 1
	hrng$Reset.Notice <- (grepl("Notice",hrng$Judgment, ignore.case = TRUE)) * 1
	hrng$Pending <- (grepl("Pending", hrng$Judgment, ignore.case = TRUE)) * 1
	hrng <- subset(hrng, Pending == 0)
	judgments <- subset(hrng, select = c(Guilty, Violations.Abated, Work.In.Progress, Reset.Reinspection, Reset.Notice, Pending))
	hrng$Reset.Other <- 1-rowSums(judgments)
	save(hrng, file = "./data/hearing-results.RData")

	# get percent hearing results by month (with all undesirable resets as one category)
	d <- group_by(hrng, Month) %>%
	summarise(mean(Guilty), mean(Violations.Abated), mean(Work.In.Progress), mean(Reset.Reinspection), mean(Reset.Notice), mean(Reset.Other))
	names(d) <- c("Month","Guilty","Violations.Abated","Work.In.Progress","Reset.Reinspection","Reset.Notice","Reset.Other")
	resets <- subset(d, select = c(Reset.Reinspection, Reset.Notice, Reset.Other))
	d$Reset.Reinspection <- NULL
	d$Reset.Notice <- NULL
	d$Reset.Other <- NULL
	d$Reset <- rowSums(resets)
	dm <- melt(d, id = "Month")
	dm <- dm[order(dm$Month),]

	dm$pos <- positionLabels(dat = dm$value, cats = 4)
	dm$lab <- paste(round(100*dm$value, 1), "%", sep = "")
	dm$lab[1:(length(dm$lab)-4)] <- rep("", length(dm$lab)-4)

	# make plots
	p <- area100pOPA(dm, x = "Month", y = "value", group = "variable", title = "Hearing Results", legend.labels = c("Guilty","Violations Abated","Work in Progress","Other Reset/Dismissed"), percent = TRUE) +
			 geom_text(aes_string(label = "lab", y = "pos", color = "variable"), size = 3, hjust = 1, vjust = -1) +
			 scale_colour_manual(values = c("grey77", "grey77", "grey30", "grey30"), guide = FALSE)

	p <- buildChart(p)
	ggsave("./output/Hearing-Results.png", plot = p, width = 7.42, height = 5.75)
}
plotHearingResults()

# bar chart version
plotHearingResultsBar <- function() {
	load("./data/hearings.RData")
	hrng <- subset(hrng, HearingDate >= as.Date("2013-01-01") & HearingDate <= final.date)

	# set up columns with judgment types
	hrng$Guilty <- (grepl("Guilty", hrng$Judgment, ignore.case = TRUE) & hrng$Judgment!="Guilty: Abated" & hrng$Judgment!="Reset: Conditional Guilty") * 1
	hrng$Violations.Abated <- (grepl("compli", hrng$Judgment, ignore.case = TRUE) | hrng$Judgment=="Guilty: Abated") * 1
	hrng$Work.In.Progress <- (hrng$Judgment=="Reset: Work In Progress" | hrng$Judgment=="Reset: Conditional Guilty") * 1
	hrng$Reset.Reinspection <- (grepl("Inspect",hrng$Judgment, ignore.case = TRUE)) * 1
	hrng$Reset.Notice <- (grepl("Notice",hrng$Judgment, ignore.case = TRUE)) * 1
	hrng$Pending <- (grepl("Pending", hrng$Judgment, ignore.case = TRUE)) * 1
	hrng <- subset(hrng, Pending == 0)
	judgments <- subset(hrng, select = c(Guilty, Violations.Abated, Work.In.Progress, Reset.Reinspection, Reset.Notice, Pending))
	hrng$Reset.Other <- 1-rowSums(judgments)
	save(hrng, file = "./data/hearing-results.RData")

	# get percent hearing results by month (with all undesirable resets as one category)
	d <- group_by(hrng, Month) %>%
	summarise(sum(Guilty), sum(Violations.Abated), sum(Work.In.Progress), sum(Reset.Reinspection), sum(Reset.Notice), sum(Reset.Other))
	names(d) <- c("Month","Guilty","Violations.Abated","Work.In.Progress","Reset.Reinspection","Reset.Notice","Reset.Other")
	resets <- subset(d, select = c(Reset.Reinspection, Reset.Notice, Reset.Other))
	d$Reset.Reinspection <- NULL
	d$Reset.Notice <- NULL
	d$Reset.Other <- NULL
	d$Reset <- rowSums(resets)
	dm <- melt(d, id = "Month")

	# make plots
	p <- barOPA(data = dm, x = "Month", y = "value", title = "Hearing Results", fill = "variable", position = "stack", legend.labels = c("Guilty","Violations Abated","Work in Progress","Other Reset/Dismissed"))
	p <- buildChart(p)

	ggsave("./output/Hearing-Results-Bar.png", plot = p, width = 7.42, height = 5.75)
}
plotHearingResultsBar()

plotHearingResets <- function() {
	# group by reset type
	load("./data/hearing-results.RData")
	hrng.2015 <- subset(hrng, HearingDate >= as.Date("2015-01-01"))
	reinsp.tot <- nrow(subset(hrng.2015, Reset.Reinspection == 1))/nrow(hrng.2015)
	notice.tot <- nrow(subset(hrng.2015, Reset.Notice == 1))/nrow(hrng.2015)
	cat("KPI -- Percent of cases reset due to no reinspection: ", reinsp.tot, "\n")
	cat("KPI -- Percent of cases reset due to no notice: ", notice.tot, "\n")

	set_kpi <- function() {
		load("./data/kpi.Rdata")
		kpi <- rbind(kpi, c("Percent of hearings reset due to failure to reinspect the property YTD", reinsp.tot ))
		kpi <- rbind(kpi, c("Percent of hearings reset due to failure to properly notify the owner YTD", notice.tot ))
		save(kpi, file = "./data/kpi.Rdata")
	}

	d <- group_by(hrng, Month) %>%
	summarise(mean(Reset.Reinspection), mean(Reset.Notice), mean(Reset.Other))
	names(d) <- c("Month","Reset.Reinspection","Reset.Notice","Reset.Other")
	dm <- melt(d, id = "Month")
	dm <- dm[order(dm$Month),]
	dm$pos <- positionLabels(dat = dm$value, cats = 3)
	dm$lab <- paste(round(100*dm$value, 1), "%", sep = "")
	dm$lab[which(dm$lab == "0%")] <- ""

	fill <- c("tomato", "tomato3", lightBlue)
	#make plots
	p <- barOPA(data = dm, x = "Month", y = "value", title = "Percent of Cases Reset", fill = "variable", percent = TRUE, position = "stack", legend.labels = c("No Resinspection","Insufficient Notice","Others (External Factors)")) +
			 scale_fill_manual(values = fill, labels = c("No Resinspection","Insufficient Notice","Others (External Factors)"))
			#  geom_text(aes_string(label = "lab", y = "pos", color = "variable"), size = 2.5) +
			#  scale_colour_manual(values = c("grey30", "grey30", "grey30"), guide = FALSE)
	p <- buildChart(p)

	ggsave("./output/Hearing-Resets.png", plot = p, width = 7.42, height = 5.75)
}
plotHearingResets()

plotAbatement <- function(){ #Lien Waivers csv must be updated(even if 0)
	load("./data/hearing-results.RData")

	d <- group_by(hrng, Month) %>% summarise(Violations.Abated = sum(Violations.Abated))
	lien.waivers <- read.csv("./data/LienWaivers.csv")
	d <- rbind(d,d)
	d[((nrow(d)/2)+1):nrow(d),2] <- lien.waivers$Approved.Lien.Waivers
	d$variable <- c(rep("Abated.At.Hearing", nrow(d)/2), rep("Approved.Lien.Waivers", nrow(d)/2))
	names(d) <- c("Month", "value", "variable")

	abate.2015 <- d[grepl("2015", d$Month),]
	cat("KPI -- Number of blighted properties brought into compliance by owners: ", sum(abate.2015$value), "\n")

	set_kpi <- function() {
		load("./data/kpi.Rdata")
		kpi <- rbind(kpi, c("Number of blighted properties brought into compliance by owners", sum(abate.2015$value)) )
		save(kpi, file = "./data/kpi.Rdata")
	}
	set_kpi()

	# make plots
	d <- d[order(d$Month),]
	d$pos <- positionLabels(dat = d$value, cats = 2)
	d$lab <- d$value
	d$lab[which(d$lab == 0)] <- ""

	p <- barOPA(data = d, x = "Month", y = "value", title = "Voluntary Abatement", fill = "variable", position = "stack", legend.labels =c("Abated at Hearing","Approved Lien Waivers")) +
			 geom_hline(yintercept = 750/12, linetype = "dashed") +
			 geom_text(aes_string(label = "lab", y = "pos", color = "variable"), size = 3) +
			 scale_colour_manual(values = c("grey77", "grey30"), guide = FALSE)

	p <- buildChart(p)

	ggsave("./output/Abatement-Totals.png", plot = p, width = 7.42, height = 5.75)
}
plotAbatement()
