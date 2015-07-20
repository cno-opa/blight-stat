# abatement.R
#
# Data Sources:
# ==========================
#
# LAMA SQL query "05-Hearing Details-LAMA Comprehensive Report"
# LAMA SQL query "Abatement Decision Tool"
# LAMA SQL query "CE Administrative Reviews"
# "JudgmentsForAbatementReview.csv"
# "SheriffSaleCollections.csv"
# "Legal-Review.csv"
# Socrata demolitions dataset
# "CNAP-totals.csv"
# "CG66-GEOPINS.csv"
#
# ==========================
#

#TODO: PROFIT

setInternet2(TRUE)

# defining projections
NOLA.proj <- CRS("+proj=lcc +lat_1=29.3 +lat_2=30.7 +lat_0=28.5 +lon_0=-91.33333333333333 +x_0=999999.9999999999 +y_0=0 +datum=NAD83 +units=us-ft +no_defs +ellps=GRS80 +towgs84=0,0,0")
longlat <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

getAbatementReview <- function(){
	# finds outcomes of all Code Enforcement judgments (closed, reviewed, needing reviews).

	# read in and clean data
	judgments <- read.csv("./data/JudgmentsForAbatementReview.csv")

	#judgments <- csvFromWeb("https://data.nola.gov/api/views/44ct-56tr/rows.csv?accessType=DOWNLOAD") ###new
	judgments <- subset(judgments, Judgment == "Guilty")
	judgments$HearingDate <- as.Date(judgments$HearingDate,"%m/%d/%Y")
	judgments$Month <- as.factor(as.yearmon(judgments$HearingDate))

	abatement.tool <- read.csv("./data/AbatementTool.csv")
	admin.reviews <- read.csv("./data/AdminReviews.csv")
	names(abatement.tool)[which(names(abatement.tool) == "CaseID")] <- "CaseNumber" #change SQL script and these won't be necessary
	names(abatement.tool)[which(names(abatement.tool) == "Recommendation")] <- "AbatementRecommendation"
	names(admin.reviews)[which(names(admin.reviews) == "NumString")] <- "CaseNumber"
	names(admin.reviews)[which(names(admin.reviews) == "Status")] <- "ReviewRecommendation"
	abatement.tool <- subset(abatement.tool, AbatementRecommendation %in% c("Clear Lot", "Complete", "Demolish", "Sell"))
	admin.reviews <- subset(admin.reviews, ReviewRecommendation %in% c("Clear Lot", "Demolish", "Sell", "Hold", "On Hold"))
	abatement.tool$AbatementRecommendation <- as.character(abatement.tool$AbatementRecommendation)
	admin.reviews$ReviewRecommendation <- as.character(admin.reviews$ReviewRecommendation)
	abatement.tool$AbatementRecommendation[which(as.character(abatement.tool$AbatementRecommendation) %in% "Complete")] <- "Occupied"
	admin.reviews$ReviewRecommendation[which(admin.reviews$ReviewRecommendation %in% c("Hold", "On Hold", "Clear Lot"))] <- "Sell"

	# categorize closed cases
	#closed.paid <- subset(judgments, O.C=="Closed" & UnpaidFees == 0)
	#closed.unpaid <- subset(judgments, O.C=="Closed" & UnpaidFees > 0)
	closed <- subset(judgments, O.C == "Closed")

	# categorize open cases
	open <- subset(judgments, O.C == "Open")
	judgments.reviews.1 <- merge(x = open, y = abatement.tool, all.x = TRUE, by = "CaseNumber")
	judgments.reviews <- merge(x = judgments.reviews.1, y = admin.reviews, all.x = TRUE, by = "CaseNumber")
	#judgments.reviews <- subset(judgments.reviews, select=c(CaseNumber, StreetAddress, HearingDate, Month, AbatementRecommendation, ReviewRecommendation, XPos, YPos, GeoPIN))
	needs.review.index <- is.na(judgments.reviews$AbatementRecommendation) & is.na(judgments.reviews$ReviewRecommendation)
	needs.review <- judgments.reviews[needs.review.index,]
	has.review <- judgments.reviews[!needs.review.index,]
	FinalRecommendation <- c()
	for(i in 1:nrow(has.review)) {
		if(has.review$AbatementRecommendation[i] == "Occupied" & is.na(has.review$ReviewRecommendation[i])) {
			FinalRecommendation[i] <- "Occupied"
		} else if(is.na(has.review$AbatementRecommendation[i]) | has.review$AbatementRecommendation[i] == "Occupied") {
			FinalRecommendation[i] <- has.review$ReviewRecommendation[i]
		} else {
			FinalRecommendation[i] <- has.review$AbatementRecommendation[i]
		}
	}
	has.review$FinalRecommendation <- FinalRecommendation
	has.review$Demolish <- (grepl("Demolish", has.review$FinalRecommendation)) * 1
	has.review$Sell <- (grepl("Sell", has.review$FinalRecommendation)) * 1
	has.review$Occupied <- (grepl("Occupied", has.review$FinalRecommendation)) * 1
	save(has.review, file = "./output/Judgments-With-Reviews.RData")

	# put together all of the categories
	#d.cp <- group_by(closed.paid, Month) %>% summarise(Closed.Paid = n())
	#d.cu <- group_by(closed.unpaid, Month) %>% summarise(Closed.Unpaid = n())
	d.nr <- group_by(needs.review, Month) %>%
					summarise(Needs.Review = n())
	d.hr <- group_by(has.review, Month) %>%
					summarise(Demolish = sum(Demolish), Sell = sum(Sell), Occupied = sum(Occupied))
	#return(list(d.cp = d.cp, d.cu = d.cu, d.nr = d.nr, d.hr = d.hr))
	return(list(d.nr = d.nr, d.hr = d.hr))
}

plotReviewProgress <- function(cache = FALSE){
	# plots review status of all cases within the relevant range (each month's bar is a snapshot from the hearings for that month's 6-month range)
	# to save data, use cache = TRUE

	# get counts from current month
	status <- getAbatementReview()
	d.hr <- data.frame(Month = status$d.hr$Month, Has.Review = rowSums(status$d.hr[,2:ncol(status$d.hr)]))
	d.hr <- d.hr[,c("Month", "Has.Review")]
	d <- merge(x = status$d.cp, y = status$d.cu, all = TRUE)
	d <- merge(x = d, y = d.hr, all = TRUE)
	d <- merge(x = d, y = status$d.nr, all = TRUE)
	d[is.na(d)] <- 0
	Counts <- colSums(d[,(2:ncol(d))], )
	Status <- names(Counts)
	month <- rep(final.date, length(status))
	month <- as.factor(as.yearmon(month))
	d.tot <- data.frame(cbind(Status, Counts))
	d.tot$Counts <- as.numeric(as.character(d.tot$Counts))
	d.tot$Month <- month

	# get/append historical data
	#d.tot.hist <- read.csv("./data/Abatement-Review-History.csv")
	#d.tot.hist$Month <- as.Date(d.tot.hist$Month, "%m/%d/%Y")
	#d.tot.hist$Month <- as.factor(as.yearmon(d.tot.hist$Month))
	#d.tot <- rbind(d.tot.hist, d.tot)
	#rownames(d.tot) <- 1:nrow(d.tot)
	#if(cache == TRUE){write.csv(d.tot, "./data/Abatement-Review-History.csv")}

	d.tot <- status$d.nr
	d.tot <- d.tot[grepl("2015", d.tot$Month),]
	d.tot <- d.tot[1:(nrow(d.tot)-2),] # CE generally doesnt begin looking at cases for two months after the judgment
	# make plot
	#d.tot$pos <- positionLabels(dat = d.tot$Counts, cats = 4)
	#d.tot$lab <- d.tot$Counts
	#d.tot$lab[which(d.tot$lab < 5)] <- ""

	#fill <- c(lightBlue, colorRampPalette(c(lightBlue, darkBlue))(3)[2], darkBlue, "firebrick")
	#p <- barOPA(data = d.tot, x = "Month", y = "Counts", title = "Outcome of Guilty Judgments", fill="Status", position="stack",
	#legend.labels=c("Closed-Paid","Closed-Unpaid","Has Review","Needs Review"))
	#p <- p + scale_fill_manual(values = fill, labels = c("Closed-Paid","Closed-Unpaid","Has Review","Needs Review"))
	#p <- p + geom_text(aes_string(label = "lab", y = "pos", color = "Status"), size = 3) +
	#scale_colour_manual(values = c("grey30", "grey30", "grey77", "grey77"), guide = FALSE)
	#p <- buildChart(p)
	#p

	d.tot$group <- rep("l", nrow(d.tot))
	fill <- rep("firebrick", nrow(d.tot))
	p <- barOPA(data = d.tot, x = "Month", y = "Needs.Review", title = "Number of Guilty Judgments Awaiting Abatement Reviews", fill = "group") +
		   scale_fill_manual(values = fill, guide = FALSE) +
			 geom_text(aes_string(label = "Needs.Review"), size = 4.5, vjust = 1.8, col = "grey77")
	p <- buildChart(p)
	p

	ggsave("./output/Abatement-Review-Progress.png", plot = p, width = 7.42, height = 5.75)
}
plotReviewProgress(cache = FALSE)

plotReviewBacklog <- function() {
	d <- read.csv("./data/judgments-awaiting-review-backlog.csv")
	d$month <- as.factor(as.yearmon(d$month))

	# redundancy
	write.csv(d, paste0("./data/judgments-awaiting-review-backlog-", format(Sys.time(), "%Y-%m-%d"), ".csv"))

	# check if r_period data is in d
	if(dateFromYearMon(r_period) > dateFromYearMon(d$month[nrow(d)])) {
		s <- getAbatementReview()$d.nr
		m <- s$Month[nrow(s)]
		b <- sum(s$Needs.Review[which(s$Month == "Jan 2015"):(nrow(s) - 2)])
		x <- data.frame(month = m, backlog = b)
		d <- rbind(d, x)
	}

	p <- lineOPA(d, "month", "backlog", "Historical view of guilty judgment backlog", labels = "backlog")
	p <- buildChart(p)
	ggsave("./output/Abatement-Review-Backlog-Hist.png", plot = p, width = 7.42, height = 5.75)
	write.csv(d, "./data/judgments-awaiting-backlog.csv", row.names = FALSE)
}
plotReviewBacklog()

plotReviewOutcomes <- function(){
	# plots review decisions for all cases that have been reviewed in the relevant range

	d.hr <- getAbatementReview()$d.hr
	# review outcomes (need to fix aesthetics)
	d.hr.agg <- colSums(d.hr[,2:ncol(d.hr)])
	d.hr.agg <- as.data.frame(d.hr.agg)
	d.hr.agg$status <- factor(rownames(d.hr.agg), levels = c("Occupied", "Demolish", "Sell"))
	names(d.hr.agg)[1] <- "count"
	p <- ggplot(d.hr.agg, aes(x = factor(status), y = count)) +
	geom_bar(stat = "identity", fill = darkBlue)+
	coord_flip() +
	geom_text(aes_string(label = "count"), size = 3.5, hjust = 1.2, col = "grey77") +
	labs(title = "Outcome of Reviews", x = NULL, y = NULL)
	p
	ggsave("./output/Abatement-Review-Outcomes.png", plot = p, width = 7.42, height = 5.75)
}
plotReviewOutcomes()


plotSaleReview <- function(){
	# plots the legal review outcome for properties flagged for sale
	# for now we have to manually update the Legal-Review.csv spreadsheet, with data sent by Code Enforcement
	# data is starting to be tracked in LAMA, so in the future it can be pulled from there

	# get and process data
	review <- read.csv("./data/Legal-Review.csv")
	review$Month <- as.Date(review$Month, "%m/%d/%Y")
	review$Month <- as.factor(as.yearmon(review$Month))
	cols <- grepl("Sales", names(review)) | grepl("Month", names(review))
	sale <- review[, cols]
	sale.melt <- melt(sale, id = "Month")
	sale.melt[is.na(sale.melt)] <- 0

	# make chart
	sale.melt <- sale.melt[order(sale.melt$Month),]
	sale.melt$pos <- positionLabels(dat = sale.melt$value, cats = 5)
	sale.melt$lab <- sale.melt$value
	sale.melt$lab[which(sale.melt$lab == 0)] <- ""

	tomatos <- colorRampPalette(c("tomato", "tomato4"))
	grays <- colorRampPalette(c("grey79", "grey48"))
	fill <- c(darkBlue, grays(2), tomatos(2))
	p <- barOPA(data = sale.melt, x = "Month", y = "value", title = "Legal Review Outcomes-Sheriff Sales", fill = "variable", position = "stack")
	p <- p + scale_fill_manual(values = fill, labels = c("Accepted","External Factors", "Miscellaneous","Procedural Deficiency", "Title Research"))
	p <- p + geom_text(aes_string(label = "lab", y = "pos", color = "variable"), size = 3) +
	scale_colour_manual(values = c("grey77", "grey30", "grey30", "grey30", "grey77"), guide = FALSE)
	p <- buildChart(p)
	p
	ggsave("./output/Abatement-Law-Review-Sales.png", plot = p, width = 7.42, height = 5.75)
}
plotSaleReview()

plotDemoReview <- function(){
	# plots the legal review outcome for properties flagged for demolition
	# for now we have to manually update the Legal-Review.csv spreadsheet, with data sent by Code Enforcement
	# data is starting to be tracked in LAMA, so in the future it can be pulled from there

	# get and process data
	review <- read.csv("./data/Legal-Review.csv")
	review$Month <- as.Date(review$Month,  "%m/%d/%Y")
	review <- subset(review, Month >= as.Date("2015-01-01"))
	review$Month <- as.factor(as.yearmon(review$Month))
	cols <- grepl("Demo", names(review)) | grepl("Month", names(review))
	sale <- review[, cols]
	sale.melt <- melt(sale, id = "Month")
	sale.melt[is.na(sale.melt)] <- 0

	# make chart
	sale.melt <- sale.melt[order(sale.melt$Month),]
	sale.melt$pos <- positionLabels(dat = sale.melt$value, cats = 5)
	sale.melt$lab <- sale.melt$value
	sale.melt$lab[which(sale.melt$lab == 0)] <- ""

	grays <- colorRampPalette(c("grey79", "grey48"))
	tomatos <- colorRampPalette(c("tomato", "tomato3"))
	fill <- c(darkBlue, grays(2), tomatos(2))
	p <- barOPA(data = sale.melt, x = "Month", y = "value", title = "Legal Review Outcomes-Demolitions", fill = "variable", position = "stack")
	p <- p + scale_fill_manual(values = fill, labels = c("Accepted","External Factors", "Miscellaneous","Procedural Deficiency", "Title Research"))
	p <- p + geom_text(aes_string(label = "lab", y = "pos", color = "variable"), size = 3) +
	scale_colour_manual(values = c("grey77", "grey30", "grey30", "grey30", "grey77"), guide = FALSE)
	p <- buildChart(p)
	p
	ggsave("./output/Abatement-Law-Review-Demos.png", plot = p, width = 7.42, height = 5.75)
}
plotDemoReview()

plotSSCollections <- function(){
	# plots the collections from sheriff sales
	# we have to manually update the Legal-Review.csv spreadsheet, with data sent by Code Enforcement

	coll <- read.csv("./data/SheriffSaleCollections.csv")
	coll$Month <- as.Date(coll$Month,"%m/%d/%Y")
	coll$Month <- as.factor(as.yearmon(coll$Month))
	m <- melt(coll, id="Month")
	m[is.na(m)] <- 0

	# make chart
	m <- m[order(m$Month),]
	m$pos <- positionLabels(dat = m$value, cats = 3)
	m$lab <- m$value
	m$lab <- m$lab/1000
	m$lab <- paste0("$", round(m$lab, 1), "K")
	m$lab[which(m$lab == "$0")] <- ""

	p <- barOPA(data = m, x = "Month", y = "value", currency = TRUE, title = "Amount of Collections from Sheriff Sales", fill = "variable", position = "stack", legend.labels= c ("Taxes from Sales", "Liens from Sale", "Liens without Sale")) +
 			 geom_text(aes_string(label = "lab", y = "pos", color = "variable"), size = 2) +
			 scale_colour_manual(values = c("grey77", "grey30", "grey30"), guide = FALSE)
	p <- buildChart(p)

	ggsave("./output/Sale-Collections.png", plot = p, width = 7.42, height = 5.75)
}
plotSSCollections()

getDemos <- function(){
	# gets and cleans demos from data.nola.gov
	# we are responsible for updating and maintaining the spreadsheet that this pulls from
	# to update the sheet, run Demos-for-Socrata.r (that file has full instructions)

	demo.url <- "http://data.nola.gov/resource/e3wd-h7q2.json?$limit=20000"
	demo <- fromJSON(paste0(readLines(demo.url)))
	demo$lon <- as.numeric(demo$location_1$longitude)
	demo$lat <- as.numeric(demo$location_1$latitude)
	demo$location_1 <- NULL

	program.pretty <- c()
	for(i in 1:nrow(demo)){
		if(demo$program[i]=="FEMA"){program.pretty[i] <- "FEMA"
		} else
		if(demo$program[i]=="IDC"){program.pretty[i] <- "Imminent Danger of Collapse"
		} else
		if(demo$program[i]%in% c("NOSD", "SDER", "SDER (NOSD)")){program.pretty[i] <- "Strategic Demolition"
		} else{program.pretty[i] <- "NORA"}
	}

	demo$program.pretty <- program.pretty
	demo$demolition_start <- as.Date(demo$demolition_start)

	return(demo)
}

mapDemosTot <- function(){
	# makes a choropleth map with total demolitions by block group

	demos <- getDemos()
	demos <- toSpatialPoints(demos, X = "lon", Y = "lat", remove.outliers = TRUE)

	cat("Since 2010 there have been: ", sum(as.numeric(as.character(demos$units)), na.rm = TRUE), "units demolished. \n")

	bg <- getBGs()
	bg <- countWithin(within = demos, around = bg, return.type = "poly")
	p <- mapOPAPoly(geom = bg, style = "within", breaks = c(-1, 0, 5, 10, 20), fill = rev(heat.colors(5)), labs = c("0", "1 - 5", "6 - 10", "11 - 20", "Greater than 20"), title = "Number of Demolitions Since 2010")
	ggsave("./output/Demos-Total.png", plot = p, width = 7.42, height = 5.75)
}
mapDemosTot()

mapDemos2015 <- function(){
	# maps demolitions by type for 2015 (will have to update script in 2016 if we're still using this map)

	demos <- getDemos()
	demos.15 <- subset(demos, demolition_start >= as.Date("2015-01-01"))

	test <- unlist(demos.15["program.pretty"])

	program.count <- table(demos.15$program.pretty)
	program.pretty <- demos.15$program.pretty
	for(i in 1:length(program.pretty)) {
		count.ind <- which(names(program.count) == program.pretty[i])
		program.pretty[i] <- paste0(program.pretty[i], " (", program.count[count.ind],")")
	}
	demos.15$program.pretty <- program.pretty

	cat("KPI -- Total Units Demolished in 2015: ", sum(as.numeric(as.character(demos.15$units)), na.rm = TRUE), "\n")
	cat("KPI -- Total Properties Demolished in 2015: ", nrow(demos.15), "\n")

	set_kpi <- function() {
		load("./data/kpi.Rdata")
		kpi <- rbind(kpi, c("Number of units demolished YTD", sum(as.numeric(as.character(demos.15$units)), na.rm = TRUE)) )
		save(kpi, file = "./data/kpi.Rdata")
	}
	set_kpi()

	new.month <- seq(final.date, length = 2, by = "-1 month")[2]
	demo.old <- subset(demos.15,demolition_start < new.month | is.na(demolition_start))
	demo.new <- subset(demos.15, demolition_start >= new.month)
	demo.old <- toSpatialPoints(demo.old, X = "lon", Y = "lat", remove.outliers = TRUE)
	demo.new <- toSpatialPoints(demo.new, X = "lon", Y = "lat", remove.outliers = FALSE)
	fill <- c("darkorchid", "darkorange2", "dodgerblue", "chartreuse3")
	new <- mapOPAPoints(pts = demo.new, X = "lon", Y = "lat",style = "program.pretty",fill = fill, size = 4)
	p <- mapOPAPoints(pts = demo.old, X = "lon", Y = "lat", style = "program.pretty", fill = fill, old.map = new, size = 2, title = "Properties Demolished in 2015") +
			 guides(fill = guide_legend(nrow = 2,byrow = TRUE))

	ggsave("./output/Demos-2015.png", plot = p,  width = 7.42, height = 5.75)
}
mapDemos2015()

plotCNAP <- function(){
	cnap <- read.csv("./data/CNAP-Totals.csv")
	cnap$Month <- as.Date(cnap$Month,"%m/%d/%Y")
	cnap$Month <- as.factor(as.yearmon(cnap$Month))

	# make plots
	p <- lineOPA(cnap, "Month", "CNAP", "Properties Receiving Routine Maintenance Through CNAP", labels = "CNAP", last_label = TRUE)
	p <- buildChart(p)
	ggsave("./output/CNAP-Totals.png", plot = p,  width = 7.42, height = 5.75)
}
plotCNAP()

mapLotClearing <- function(){
	# maps lot clearing from NORA, Ch. 66 and CNAP
	# CNAP still needs to be added (working on getting a reliable feed from data.nola.gov)

	# get NORA data
	nora <- csvFromWeb(file.source="https://data.nola.gov/api/views/5ktx-e9wc/rows.csv?accessType=DOWNLOAD")
	geopin <- as.numeric(as.character(nora$GEOPIN))
	program <- rep("NORA Inventory", nrow(nora))
	nora <- data.frame(geopin, program)

	# get ch 66 data
	#ch66 <- csvFromWeb("https://data.nola.gov/api/views/5duy-3c6h/rows.csv?accessType=DOWNLOAD")
	ch66 <- read.csv("./data/Ch66-GEOPINS.csv")
	ch66 <- subset(ch66, STATUS == "COMPLIANT" | STATUS == "REMOVED" | STATUS == "RMC")
	geopin <- as.numeric(ch66$GEOPIN)
	program <- rep("Chapter 66", nrow(ch66))
	ch66 <- data.frame(geopin, program)

	# get CNAP data (change this to download from web when available)
	CNAP <- readOGR(dsn = "./data", layer = "CNAP")
	parcels <- getParcels()
	# get GEOPINs and Council Districts
	CNAP.geopin <- over(geometry(CNAP), parcels)
	geopin <- as.numeric(CNAP.geopin$GEOPIN)
	program <- rep("CNAP", nrow(CNAP))
	CNAP <- data.frame(geopin, program)

	# put it together and make a map
	maintenance <- rbind(ch66, nora, CNAP)
	dupe.geopins <- duplicated(maintenance$geopin)
	maintenance <- maintenance[!dupe.geopins,]
	program.counts <- legendCounts(maintenance$program)
	maintenance$program <- program.counts


	cat("Total lots maintained: ", nrow(maintenance), "\n")
	p <- mapOPAPoly(geom = "parcels", poly.dat = maintenance, id.var = "geopin", style = "program", fill = c("purple","green", "orangered2"), title = "Lot Maintenance in New Orleans")
	ggsave("./output/Lot-Clearing-Map.png", plot = p,  width = 7.42, height = 5.75)
}
mapLotClearing()

mapSales <- function(){
	# maps sales since 2010

	sales <- readOGR("./data", "Sales") #need to geocode sales in ArcMap
	sales$Sold <- gsub("Yes", "Sold", sales$Sold)
	sales$Sold <- gsub("No", "Not Sold", sales$Sold)
	sales$Sold <- factor(sales$Sold, levels=c("Sold", "Not Sold")) #adjust order if plotting incorrectly
	new.sales <- subset(sales, New == "Yes")
	old.sales <- subset(sales, New == "No")
	new <- mapOPAPoints(new.sales, X = "X", Y = "Y", style = "Sold", fill = c("red", "green"), size = 4, location=c(-90.030368, 29.985083))
	p <- mapOPAPoints(old.sales, title = "Sheriff Sale Results Since 2010", X = "X", Y = "Y", style = "Sold", fill = c("red", "green"), size = 2, location=c(-90.030368, 29.985083), old.map = new)
	ggsave("./output/Sales-Map.png", plot = p,  width = 7.42, height = 5.75)
}
mapSales()
