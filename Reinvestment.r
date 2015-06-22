## Reinvestment.r

#must run plotters.r
#must run Spatial-OPA.r

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
require("reshape2")
library(maps)
library(maptools)
library(sp)
library(rgeos)
library(RColorBrewer)
library(classInt)
library(rgdal)
library(ggmap)
theme_set(theme_opa())

mapNORASales <- function(final.date){
	#Get and process data
	nora.sold <- csvFromWeb("https://data.nola.gov/api/views/hpm5-48nj/rows.csv?accessType=DOWNLOAD")
	
	##link not working for some reason, which makes csvFromWeb not work
	##the data that is returned only goes through the end of April
	##if it continues not working, download manually and save as NORA_Sold_Properties.csv then use this code
	#nora.sold <- read.csv("Data\\NORA_Sold_Properties.csv")
	#nora.sold.url = "https://data.nola.gov/resource/hpm5-48nj.json"
	#nora.sold <- fromJSON(paste0(readLines(nora.sold.url)))
	
	#nora.sold$Sale.Date=as.Date(nora.sold$Sale.Date, "%m/%d/%Y") ## use this line if pulling from csv
	nora.sold$Sale.Date=as.Date(nora.sold$Sale.Date, "%m/%d/%y") ##use this line if pulling directly from the web...bizarrely there are some differences between the two formats
	nora.sold <- subset(nora.sold, Sale.Date > as.Date("2010-01-01"))
	nora.2015 <- subset(nora.sold, Sale.Date >= as.Date("2015-01-01"))
	cat("Number of sold properties in 2015:", nrow(nora.2015), "\n")

	#Divide last month of sales
	program.counts <- legendCounts(nora.2015$Disposition.Channel)
	nora.2015$Disposition.Channel <- program.counts
	new.month <- seq(final.date, length = 2, by = "-1 month")[2]
	nora.old <- subset(nora.2015, Sale.Date < new.month)
	nora.new <- subset(nora.2015, Sale.Date >= new.month)
	
	#Make into spatial objects and plot
	nora.old.sp <- geopinsToPoints(nora.old, geopin.col="Geopin")
	nora.new.sp <- geopinsToPoints(nora.new, geopin.col="Geopin")
	
	cols <- c("firebrick2", "blue", "green", "orange")
	new <- mapOPApoints(pts = nora.new.sp, X = "X", Y = "Y", style = "Disposition.Channel", fill = cols, size = 4)
	p <- mapOPApoints(pts = nora.old.sp, X = "X", Y = "Y", style = "Disposition.Channel", fill = cols, size = 2, old.map = new, title = "NORA Sold Properties in 2015")
	ggsave("Final/NORA-Sales.png", plot = p, width = 7.42, height = 5.75)
}
mapNORASales(final.date=as.Date("2015-05-30"))

mapSalesInv <- function(){
	nora.sold <- csvFromWeb("https://data.nola.gov/api/views/hpm5-48nj/rows.csv?accessType=DOWNLOAD")
	geopin <- as.numeric(as.character(nora.sold$Geopin))
	status <- rep("Sold Properties", nrow(nora.sold))
	nora.sold <- data.frame(geopin, status)
	
	nora.inv <- csvFromWeb(file.source="https://data.nola.gov/api/views/5ktx-e9wc/rows.csv?accessType=DOWNLOAD")
	geopin <- as.numeric(as.character(nora.inv$GEOPIN))
	status <- rep("Remaining Inventory", nrow(nora.inv))
	nora.inv <- data.frame(geopin, status)

	nora <- rbind(nora.sold, nora.inv)
	nora.dupes <- duplicated(nora$geopin)
	nora <- nora[!nora.dupes,]
	
	program.counts <- legendCounts(nora$status)
	nora$status <- program.counts
	
	p <- mapOPApoly(geom = "parcels", poly.dat = nora, id.var = "geopin", style = "status", fill = c("slateblue", "orangered"), title = "NORA Sales since 2010 and \n  Remaining Inventory")
	ggsave("Final/NORA-Sales-and-Inventory.png", plot = p, width = 7.42, height = 5.75)
}
mapSalesInv()

getOCD <- function(){
	ocd <- read.csv("Data\\OCD-Data.csv")
	keep <- seq(1, nrow(ocd), by=2)
	ocd <- ocd[keep,]
	names(ocd)[1] <- "Program"
	ocd <- ocd[,1:5]
	ocd <- melt(ocd, id="Program")
	return(ocd)
}

plotProgram <- function(program){
	ocd <- getOCD()
	program.df <- subset(ocd, Program == program)
	program.df <- subset(program.df, variable != "Units.Under.Contract")
	
	program.df$variable <- gsub("Units.in.Pre.Development", "Pre-Development", program.df$variable)
	program.df$variable <- gsub("Units.Under.Development", "Under Development", program.df$variable)
	program.df$variable <- gsub("Units.Completed.2015", "Completed", program.df$variable)
	program.df$variable <- factor(program.df$variable, levels = c("Pre-Development", "Under Development", "Completed"))
	
	p <- barOPA(data=program.df, x="variable", y="value", title=program, fill = "variable") + 
	scale_fill_manual(values = c("red", "yellow", "green"))+
	geom_text(aes_string(label = "value"), size = 3.5, vjust = 1.8, col = "grey30") 

	ggsave(paste0("Final/OCD-", program,".png"), plot = p, width = 7.42, height = 5.75)
}

plotProgram(program="Owner-Occupied Rehabilitation Program")
plotProgram(program="Homeownership Development Program")
plotProgram(program="Rental Housing Program")
