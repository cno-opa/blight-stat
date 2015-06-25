#### Code to get data into a useable form on Socrata.

### Steps before running this script:
##  1. Get all demo spreadsheets from demolition contacts. Contacts are:
# Strategic Demolitions (SDER): Ariane Patton - ajpatton@nola.gov
# Imminent Danger of Collapse (IDC): John Ovella - jrovella@durrhc.com
# FEMA: Chelsea Hart - clhart@nola.gov, can also get data from William McGowen-william.mcgowen@tetratech.com
# or Jerry Assam - jerry.assam@tetratech.com
# NORA: Jonathan Rouege - jrouege@lalandtrust.us
# Demo contacts are all on the standard BlightSTAT data request that goes out on a monthly basis

##  2. Process data in Excel and combine into one sheet. The data all comes in slightly different forms
## (and the form is not always consistent month-to-month), so it's useful to put it in a single csv file with
## the following headings: address, start date, end date, units, program (they don't have to have those exact names
## or be in that order, but all of that data will go on Socrata)

##  3. Add a column with a cleaned address for geocoding. This should remove dashes and other special characters,
##  and abbreviated street names should be spelled out. eg: 1000-02 MLK Dr should be 1000 Martin Luther King Junior Dr

##  4. Geocode the addresses in ArcCatalog by right clicking and selecting "Geocode Addresses."
##  Choose "Browse for Locator" and navigate to the CNO_Composite_APC locator, which is in
##  O:\GIS\Socrata_GIS_Data\GIS_Locators_20140117

##  For more info on steps 3 and 4, see the ArcGIS help page
##  http://help.arcgis.com/en/arcgisdesktop/10.0/help/index.html#//002500000026000000.htm
##  The enterprise GIS team can also provide guidance on geocoding
## (it's pretty straightforward once you've done it a couple of times)

## 5. Save the results as a shapefile and run the following script on the result:

# setwd("O:\\Projects\\BlightStat\\2015\\04-April - Copy\\Source Data\\GIS") #working directory
setInternet2(TRUE)

# Load data
#demo.name <- "Demos-May-SDER" #shapefile name with no extensions
demos <- readOGR("./data", "DemosApril")
council <- getCouncil()
parcels <- getParcels()

# Get GEOPINs and Council Districts
demos.geopin <- over(geometry(demos), parcels)
GEOPIN <- demos.geopin$GEOPIN
demos <- spCbind(demos, GEOPIN)

demos.council <- over(geometry(demos), council)
council <- demos.council$COUNCILDIS
demos <- spCbind(demos, council)

# Get coordinates as WGS84 (the format on Socrata)
demos <- spTransform(demos, CRS("+init=epsg:4326"))
demo.coords <- demos@coords

# Convert to a data.frame and do some final cleaning
demos <- as.data.frame(demos)
demos <- apply(demos, 2, as.character)
demos <- cbind(demos, demo.coords) # this may duplicate the coords.x1 (X/long) and coords.x2 (Y/lat) columns,
                                   # but better to have too many coordinates than not enough
demos[is.na(demos)] <- ""

# Export
write.csv(demos, "./output/demos-for-socrata.csv")

# This will put the file into your working directory.
# To add the data to Socrata, go to the Demolition table on Socrata:
# https://data.nola.gov/Housing-Land-Use-and-Blight/BlightStatus-Demolitions/e3wd-h7q2
# You must have permission to edit the dataset.
# Click the "Edit" button at the top. Choose "Edit Dataset." Hit the top "Edit" button again,
# then go to the Append and Replace Wizard.
# Select Append, then choose the relevant columns from the original that will go to

## A couple of notes:
# 1. The Full_Address column gives the address as provided by demolition contacts.
# Match_Address should be the address resulting from geocoding.
# 2. To get the coordinates, choose to import from multiple columns, then select the relevant lat/long columns.
# R should produce a coords.x1 and coords.x2 column, which are longitude and latitude, respectively.
# 3. Spot check data to make sure it lines up correctly before publishing, particularly the coordinates
