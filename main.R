# runs the whole kit and kaboodle

# TODO: PROFIT

.libPaths("C:\\RPackages")

# initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
}

# fn to source files stored on Github
source_https <- function(u, unlink.tmp.certs = FALSE) {
    require(RCurl)

    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    if(unlink.tmp.certs) unlink("cacert.pem")

    eval(parse(text = script), envir= .GlobalEnv)
}

# reset KPI data object for this session
reset_kpi <- function() {
  kpi <- data.frame(measure = NA, value = NA)
  save(kpi, file = "./data/kpi.Rdata")
}

print_kpis <- function() {
  load('./data/kpi.Rdata')
  print(kpi)
  write.csv(kpi, paste0("./output/kpis-", format(Sys.time(), "%Y-%m-%d"), ".csv"), row.names = FALSE)
}

# sequence of script executions
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/mappers.R")
theme_set(theme_opa())
init("R/lib")
reset_kpi()

# get user to set reporting period
cat("What is the reporting period?\n\n##(Use mmm yyyy format, please)##\n\n")
r_period <- readLines("stdin", 1, warn = FALSE)

# set `final.date` variable, which is what things in `R/` use
final.date <- as.Date(dateFromYearMon(r_period))

init("R")
print_kpis()

# finish
cat("Finished!")
