# runs the whole kit and kaboodle

.libPaths("C:\\RPackages")

# initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
}

#fn to source files stored on Github
source_https <- function(u, unlink.tmp.certs = FALSE) {
    # load package
    require(RCurl)

    # read script lines from website using a security certificate
    if(!file.exists("cacert.pem")) download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile = "cacert.pem")
    script <- getURL(u, followlocation = TRUE, cainfo = "cacert.pem")
    if(unlink.tmp.certs) unlink("cacert.pem")

    # parse lines and evaluate in the global environement
    eval(parse(text = script), envir= .GlobalEnv)
}

# sequence of script executions
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/plotters.R")
source_https("https://raw.githubusercontent.com/cno-opa/graphics/master/mappers.R")
theme_set(theme_opa())
init("R/lib")

# get user to set reporting period
cat("What is the reporting period?\n\n##(Use mmm yyyy format, please)##\n\n")
r_period <- readLines("stdin", 1, warn = FALSE)

# set `final.date` variable
final.date <- as.Date(dateFromYearMon(r_period))

init("R")

# finish
cat("Finished!")
