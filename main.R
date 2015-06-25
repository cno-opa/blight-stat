# runs the whole kit and kaboodle

.libPaths("C:\\RPackages")

# get user to set reporting period. some functions in plotters.R need this variable
# cat("What is the reporting period?\n\n##(Use mmm yyyy format, please)##\n\n")
# r_period <- readLines("stdin", 1, warn = FALSE)

# initialize
init <- function(subdir) {
  Rfiles <- list.files(subdir, pattern = "*.R", full.names = TRUE)
  sapply(Rfiles, source)
  cat("Running!\n")
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
init("R")

# finish
cat("Finished!")
