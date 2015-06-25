# utils.R
# generic utility functions used by cleaners.R, plotters.R, and most of the scripts
#
#

require(stringr)
require(dplyr)

slugify <- function(col_names) {
  col_names <- tolower( gsub( '\\.', '_', str_trim(col_names) ) )
  return( gsub('(_)$|^(_)', '', col_names) )
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

toDate <- function(col) {
  as.POSIXct(col, format = "%m/%d/%Y")
}

getOneYear <- function(df, date_col, from_date) {
  #`df` data frame to subset out into one year-long period
  #`date_col` column in said data frame which has the month-year date by which you'd like to subset it out. do not pass this in quotes
  #`from` month of the reporting period, should be formatted "mmm yyyy"
  
  date_col <- eval(substitute(date_col), envir = df)
  date_col <- as.Date(as.yearmon(date_col), format = "%b %Y")
  l <- as.Date((as.yearmon(from_date) - 1), format = "%b %Y") #sets to reporting period minus one year
  u <- as.Date((as.yearmon(from_date) + .1), format = "%b %Y") #sets to reporting period plus one month
  df <- filter(df, ymd(date_col) >= ymd(l) & ymd(date_col) < ymd(u))
  return(df)
}

getTwoYears <- function(df, date_col, from_date) {
  #`df` data frame to subset out into one year-long period
  #`date_col` column in said data frame which has the month-year date by which you'd like to subset it out. do not pass this in quotes
  #`from` month of the reporting period, should be formatted "mmm yyyy"
  
  date_col <- eval(substitute(date_col), envir = df)
  date_col <- as.Date(as.yearmon(date_col), format = "%b %Y")
  l <- as.Date((as.yearmon(from_date) - 2), format = "%b %Y") #sets to reporting period minus two years
  u <- as.Date((as.yearmon(from_date) + .1), format = "%b %Y") #sets to reporting period plus one month
  df <- filter(df, ymd(date_col) >= ymd(l) & ymd(date_col) < ymd(u))
  return(df)
}

prettyPercentBreaks <- function(m, n) {
  # returns a range of pretty values in or near range boundaries for small ranges, like when you're dealing with percents
  # `m` is max value in your vector
  # note this does not return negative values
  
  if(min(m) < 0) stop("Sorry, this just returns a range for positive values. Use pretty_breaks() to get a range with negative values")
  
  #get initial inteval and round up to number of significant digits in it
  int <- m/n
  sigs <- nchar(strsplit( as.character(int), ".", fixed = TRUE )[[1]][2])
  if(sigs > 2) sigs <- 2
  int <- round(int, sigs)
  
  #recalc with m, as max, one interval above m user entered
  m <- m + int
  int <- m/n
  sigs <- nchar(strsplit( as.character(int), ".", fixed = TRUE )[[1]][2])
  if(sigs > 2) sigs <- 2
  int <- round(int, sigs)
  
  return(seq(0, m, int))
}

dateFromYearMon <- function(ym, eom = TRUE) {
  if(eom == TRUE) {
    ym <- as.Date(as.yearmon(ym))
    dateObj <- ymd(paste(
      year(ym),
      month(ym),
      days_in_month(month(ym)),
      sep = "-"
    ))
    return(dateObj)
  } else if(eom == FALSE) {
    ym <- as.Date(as.yearmon(ym))
    dateObj <- ymd(paste(
      year(ym),
      month(ym),
      "01",
      sep = "-"
    ))
    return(dateObj)
  }
}
