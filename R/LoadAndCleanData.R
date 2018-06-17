#' Read earthquake related data from a file
#'
#' The function is supposed to be used to read an embedded file which contains
#' earthquake related data
#' (downloaded from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{NOAA database}).
#' It also can be used in case you need to read a file downloaded manually.
#'
#' @param filename A name of the file to read. By default read the data
#' of an embedded file (downloaded from \href{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}{NOAA database}).
#'
#' @examples
#' \dontrun{file_read()}
#'
#' @export
#'
#' @importFrom readr read_delim
file_read <- function(filename = "") {
  if (filename == "")
    filename <- system.file("extdata",
                            "results.txt",
                            package = "Earthquakes4Coursera")
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  readr::read_delim(filename, delim = "\t")
}

#' Date Conversion from Numeric Representation
#'
#' In contrast to \code{\link{ISOdate}} it can also define BC dates (at least those
#' of them that comply with R). Only the additional day of leap years (February 29)
#' cannot be defined correctly as R introduces extra 0-th year, which actually didn't
#' exist. That is the only reason why BC leap years in R are shifted on 1-year away
#' from real ones.
#'
#' @param Y, numerical values to specify a year.
#' @param M, numerical values to specify a month
#' @param D, numerical values to specify a day
#'
#' @return An object of class "\code{\link{Date}}".
#'
#' @examples
#'  eq_get_date(-100, 12, 31);
#'  eq_get_date(2000);
#'
#' @export
eq_get_date <- function(Y, M = 1, D = 1) {
  Shift <- -2400
  M <- ifelse(is.na(M), 1, M)
  D <- ifelse(is.na(D), 1, D)
  T_Date0 <- ISOdate(-Shift, 1, 1, 0)
  T_Year = Y - Shift
  T_Date = ISOdate(T_Year, M, D, 0)
  T_Shift = (as.numeric(T_Date) - as.numeric(T_Date0)) / 3600 / 24
  as.Date(T_Shift, "0000-01-01")
}

#' Cleans the location data
#'
#' It's supposed to be used to clean the data of the LOCATION_NAME column. It does
#' it in the following way:
#' \itemize{
#'  \item stripes out the country name (including the colon)
#'  \item converts names to title case (as opposed to all caps)}
#'
#' @param location input location to clean.
#'
#' @return Cleaned location which contains the places affected by earthquake converted
#'   to title case.
#'
#' @examples
#'  eq_location_clean("TURKEY:  IZMIR, EFES"); #Izmir, Efes
#' @importFrom stringr str_to_title
#'
#' @export
eq_location_clean <- function(location) {
  stringr::str_to_title(sub("^[^:]*: *", "", location))
}

#' Cleans NOAA data frame with earthquake related data
#'
#' It's expected that the data frame is produced by the \code{\link{file_read}}
#' function. The cleaning is performed in the following way:
#' \enumerate{
#'  \item Creates the \code{DATE} column by uniting the year, month, day
#'    and converting it to the Date class
#'  \item Converts the \code{LATITUDE} and \code{LONGITUDE} columns to numeric class
#'  \item Cleans the \code{LOCATION_NAME} column by stripping out the country name
#'    (including the colon) and converts names to title case (as opposed to all caps)}
#'
#' @param df a data frame to clean
#'
#' @return Cleaned data frame
#'
#' @examples
#'  file_read() %>% eq_clean_data()
#'
#' @importFrom dplyr mutate select coalesce %>%
#'
#' @export
eq_clean_data <- function(df) {
  df %>%
    dplyr::mutate(
      DATE = eq_get_date(Y = YEAR, M = MONTH, D = DAY),
      LOCATION_NAME = eq_location_clean(LOCATION_NAME),
      DEATHS = dplyr::coalesce(as.numeric(DEATHS), NA_real_),
      EQ_MAG_MS = dplyr::coalesce(as.numeric(EQ_MAG_MS), NA_real_),
      EQ_PRIMARY = dplyr::coalesce(as.numeric(EQ_PRIMARY), NA_real_),
      COUNTRY = as.factor(COUNTRY),
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE)) %>%
    dplyr::select(
      "DATE", "LATITUDE", "LONGITUDE", "LOCATION_NAME", "COUNTRY",
      "DEATHS", "TOTAL_DEATHS", "EQ_MAG_MS", "EQ_PRIMARY")
}
