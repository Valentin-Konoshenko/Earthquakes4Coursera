#####
if (!require(readr)) {
  install.packages("readr");
  if(!require(readr, character.only = TRUE)) stop("Package radr not found");
}
if (!require(dplyr)) {
  install.packages("dplyr");
  if(!require(dplyr, character.only = TRUE)) stop("Package dplyr not found");
}
#####
file_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  readr::read_delim(filename, delim = "\t")
}

get_date <- function(Y, M, D) {
  Shift <- -2400
  T_Date0 <- ISOdate(-Shift, 1, 1, 0)
  T_Year = Y - Shift
  T_Date = ISOdate(T_Year, M, D, 0)
  T_Shift = (as.numeric(T_Date) - as.numeric(T_Date0)) / 3600 / 24
  as.Date(T_Shift, "0000-01-01")
}

eq_clean_data <- function(df) {
  df %>%
    dplyr::select(
      "YEAR", "MONTH", "DAY",
      "LATITUDE", "LONGITUDE", "LOCATION_NAME", "COUNTRY") %>%
    dplyr::mutate(MONTH = ifelse(is.na(MONTH), 1, MONTH)) %>%
    dplyr::mutate(DAY = ifelse(is.na(DAY), 1, DAY)) %>%
    dplyr::mutate(DATE = get_date(Y = YEAR, M = MONTH, D = DAY)) %>%
    dplyr::select(-YEAR, -MONTH, -DAY)
}

NOAA <- file_read("inst\\extdata\\results.txt")
NOAAC <- eq_clean_data(NOAA)
