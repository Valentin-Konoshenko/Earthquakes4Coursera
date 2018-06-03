#####
if (!require(readr)) {
  install.packages("readr");
  if(!require(readr, character.only = TRUE)) stop("Package readr not found");
}
if (!require(dplyr)) {
  install.packages("dplyr");
  if(!require(dplyr, character.only = TRUE)) stop("Package dplyr not found");
}
if (!require(ggplot2)) {
  install.packages("ggplot2");
  if(!require(ggplot2, character.only = TRUE)) stop("Package ggplot2 not found");
}
if (!require(lubridate)) {
  install.packages("lubridate");
  if(!require(lubridate, character.only = TRUE)) stop("Package lubridate not found");
}
#####
file_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  readr::read_delim(filename, delim = "\t")
}

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

eq_location_clean <- function(location) {
  stringr::str_to_title(sub("^.*: *", "", location))
}

eq_clean_data <- function(df) {
  df %>%
    dplyr::mutate(
      DATE = eq_get_date(Y = YEAR, M = MONTH, D = DAY),
      LOCATION_NAME = eq_location_clean(LOCATION_NAME),
      DEATHS = as.numeric(coalesce(DEATHS, 0)),
      EQ_MAG_MS = as.numeric(ifense(is.na(EQ_MAG_MS), 0, EQ_MAG_MS))) %>%
    dplyr::select(
      "DATE", "LATITUDE", "LONGITUDE", "LOCATION_NAME", "COUNTRY",
      "DEATHS", "EQ_MAG_MS")
}

NOAA <- file_read("inst\\extdata\\results.txt")
NOAAC <- eq_clean_data(NOAA)
#####

NOAAC %>%
  ggplot(aes(x = DATE, color = DEATHS, size = EQ_MAG_MS)) +
  geom_point(alpha = 0.5, y = 0) +
  xlim(eq_get_date(-2000, 1, 1), eq_get_date(0, 1, 1))
