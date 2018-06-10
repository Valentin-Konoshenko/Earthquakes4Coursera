file_read <- function(filename = "inst\\extdata\\results.txt") {
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
      DEATHS = coalesce(as.numeric(DEATHS), NA_real_),
      EQ_MAG_MS = coalesce(as.numeric(EQ_MAG_MS), NA_real_),
      EQ_PRIMARY = coalesce(as.numeric(EQ_PRIMARY), NA_real_),
      COUNTRY = as.factor(COUNTRY),
      LATITUDE = as.numeric(LATITUDE),
      LONGITUDE = as.numeric(LONGITUDE)) %>%
    dplyr::select(
      "DATE", "LATITUDE", "LONGITUDE", "LOCATION_NAME", "COUNTRY",
      "DEATHS", "TOTAL_DEATHS", "EQ_MAG_MS", "EQ_PRIMARY")
}

NOAAC <- file_read() %>% eq_clean_data()
