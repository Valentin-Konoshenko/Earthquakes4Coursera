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
if (!require(grid)) {
  install.packages("grid");
  if(!require(grid, character.only = TRUE)) stop("Package grid not found");
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
      DEATHS = coalesce(as.numeric(DEATHS), NA_real_),
      EQ_MAG_MS = coalesce(as.numeric(EQ_MAG_MS), NA_real_),
      COUNTRY = as.factor(COUNTRY)) %>%
    dplyr::select(
      "DATE", "LATITUDE", "LONGITUDE", "LOCATION_NAME", "COUNTRY",
      "DEATHS", "EQ_MAG_MS")
}

NOAA <- file_read("inst\\extdata\\results.txt")
NOAAC <- eq_clean_data(NOAA)

#####

GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("xmin", "xmax"),
    default_aes = ggplot2::aes(shape = 19, colour = NA,
                               alpha = 0.5##, size = 10##, size = 5
                               ),
    draw_key = ggplot2::draw_key_point,
    draw_group =
      function(data, panel_scales, coord) {
        xmin <- as.numeric(data[1, "xmin"])
        xmax <- as.numeric(data[1, "xmax"])
        str(data)
        data_filtered <-
          data[data$x >= xmin & data$x <= xmax & !is.na(data$x), ] %>%
          dplyr::mutate(size = if_else(is.na(size), 1, size))
        str(data_filtered)
        coords <- coord$transform(data_filtered, panel_scales)
        str(coords)
       gp <<- grid::gpar(
            alpha = coords$alpha,
          col = coords$colour,
          fontsize = grid::unit(coords$size * 2, "npc")
            ##1 *
            ##ifelse(is.na(coords$size), 1, coords$size)
        )
        grid::pointsGrob(coords$x, coords$y, pch = coords$shape
          ,gp = gp
          )
      }
    )
#####
geom_timeline <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "identity", show.legend = NA, na.rm = FALSE,
           inherit.aes = TRUE, xmin, xmax, ...) {
    layer(geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ...))
  }
#####
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("CHINA", "USA"),
                between(lubridate::year(DATE),  2010, 2011)) %>%
  ggplot(aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_MAG_MS
             )) +
  geom_timeline(
    xmin = eq_get_date(2010, 1, 1),
    xmax = eq_get_date(2011, 12, 31))

NOAAC %>%
  dplyr::filter(COUNTRY %in% c("CHINA"),
                between(lubridate::year(DATE), 2010, 2011)) %>%
  ggplot(aes(x = DATE, y = COUNTRY, color = DEATHS
             )) +
  geom_timeline(
    xmin = eq_get_date(2010, 1, 1),
    xmax = eq_get_date(2011, 12, 31))

NOAAC %>%
  dplyr::filter(COUNTRY %in% c("CHINA", "USA", "IRAN"),
                between(lubridate::year(DATE), 2010, 2011)) %>%
  ggplot(aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_MAG_MS)) +
  geom_point(alpha = 0.5)
#####direct####
NOAAC %>%
  ggplot(aes(x = DATE, color = DEATHS, size = EQ_MAG_MS)) +
  geom_point(alpha = 1, y = 0) +
  xlim(eq_get_date(1988, 12, 1), eq_get_date(1988, 12, 31))

NOAAC %>% filter(lubridate::year(DATE) == 1988) %>%
  ggplot(aes(x = DATE, color = DEATHS, size = EQ_MAG_MS)) +
  geom_segment(aes(x = min(DATE), xend = max(DATE), y = 0, yend = 0),
               color = "green", alpha = 0.5, size = 1) +
  geom_point(alpha = 0.5, y = 0)
