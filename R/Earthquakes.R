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
      EQ_PRIMARY = coalesce(as.numeric(EQ_PRIMARY), NA_real_),
      COUNTRY = as.factor(COUNTRY)) %>%
    dplyr::select(
      "DATE", "LATITUDE", "LONGITUDE", "LOCATION_NAME", "COUNTRY",
      "DEATHS", "EQ_MAG_MS", "EQ_PRIMARY")
}

NOAA <- file_read("inst\\extdata\\results.txt")
NOAAC <- eq_clean_data(NOAA)

#####
theme_timeline <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position='bottom',
      legend.title = ggplot2::element_text(face = "bold" ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(size = 1),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x  = ggplot2::element_line(size = 1))
}

GeomTimeline <-
  ggplot2::ggproto(
    "GeomTimeline", ggplot2::Geom,
    required_aes = c("xmin", "xmax"),
    default_aes = ggplot2::aes(shape = 19, colour = "black", fill = "white",
                               alpha = 0.5, stroke = 0.5, size = 3, y = 0),
    draw_key = ggplot2::draw_key_point,
    draw_panel =
      function(data, panel_scales, coord, na.rm = FALSE) {
        xmin <- as.numeric(data[1, "xmin"])
        xmax <- as.numeric(data[1, "xmax"])
        data <- data[data$x >= xmin & data$x <= xmax & !is.na(data$x), ]
        coords <- coord$transform(data, panel_scales)
        if (nrow(data) == 0) {ggplot2::zeroGrob()}
        else {
          eq_point <-
            grid::pointsGrob(
              x = coords$x,
              y = coords$y,
              pch = coords$shape,
              size = grid::unit(coords$size / 4, "char"),
              gp = grid::gpar(col = coords$colour,
                              alpha = coords$alpha))
          eq_line <- grid::segmentsGrob(
            x0 = 0,
            x1 = 1,
            y0 = coords$y,
            y1 = coords$y,
            default.units = "native",
            gp = grid::gpar(
              size = 0.5,
              alpha = coords$alpha * 0.5,
              col = "grey"))
          timeline <- grid::gTree(children = grid::gList(eq_line, eq_point))
        }
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
