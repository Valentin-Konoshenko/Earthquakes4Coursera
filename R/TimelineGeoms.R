##### theme_timeline ####
#' This theme is used to visualize earthquakes
#'
#' The theme is built on top \code{\link{theme_bw}} in order to follow the style
#' of the Coursera's assignment
#'
#' @examples
#' ggplot(mtcars) + geom_point(aes(x = wt, y = mpg)) + theme_timeline()
#'
#' @export
#'
#' @import ggplot2
theme_timeline <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = 'bottom',
      legend.title = ggplot2::element_text(face = "bold" ),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_line(size = 1),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x  = ggplot2::element_line(size = 1))
}
##### GeomTimeline ####
GeomTimeline <- ggplot2::ggproto(
  "GeomTimeline", ggplot2::Geom, required_aes = "x",
  default_aes = ggplot2::aes(
    shape = 19, colour = "black", fill = "white",
    alpha = 0.5, stroke = 0.5, size = 3, y = 0),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord, na.rm = FALSE, xmin, xmax) {
    data <- data[
      (is.na(xmin) | data$x >= as.numeric(xmin)) &
      (is.na(xmax) | data$x <= as.numeric(xmax)), ]
    coords <- coord$transform(data, panel_scales)
    if (nrow(data) == 0) {ggplot2::zeroGrob()}
    else {
      eq_point <-
        grid::pointsGrob(
          x = coords$x,
          y = coords$y,
          pch = coords$shape,
          size = grid::unit(coords$size / 4, "char"),
          gp = grid::gpar(col = coords$colour, alpha = coords$alpha))
      eq_line <- grid::segmentsGrob(
        x0 = 0, y0 = coords$y,
        x1 = 1, y1 = coords$y,
        default.units = "native",
        gp = grid::gpar(
          size = 0.5,
          alpha = coords$alpha * 0.5,
          col = "grey"))
      timeline <- grid::gTree(children = grid::gList(eq_line, eq_point))}
  }
)
##### geom_timeline ####
#' A time line of earthquakes
#'
#' This geom plots a time line of earthquakes.
#' ranging from \code{xmin} to \code{xmax} dates with a point for each earthquake.
#' Optional aesthetics include color, size, and alpha (for transparency).
#' The \code{x} aesthetic is a date and an optional \code{y} aesthetic is a
#' factor indicating some stratification in which case multiple time lines
#' will be plotted for each level of the factor (e.g. country).
#'
#' @param xmin Min date for additional filtering
#' @param xmax Max date for additional filtering

#' @section Aesthetics:
#'
#' \code{geom_timeline} understands the following aesthetics
#' (required aesthetics are in bold):
#' \itemize{
#'  \item \strong{x}
#'  \item y
#'  \item xmin
#'  \item xmax
#'  \item alpha
#'  \item colour
#'  \item fill
#'  \item size
#'  \item shape}
#'
#' @examples
#' #without stratification
#' file_read() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY %in% c("USA", "CHINA"),
#'                 between(lubridate::year(DATE), 2010, 2011)) %>%
#'   ggplot(aes(x = DATE,
#'              color = DEATHS,
#'              size = EQ_PRIMARY)) +
#'   geom_timeline(alpha = 0.4) +
#'   theme_timeline() +
#'   labs(size = "Richter scale value", colour = "# deaths")
#'
#' #with stratification and additional filtering by time
#' file_read() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY %in% c("USA", "CHINA", "CHILE"),
#'                 between(lubridate::year(DATE), 2010, 2011)) %>%
#'   ggplot(aes(x = DATE,
#'              y = COUNTRY,
#'              color = DEATHS,
#'              size = EQ_PRIMARY)) +
#'   geom_timeline(xmin = eq_get_date(2010, 1, 1),
#'                 xmax = eq_get_date(2010, 12, 31),
#                  alpha = 0.4) +
#'   theme_timeline() +
#'   labs(size = "Richter scale value", colour = "# deaths")
#' @inheritParams ggplot2::geom_ribbon
#'
#' @export
#'
#' @import ggplot2
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE, inherit.aes = TRUE,
                          xmin = NA, xmax = NA, ...) {
  layer(geom = GeomTimeline, mapping = mapping, data = data, stat = stat,
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, xmin = xmin, xmax = xmax, ...))
}
##### GeomTimelineLabel ####
GeomTimelineLabel <- ggplot2::ggproto(
  "GeomTimelineLabel", ggplot2::Geom,
  required_aes = c("x", "label"),
  default_aes = ggplot2::aes(y = 0, angle = 45),
  draw_key = ggplot2::draw_key_point,
  draw_panel = function(data, panel_scales, coord, n_max) {
    if (!is.na(n_max))
      data <- data %>%
        dplyr::group_by(y) %>%
        dplyr::top_n(n = n_max, wt = size) %>%
        dplyr::ungroup()
    data <- data %>% dplyr::mutate(group = row_number())
    coords <- coord$transform(data, panel_scales)
    line_coords <- coords %>%
      dplyr::mutate_(y = ~ y + 0.1) %>%
      dplyr::bind_rows(coords)
    text <- grid::textGrob(
      label = coords$label,
      x = coords$x,
      y = coords$y + 0.1,
      rot = 45,
      just = c("left", "center"),
      gp = grid::gpar(col = "black", fontsize = 4 * .pt))
    lines <- grid::polylineGrob(
      x = line_coords$x,
      y = line_coords$y,
      id = line_coords$group,
      gp = grid::gpar(col = alpha("gray20", 0.25), lwd = 0.5 * .pt))
    grid::grobTree(lines, text)
  }
)
##### geom_timeline_label ####
#' Annotations to the earthquake data
#'
#' This geom adds a vertical line to each data point with a text annotation
#' (e.g. the location of the earthquake) attached to each line. There is an
#' option to subset to \code{n_max} number of earthquakes, where we take the
#' \code{n_max} largest (by magnitude) earthquakes.
#' Aesthetics are \code{x}, which is the date of the earthquake and \code{label}
#' which takes the column name from which annotations will be obtained.
#' If \code{n_max} is omitted, each earthquake is annotated
#'
#' @param n_max A number of largest (by magnitude) earthquakes to be annotated.
#' If omitted, each earthquake is annotated
#'
#' @section Aesthetics:
#'
#' \code{geom_timeline_label} understands the following aesthetics
#' (required aesthetics are in bold):
#' \itemize{
#'  \item \strong{x}
#'  \item \strong{label}
#'  \item n_max}
#'
#' @examples
#' #without stratification with 2 greatest earthquakes are annotated
#' file_read() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY %in% c("USA", "CHINA"),
#'                 between(lubridate::year(DATE), 2010, 2011)) %>%
#'   ggplot(aes(x = DATE,
#'              color = DEATHS,
#'              size = EQ_PRIMARY)) +
#'   geom_timeline(alpha = 0.4) +
#'   theme_timeline() +
#'   labs(size = "Richter scale value", colour = "# deaths") +
#'   geom_timeline_label(aes(label = LOCATION_NAME), n_max = 2)
#' @inheritParams ggplot2::geom_ribbon
#'
#' @export
#'
#' @import ggplot2
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, n_max = NA, ...) {
  ggplot2::layer(geom = GeomTimelineLabel, mapping = mapping, data = data,
                 stat = stat, position = position, show.legend = show.legend,
                 inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, n_max = n_max, ...))}
