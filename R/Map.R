#' Visualize earthquakes on a map
#'
#' The function takes an argument \code{eq_data} containing the filtered data frame
#' with earthquakes to visualize. The function maps the epicenters
#' (\code{LATITUDE}/\code{LONGITUDE}) and annotates each point with in pop up window
#' containing annotation data stored in a column of the data frame.
#' The user is able to choose which column to use for the annotation in
#' the pop-up with a function argument named \code{annot_col}.
#' Each earthquake is shown with a circle whose radius is proportional
#' to the earthquake's magnitude.
#'
#' @param eq_data A data frame containing earthquake related data.
#' @param annot_col The column name which should be used for annotation purpose
#'
#' @examples
#' library(dplyr)
#' file_read() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 2000) %>%
#'   eq_map(annot_col = "DATE")
#' @export
#'
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr "%>%"
eq_map <- function(eq_data, annot_col){
  annot <- eq_data[[annot_col]] %>% as.character
  leaflet::leaflet() %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(data = eq_data,
                              lng = ~ LONGITUDE,
                              lat = ~ LATITUDE,
                              radius = ~ EQ_PRIMARY,
                              weight = 1,
                              popup = annot)
}
#' Prepare the annotation of an earthquake
#'
#' The function creates an HTML label, which can be used as the annotation text in the leaflet map.
#' The function combines the following information about an earthquake:
#' \itemize{
#'  \item Location
#'  \item Magnitude
#'  \item Total number of deaths}
#' If an earthquake is missing values for any of these items, it is skipped for that element
#' of the tag.
#'
#' @param data A row of data frame data containing earthquake related data.
#'
#' @return An HTML label, which can be used as the annotation text in the leaflet map
#'
#' @examples
#' library(dplyr)
#' file_read() %>%
#'   eq_clean_data() %>%
#'   dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2014) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'   eq_map(annot_col = "popup_text")
#' @export
eq_create_label <- function(data) {
  Popup1 <- ifelse(is.na(data$LOCATION_NAME), "",
                   paste("<b>Location:</b>", data$LOCATION_NAME, "<br/>"))
  Popup2 <- ifelse(is.na(data$EQ_PRIMARY), "",
                   paste("<b>Magnitude:</b>", data$EQ_PRIMARY, "<br/>"))
  Popup3 <- ifelse(is.na(data$TOTAL_DEATHS), "",
                   paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br/>"))
  paste0(Popup1, Popup2, Popup3)
}
