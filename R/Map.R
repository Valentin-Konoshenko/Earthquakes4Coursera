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
eq_create_label <- function(data) {
  Popup1 <- ifelse(is.na(data$LOCATION_NAME), "",
                   paste("<b>Location:</b>", data$LOCATION_NAME, "<br/>"))
  Popup2 <- ifelse(is.na(data$EQ_PRIMARY), "",
                   paste("<b>Magnitude:</b>", data$EQ_PRIMARY, "<br/>"))
  Popup3 <- ifelse(is.na(data$TOTAL_DEATHS), "",
                   paste("<b>Total deaths:</b>", data$TOTAL_DEATHS, "<br/>"))
  paste0(Popup1, Popup2, Popup3)
}
