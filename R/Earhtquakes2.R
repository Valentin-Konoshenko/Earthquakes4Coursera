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
        )})

NOAAC %>%
  dplyr::filter(COUNTRY %in% c("CHINA", "USA"),
                between(lubridate::year(DATE),  2010, 2011)) %>%
  ggplot(aes(x = DATE, y = COUNTRY, color = DEATHS, size = EQ_MAG_MS
  )) +
  geom_timeline(xmin = eq_get_date(2010, 1, 1), xmax = eq_get_date(2011, 12, 31))

NOAAC %>%
  dplyr::filter(COUNTRY %in% c("CHINA"),
                between(lubridate::year(DATE), 2010, 2011)) %>%
  ggplot(aes(x = DATE, y = COUNTRY, color = DEATHS
  )) +
  geom_timeline(xmin = eq_get_date(2010, 1, 1), xmax = eq_get_date(2011, 12, 31))
