NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA", "CHINA", "SPAIN", "CHILE"),
                between(lubridate::year(DATE),  2010, 2018)) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_PRIMARY
  )) +
  geom_timeline(xmin = eq_get_date(2010, 1, 1),
                xmax = eq_get_date(2018, 12, 31),
                alpha = 0.4
                ) +
  theme_timeline() +
  labs(size = "Richter scale value") +
  labs(colour = "# deaths")
