## 1 ####
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
## 2 ####
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA", "CHINA", "SPAIN", "CHILE"),
                between(lubridate::year(DATE), 2010, 2018)) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_PRIMARY
  )) +
  geom_timeline(xmin = as.Date("2010-01-01"),
                xmax = eq_get_date(2018, 12, 31),
                alpha = 0.4
  ) +
  theme_timeline() +
  labs(size = "Richter scale value") +
  labs(colour = "# deaths")
## 3 ####
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA", "CHINA", "SPAIN", "CHILE"),
                between(lubridate::year(DATE),  -2000, 1)) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_PRIMARY
  )) +
  geom_timeline(xmin = eq_get_date(-2000, 01, 01),
                xmax = as.Date("0001-01-01"),
                alpha = 1
  ) +
  theme_timeline() +
  labs(size = "Richter scale value", colour = "# deaths")
## 4 ####
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA", "CHINA", "SPAIN", "CHILE"),
                between(lubridate::year(DATE),  -2000, 1)) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_PRIMARY
  )) +
  geom_timeline(xmin = eq_get_date(-2000, 01, 01),
                xmax = as.Date("0001-01-01"),
                alpha = 1
  ) +
  theme_timeline() +
  labs(size = "Richter scale value", colour = "# deaths") +
  geom_timeline_label(aes(label = LOCATION_NAME))

## 5 ####
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA", "CHINA", "SPAIN", "CHILE"),
                between(lubridate::year(DATE),  2010, 2011)) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_PRIMARY
  )) +
  geom_timeline(xmin = eq_get_date(2010, 01, 01),
                xmax = as.Date("2011-12-31"),
                alpha = 1
  ) +
  theme_timeline() +
  labs(size = "Richter scale value", colour = "# deaths") +
  geom_timeline_label(aes(label = LOCATION_NAME))
