
[![Travis-CI Build Status](https://travis-ci.org/Valentin-Konoshenko/Earthquakes4Coursera.svg?branch=master)](https://travis-ci.org/Valentin-Konoshenko/Earthquakes4Coursera) <!-- README.md is generated from README.Rmd. Please edit that file -->

Earthquakes4Coursera
====================

Earthquakes4Coursera has been developed in a context of the Coursera's course "Mastering Software Development in R Capstone". The goal of the package is to visualize earthquakes registered in [NOAA Significant Earthquake Database](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1).

Installation
------------

You can install Earthquakes4Coursera from github with:

``` r
# install.packages("devtools")
devtools::install_github("Valentin-Konoshenko/Earthquakes4Coursera")
```

Usage
-----

It allows to visualize earthquakes in two ways:

-   Time lines to show earthquakes in time and
-   Mapping to visualize earthquakes in space

Examples of timelines
---------------------

This is a basic example of plotting a timeline:

``` r
library(Earthquakes4Coursera)
library(dplyr)
library(ggplot2)
#> Warning: package 'ggplot2' was built under R version 3.4.4
NOAAC <- file_read() %>% eq_clean_data()
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA"),
                between(lubridate::year(DATE), 2000, 2017)) %>%
  ggplot(aes(x = DATE,
             color = DEATHS,
             size = EQ_PRIMARY)) +
  geom_timeline() +
  theme_timeline() +
  labs(size = "Richter scale value", colour = "# deaths") +
  geom_timeline_label(aes(label = LOCATION_NAME), n_max = 5)
```

![](README-timeline_basic-1.png)

The following timeline stratifies the earthquakes by countries:

``` r
NOAAC %>%
  dplyr::filter(COUNTRY %in% c("USA", "CHINA"),
                between(lubridate::year(DATE),  2008, 2011)) %>%
  ggplot(aes(x = DATE,
             y = COUNTRY,
             color = DEATHS,
             size = EQ_PRIMARY)) +
  geom_timeline() +
  theme_timeline() +
  labs(size = "Richter scale value", color = "# deaths") +
  geom_timeline_label(aes(label = LOCATION_NAME), n_max = 3)
```

![](README-timeline_countries-1.png)

Example of mapping
------------------

``` r
library(lubridate)
NOAAC %>%
  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2014) %>%
  dplyr::mutate(popup_text = eq_create_label(.)) %>%
  eq_map(annot_col = "popup_text")
```
