context("eq_map function")

pct <- file_read() %>%
  eq_clean_data() %>%
  dplyr::filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 2000) %>%
  eq_map(annot_col = "DATE")

test_that("Class is correct", {
  expect_is(pct, "leaflet")
  expect_is(pct, "htmlwidget")
})

test_that("correct methods are executed", {
  expect_match(pct$x$calls[[1]]$method, "(addTiles)|(addCircleMarkers)")
  expect_match(pct$x$calls[[2]]$method, "(addTiles)|(addCircleMarkers)")
})

