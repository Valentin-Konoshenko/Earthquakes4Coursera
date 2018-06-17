context("eq_clean_data function")

raw_data <-
  data.frame(
    I_D = c(1, 2),
    YEAR = c(-700, 1000),
    MONTH = c(NA, 10),
    DAY = c(NA, 3),
    EQ_MAG_MS = c(NA, "7"),
    EQ_PRIMARY = c("3.2", "7"),
    COUNTRY = c("COUNTRY1", "COUNTRY2"),
    LOCATION_NAME = c("COUNTRY1: PLACE1, PLACE2", "COUNTRY2: PLACE3"),
    LATITUDE = c("10","20"),
    LONGITUDE = c("40","-30"),
    TOTAL_DEATHS = c(NA, 10),
    DEATHS = c(NA, 10))
clean_data <- eq_clean_data(raw_data)

test_that("a class of result object is data.frame", {
  expect_is(clean_data, "data.frame")
})

test_that("number of rows has not changed", {
  expect_identical(as.double(nrow(clean_data)), 2)
})

test_that("LOCATION_NAME is correct", {
  expect_identical(clean_data$LOCATION_NAME, c("Place1, Place2", "Place3"))
})
