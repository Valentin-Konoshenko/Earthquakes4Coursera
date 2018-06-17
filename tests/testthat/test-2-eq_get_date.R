context("eq_get_date function")

test_that("a class of result is Date", {
  expect_is(eq_get_date(2000, 1, 1), "Date")
  expect_is(eq_get_date(-2000, 1, 1), "Date")
})

test_that("a produced positive date is correct", {
  expect_identical(eq_get_date(200, 06, 17), as.Date(ISOdate(200, 06, 17, 0)))
  expect_identical(eq_get_date(4, 2, 29), as.Date(ISOdate(4, 2, 29, 0)))
})

test_that("a produced negative date is correct", {
  date_to_check <- eq_get_date(-73, 05, 31)
  expect_identical(lubridate::year(date_to_check), -73)
  expect_identical(lubridate::month(date_to_check), 5)
  expect_identical(as.double(lubridate::day(date_to_check)), 31)
})
