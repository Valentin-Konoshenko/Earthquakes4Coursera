context("eq_create_label function")

test_that("Location + Magnitude + Deaths", {
  input_row <- data.frame(LOCATION_NAME = "L1",
                          EQ_PRIMARY = 5,
                          TOTAL_DEATHS = 0)
  output_label = "<b>Location:</b> L1 <br/><b>Magnitude:</b> 5 <br/><b>Total deaths:</b> 0 <br/>"
  expect_identical(eq_create_label(input_row), output_label)
})

test_that("Magnitude + Deaths", {
  input_row <- data.frame(LOCATION_NAME = NA,
                          EQ_PRIMARY = 5,
                          TOTAL_DEATHS = 0)
  output_label = "<b>Magnitude:</b> 5 <br/><b>Total deaths:</b> 0 <br/>"
  expect_identical(eq_create_label(input_row), output_label)
})

test_that("Location + Magnitude", {
  input_row <- data.frame(LOCATION_NAME = "L1",
                          EQ_PRIMARY = 5,
                          TOTAL_DEATHS = NA)
  output_label = "<b>Location:</b> L1 <br/><b>Magnitude:</b> 5 <br/>"
  expect_identical(eq_create_label(input_row), output_label)
})

test_that("Location + Deaths", {
  input_row <- data.frame(LOCATION_NAME = "L1",
                          EQ_PRIMARY = NA,
                          TOTAL_DEATHS = 0)
  output_label = "<b>Location:</b> L1 <br/><b>Total deaths:</b> 0 <br/>"
  expect_identical(eq_create_label(input_row), output_label)
})
