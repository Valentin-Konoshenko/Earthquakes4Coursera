context("file_read function")

test_that("a class of read data is tbl_df", {
  embedded_data <<- file_read()
  expect_is(embedded_data, "tbl_df")
})

test_that("the embedded data set is not empty", {
  expect_gt(nrow(embedded_data), 0)
})

test_that("the erros is thrown if a file does not exist", {
  expect_error(file_read("absent_file"), "does not exist")
})
