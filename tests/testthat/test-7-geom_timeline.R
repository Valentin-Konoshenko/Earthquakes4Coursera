context("geom_timeline function")

pct <- ggplot(mtcars, aes(x = wt, size = disp, color = cyl)) +
  geom_timeline()

test_that("Class is still ggplot", {
  expect_is(pct, "ggplot")
})

test_that("There are both xmin and xmax parameters", {
  params = pct$layers[[1]]$geom_params
  expect_is(params, "list")
  expect_identical(exists("xmin", where = params), TRUE)
  expect_identical(exists("xmax", where = params), TRUE)
})
