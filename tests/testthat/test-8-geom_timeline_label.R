context("geom_timeline_label function")

pct <- ggplot(mtcars, aes(x = wt, size = disp, color = cyl)) +
  geom_timeline() +
  geom_timeline_label(aes(label = drat), n_max = 4)

test_that("Class is still ggplot", {
  expect_is(pct, "ggplot")
})

test_that("n_max exists and set correctly", {
  params = pct$layers[[2]]$geom_params
  expect_is(params, "list")
  expect_identical(exists("n_max", where = params), TRUE)
  expect_identical(params$n_max, 4)
})
