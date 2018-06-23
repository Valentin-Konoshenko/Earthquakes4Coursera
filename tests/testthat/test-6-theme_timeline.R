context("theme_timeline function")

pct <- ggplot(mtcars) +
  geom_point(aes(x = wt, y = mpg)) +
  theme_timeline()

test_that("Class is theme", {
  expect_is(pct, "ggplot")
  expect_is(pct$theme, "theme")
})

test_that("panel.border is empty", {
  expect_is(pct$theme["panel.border"], "list")
  expect_is(pct$theme[["panel.border"]], "element_blank")
})
