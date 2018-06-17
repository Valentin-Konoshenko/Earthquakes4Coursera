context("eq_location_clean function")

test_that("a general case", {
  clean_result <- "Sur (Tyre), Sayda (Saida)"
  expect_identical(eq_location_clean("LEBANON:  SUR (TYRE), SAYDA (SAIDA)"),
                   clean_result)
  expect_identical(eq_location_clean(" LEB ANON:  SUR (TYRE), SAYDA (SAIDA)"),
                   "Sur (Tyre), Sayda (Saida)")
  expect_identical(eq_location_clean(" LEBANON  :  SUR (TYRE), SAYDA (SAIDA)"),
                   "Sur (Tyre), Sayda (Saida)")
  expect_identical(eq_location_clean(" LEBANON  : SUR (TYRE), SAYDA (SAIDA)"),
                   "Sur (Tyre), Sayda (Saida)")
  expect_identical(eq_location_clean(" LEBANON  :SUR (TYRE), SAYDA (SAIDA)"),
                   "Sur (Tyre), Sayda (Saida)")
})

test_that("a country is absent", {
  clean_result <- "Sur (Tyre), Sayda (Saida)"
  expect_identical(eq_location_clean("SUR (TYRE), SAYDA (SAIDA)"),
                   clean_result)
  expect_identical(eq_location_clean(": SUR (TYRE), SAYDA (SAIDA)"),
                   clean_result)
  expect_identical(eq_location_clean(" : SUR (TYRE), SAYDA (SAIDA)"),
                   clean_result)
  expect_identical(eq_location_clean(":SUR (TYRE), SAYDA (SAIDA)"),
                   clean_result)
})

test_that("everything other than the first country is preserved", {
  expect_identical(eq_location_clean("CHINA:  GANSU PROVINCE:  LINTAO"),
                   "Gansu Province:  Lintao")
  expect_identical(eq_location_clean("CHINA::GANSU PROVINCE:  LINTAO"),
                   ":Gansu Province:  Lintao")
})

