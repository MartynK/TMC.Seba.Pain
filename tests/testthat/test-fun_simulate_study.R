# tests/testthat/test-fun_simulate_study.R

library(testthat)
library(lmerTest)
library(emmeans)

test_that("Function returns expected object type", {
  result <- fun_simulate_study()
  expect_true(inherits(result, "emmGrid") || inherits(result, "emm_list"))
})

test_that("Function works with default parameters", {
  result <- fun_simulate_study()
  expect_s3_class(result, "emmGrid")
  summary_result <- summary(result)
  expect_true(all(c("contrast", "estimate", "SE", "df", "t.ratio", "p.value") %in% names(summary_result)))
})

test_that("Function handles custom parameters", {
  result <- fun_simulate_study(
    n_samp = 30,
    block_size_coef = 1.5,
    total_variance = 5,
    ICC = 0.5,
    treat_eff_c = c(4, 2, 0),
    missing_prop = c(0.1, 0.2)
  )
  expect_s3_class(result, "emmGrid")
})

test_that("Function handles insufficient data gracefully", {
  result <- fun_simulate_study(n_samp = 5)
  expect_true(is.data.frame(result))
  expect_equal(result$p.value, c(2, 2, 2))
})

test_that("Model fitting fails with excessive missing data", {
  result <- fun_simulate_study(n_samp = 18, missing_prop = c(0.9, 1))
  expect_true(is.data.frame(result))
  expect_equal(result$p.value, c(2, 2, 2))
})

test_that("Function handles negative total variance", {
  expect_error(
    fun_simulate_study(total_variance = -1),
    "non-numeric argument to binary operator"
  )
})

test_that("Function handles invalid ICC values", {
  expect_error(
    fun_simulate_study(ICC = 1.2),
    "variance components cannot be negative"
  )
  expect_error(
    fun_simulate_study(ICC = -0.1),
    "variance components cannot be negative"
  )
})

test_that("Function handles incorrect length of treatment effects", {
  expect_error(
    fun_simulate_study(treat_eff_c = c(1, 2)),
    "arguments imply differing number of rows: 3, 2"
  )
})

test_that("Function handles non-numeric inputs", {
  expect_error(
    fun_simulate_study(n_samp = "18"),
    "Arguments must be numeric."
  )
  expect_error(
    fun_simulate_study(block_size_coef = "1"),
    "non-numeric argument to binary operator"
  )
  expect_error(
    fun_simulate_study(total_variance = "2"),
    "non-numeric argument to binary operator"
  )
})

test_that("Function works when ICC is zero (no random effects)", {
  result <- fun_simulate_study(ICC = 0)
  expect_true(inherits(result, "emmGrid"))
})

test_that("Function works when total_variance equals var_random", {
  result <- fun_simulate_study(total_variance = 2, ICC = 1)
  expect_true(inherits(result, "emmGrid"))
})

test_that("Function produces expected contrasts", {
  result <- fun_simulate_study()
  summary_result <- summary(result)
  expect_true(all(c("C vs A", "B vs A", "C vs B") %in% summary_result$contrast))
})
