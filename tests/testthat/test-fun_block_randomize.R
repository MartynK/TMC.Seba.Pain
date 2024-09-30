# tests/testthat/test-fun_block_randomize.R

library(testthat)

test_that("Returns correct length", {
  n <- 100
  block_size <- 20
  arms <- 6
  assignments <- fun_block_randomize(n, block_size, arms)
  expect_equal(length(assignments), n)
})

test_that("Handles non-integer block size division with warning", {
  n <- 50
  block_size <- 17
  arms <- 5
  expect_warning(
    assignments <- fun_block_randomize(n, block_size, arms),
    "Block size is not a multiple of the number of arms."
  )
  expect_equal(length(assignments), n)
})

test_that("Assigns valid arms", {
  n <- 100
  block_size <- 20
  arms <- 6
  assignments <- fun_block_randomize(n, block_size, arms)
  expect_true(all(assignments %in% 1:arms))
})

test_that("Errors on non-numeric input", {
  expect_error(fun_block_randomize("100", 20, 6), "Arguments must be numeric.")
  expect_error(fun_block_randomize(100, "20", 6), "Arguments must be numeric.")
  expect_error(fun_block_randomize(100, 20, "6"), "Arguments must be numeric.")
})

test_that("Works when n is not a multiple of block_size", {
  n <- 105
  block_size <- 20
  arms <- 6
  assignments <- fun_block_randomize(n, block_size, arms)
  expect_equal(length(assignments), n)
})

test_that("Produces approximately equal assignments per arm", {
  n <- 120
  block_size <- 20
  arms <- 4
  assignments <- fun_block_randomize(n, block_size, arms)
  counts <- table(assignments)
  expected_count <- n / arms
  expect_true(all(abs(counts - expected_count) <= arms))
})

test_that("Handles edge case with block size less than number of arms", {
  n <- 10
  block_size <- 3
  arms <- 5
  expect_warning(
    assignments <- fun_block_randomize(n, block_size, arms),
    "Block size is not a multiple of the number of arms."
  )
  expect_equal(length(assignments), n)
  expect_true(all(assignments %in% 1:arms))
})

test_that("Handles zero subjects", {
  n <- 0
  block_size <- 20
  arms <- 6
  assignments <- fun_block_randomize(n, block_size, arms)
  expect_equal(length(assignments), 0)
})

test_that("Handles single subject", {
  n <- 1
  block_size <- 1
  arms <- 1
  assignments <- fun_block_randomize(n, block_size, arms)
  expect_equal(length(assignments), 1)
  expect_equal(assignments, 1)
})
