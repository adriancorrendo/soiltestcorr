## quadratic_plateau

# 1
# Test quadratic plateau
# Options tidy = FALSE, plot = FALSE, resid=FALSE
context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = FALSE,
                                                resid = FALSE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

# 2
# Test quadratic plateau
# Options tidy = FALSE, plot = FALSE, resid = TRUE

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = FALSE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})


# 3
# Test quadratic plateau
# Options tidy = FALSE, plot = TRUE, resid = TRUE

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = TRUE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

# 4
# Test quadratic plateau
# Options tidy = TRUE, plot = TRUE, resid = TRUE

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = TRUE,
                                                plot = TRUE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

# 5
# Test quadratic plateau
# Options tidy = TRUE, plot = FALSE, resid = FALSE
context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = TRUE,
                                                plot = FALSE,
                                                resid = FALSE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

# 6
# Test quadratic plateau
# Options tidy = TRUE, plot = FALSE, resid = TRUE
context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = TRUE,
                                                plot = FALSE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})


# 7
# Test quadratic plateau
# Options tidy = FALSE, plot = TRUE, resid = FALSE
context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = TRUE,
                                                resid = FALSE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})