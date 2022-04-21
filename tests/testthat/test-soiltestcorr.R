context("run mod_alcc() with packaged dataset data_test")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, RY = RY, STV = STK, target=90, confidence = 0.95,
                                       plot = FALSE, tidy = FALSE),
           silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})

## cate_nelson_1965

context("run cate_nelson_1965() with packaged dataset freitas1966")

cate_nelson_1965.test <- try(cate_nelson_1965(data = freitas1966, STV = STK, RY = RY, target = 90,
                                              plot = FALSE, tidy = FALSE),
           silent = TRUE)

test_that("no error in fitting cate_nelson_1965() for the example dataset", {
  
  expect_false(inherits(cate_nelson_1965.test, "try-error"))
  
})

## cate_nelson_1971

context("run cate_nelson_1971() with packaged dataset freitas1966")

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas1966, STV = STK, RY = RY,
                                              plot = FALSE, tidy = FALSE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_false(inherits(cate_nelson_1971.test, "try-error"))
  
})

#################################################################################
## linear_plateau

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                             silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          tidy=FALSE,
                                          plot = TRUE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

#################################################################################

## quadratic_plateau

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = FALSE,
                                                resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = FALSE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = FALSE,
                                                plot = TRUE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = TRUE,
                                                plot = TRUE,
                                                resid = TRUE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, STV = STK, RY = RY,
                                                tidy = TRUE,
                                                plot = FALSE,
                                                resid = FALSE),
                              silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})
