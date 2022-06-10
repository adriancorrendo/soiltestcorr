# Tests for soiltestcorr

require(dplyr)

#################################################################################
# 1
## linear_plateau
## nrow<4
context("run linear_plateau() with packaged dataset freitas1966")

freitas_less_4 <- soiltestcorr::freitas1966 %>% dplyr::slice_head(n=3)

linear_plateau.test <- try(linear_plateau(data = freitas_less_4, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_true(inherits(linear_plateau.test, "try-error"))
  
})

# 2
## linear_plateau
## missing stv

linear_plateau.test <- try(linear_plateau(data = freitas1966, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_true(inherits(linear_plateau.test, "try-error"))
  
})

# 3
## linear_plateau
## missing ry

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, 
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_true(inherits(linear_plateau.test, "try-error"))
  
})


# 2
## linear_plateau
## Options tidy = FALSE, plot = FALSE, resid = FALSE 
context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = TRUE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})


# 9 Target < plateau

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                                tidy = TRUE,
                                                plot = FALSE,
                                                resid = TRUE,
                                                target = 90),
                              silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})


# 10 Target > plateau

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, stv = STK, ry = RY,
                                                tidy = TRUE,
                                                plot = FALSE,
                                                resid = TRUE,
                                                target = 99),
                              silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})
