# Tests for soiltestcorr

require(dplyr)

#################################################################################
# 1
## quadratic_plateau
## nrow<4
context("run quadratic_plateau() with packaged dataset freitas1966")

freitas_less_4 <- soiltestcorr::freitas1966 %>% dplyr::slice_head(n=3)

quadratic_plateau.test <- try(quadratic_plateau(data = freitas_less_4, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_true(inherits(quadratic_plateau.test, "try-error"))
  
})

# 2
## quadratic_plateau
## missing stv

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_true(inherits(quadratic_plateau.test, "try-error"))
  
})

# 3
## quadratic_plateau
## missing ry

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, 
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_true(inherits(quadratic_plateau.test, "try-error"))
  
})


# 2
## quadratic_plateau
## Options tidy = FALSE, plot = FALSE, resid = FALSE 
context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = TRUE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})

context("run quadratic_plateau() with packaged dataset freitas1966")

quadratic_plateau.test <- try(quadratic_plateau(data = freitas1966, stv = STK, ry = RY,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting quadratic_plateau() for the example dataset", {
  
  expect_false(inherits(quadratic_plateau.test, "try-error"))
  
})