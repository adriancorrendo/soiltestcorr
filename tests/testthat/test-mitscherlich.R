# Tests for soiltestcorr

require(dplyr)

#################################################################################
# 1
## mitscherlich
## nrow<4
context("run mitscherlich() with packaged dataset freitas1966")

freitas_less_4 <- soiltestcorr::freitas1966 %>% dplyr::slice_head(n=3)

mitscherlich.test <- try(mitscherlich(data = freitas_less_4, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})

# 2
## mitscherlich
## missing stv

mitscherlich.test <- try(mitscherlich(data = freitas1966, ry = RY,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})

# 3
## mitscherlich
## missing ry

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})


# 2
## mitscherlich
## Options tidy = FALSE, plot = FALSE, resid = FALSE 
context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy=FALSE,
                                          plot = TRUE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, target = 90,
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})


# 9

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, 
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = TRUE,
                                          target = 90),
                              silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})


# 10

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          type = 1, 
                                          tidy = TRUE,
                                          plot = FALSE,
                                          resid = TRUE,
                                          target = 100),
                              silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

# 11
## mitscherlich
## nrow<4
context("run mitscherlich() with packaged dataset freitas1966")


mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                      target = 90,
                                      tidy=FALSE,
                                      plot = FALSE,
                                      resid = FALSE),
                         silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})
