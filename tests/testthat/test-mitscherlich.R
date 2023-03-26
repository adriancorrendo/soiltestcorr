# Tests for soiltestcorr

require(dplyr)

#################################################################################
# 1
## mitscherlich
## nrow<4
context("run mitscherlich() with packaged dataset freitas1966")

freitas_less_4 <- soiltestcorr::freitas1966 %>% dplyr::slice_head(n = 3)

mitscherlich.test <- try(mitscherlich(
  data = freitas_less_4, stv = STK, ry = RY), silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})

# 2
## mitscherlich
## missing stv

mitscherlich.test <- try(mitscherlich(data = freitas1966, ry = RY), silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})

# 3
## mitscherlich
## missing ry

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK), silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})


# 2
## mitscherlich
## Options tidy = FALSE, plot = FALSE, resid = FALSE 
context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                      tidy=FALSE, plot = FALSE, resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

# 3

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = FALSE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

# 4

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                          tidy=FALSE,
                                          plot = TRUE,
                                          resid = TRUE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test, "try-error"))
  
})

# 5

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

# 6

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


# 7

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


# 8

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


# 9 NO LONGER NEEDED
## mitscherlich
## type = NULL
# context("run mitscherlich() with packaged dataset freitas1966")
# 
# 
# mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
#                                       type = NULL), silent = TRUE)
# 
# test_that("no error in fitting mitscherlich() for the example dataset", {
#   
#   expect_true(inherits(mitscherlich.test, "try-error"))
#   
# })

# 10
## mitscherlich
## type = number besides 1, 2, 3
context("run mitscherlich() with packaged dataset freitas1966")


mitscherlich.test <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                      type = 4), silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_true(inherits(mitscherlich.test, "try-error"))
  
})

# 11
## boot_mitscherlich
## context("run boot_mitscherlich() with packaged dataset freitas1966")
boot_mitscherlich.test <- try(boot_mitscherlich(
  data = freitas1966, stv = STK, ry = RY, n = 20, type = 1, target = 90),
  silent = TRUE)

test_that("no error in fitting boot_mitscherlich() for the example dataset", {
  
  expect_false(inherits(boot_mitscherlich.test, "try-error"))
  
})

# 12. Type 2

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test_2 <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                      type = 2, target = 90,
                                      tidy = TRUE,
                                      plot = FALSE,
                                      resid = FALSE),
                         silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test_2, "try-error"))
  
})

# 12. Type 3

context("run mitscherlich() with packaged dataset freitas1966")

mitscherlich.test_3 <- try(mitscherlich(data = freitas1966, stv = STK, ry = RY,
                                        type = 3, target = 90,
                                        tidy = TRUE,
                                        plot = FALSE,
                                        resid = FALSE),
                           silent = TRUE)

test_that("no error in fitting mitscherlich() for the example dataset", {
  
  expect_false(inherits(mitscherlich.test_3, "try-error"))
  
})


