# Tests for soiltestcorr

require(dplyr)

# 1
# Test mod_alcc()
# Options plot = FALSE, tidy = FALSE

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, RY = RY, STV = STK, target=90, 
                              confidence = 0.95,
                              plot = FALSE, tidy = FALSE),
           silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})

# 2
# Test mod_alcc()
# Options plot = FALSE, tidy = TRUE

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, RY = RY, STV = STK, target=90, 
                              confidence = 0.95,
                              plot = FALSE, tidy = TRUE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})

# 3
# Test mod_alcc()
# Options plot = TRUE, tidy = FALSE

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, RY = RY, STV = STK, target=90, 
                              confidence = 0.95,
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})

#######################

#######################################

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

#################################################################################


