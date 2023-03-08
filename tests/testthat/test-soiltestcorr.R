# Tests for soiltestcorr::mod_alcc

require(dplyr)

# 1
# Test mod_alcc()
# Options plot = FALSE, tidy = FALSE

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, ry = RY, stv = STK, target=90, 
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

mod_alcc.test <- try(mod_alcc(data = freitas1966, ry = RY, stv = STK, target=90, 
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

mod_alcc.test <- try(mod_alcc(data = freitas1966, ry = RY, stv = STK, target=90, 
                              confidence = 0.95,
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})


#################################################################################


# 4
# Test mod_alcc()
# missing stv

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, ry = RY, target=90, 
                              confidence = 0.95,
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_true(inherits(mod_alcc.test, "try-error"))
  
})


#################################################################################

# 5
# Test mod_alcc()
# missing ry

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, stv = STK, target=90, 
                              confidence = 0.95,
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_true(inherits(mod_alcc.test, "try-error"))
  
})


#################################################################################

# 6
# Test mod_alcc()
# missing target

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, ry = RY, stv = STK, 
                              confidence = 0.95,
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_true(inherits(mod_alcc.test, "try-error"))
  
})


#################################################################################

# 7
# Test mod_alcc()
# missing confidence (warning)

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas1966, ry = RY, stv = STK, target=90, 
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})


#################################################################################



# 8
# Test mod_alcc()
# N < 8

context("run mod_alcc() with packaged dataset freitas1996")

data("freitas1966")

freitas_less_8 <- soiltestcorr::freitas1966 %>% dplyr::slice_head(n=5)

## mod_alcc

mod_alcc.test <- try(mod_alcc(data = freitas_less_8, ry = RY, stv = STK, target=90, 
                              plot = TRUE, tidy = FALSE),
                     silent = TRUE)

test_that("no error in fitting mod_alcc() for the example dataset", {
  
  expect_false(inherits(mod_alcc.test, "try-error"))
  
})

## 9 boot_mod_alcc

boot_mod_alcc.test <- try(boot_mod_alcc(data = freitas1966, 
                                        ry = RY, stv = STK, target=90, n = 2),
                     silent = TRUE)

test_that("no error in fitting boot_mod_alcc() for the example dataset", {
  
  expect_false(inherits(boot_mod_alcc.test, "try-error"))
  
})
