context("run modALCC() with packaged dataset data_test")

data("freitas1966")

modALCC.test <- try(modALCC(data = freitas1966, RY = RY, STV = STK, target=90, confidence = 0.95,
                                       plot = FALSE, tidy = FALSE),
           silent = TRUE)

test_that("no error in fitting modALCC() for the example dataset", {
  
  expect_false(inherits(modALCC.test, "try-error"))
  
})

## cate.nelson.1965

context("run cate.nelson.1965() with packaged dataset freitas1966")

cate_nelson_1965.test <- try(cate_nelson_1965(data = freitas1966, STV = STK, RY = RY, target = 90,
                                              plot = FALSE, tidy = FALSE),
           silent = TRUE)

test_that("no error in fitting cate.nelson.1965() for the example dataset", {
  
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


## linear_plateau

context("run linear_plateau() with packaged dataset freitas1966")

linear_plateau.test <- try(linear_plateau(data = freitas1966, STV = STK, RY = RY,
                                          plot = FALSE),
                             silent = TRUE)

test_that("no error in fitting linear_plateau() for the example dataset", {
  
  expect_false(inherits(linear_plateau.test, "try-error"))
  
})

