context("run modALCC() with packaged dataset data_test")

data("data_test")

modALCC.test <- try(modALCC(data = data_test, STV = STV, RY = RY, target = 90, confidence = 0.95),
           silent = TRUE)

test_that("no error in fitting modALCC() for the example dataset", {
  
  expect_false(inherits(modALCC.test, "try-error"))
  
})

## cate.nelson.1965

context("run cate.nelson.1965() with packaged dataset freitas1966")

cate.nelson.1965.test <- try(cate.nelson.1965(data = data_test, STV = STV, RY = RY, target = 90),
           silent = TRUE)

test_that("no error in fitting cate.nelson.1965() for the example dataset", {
  
  expect_false(inherits(cate.nelson.1965.test, "try-error"))
  
})

## cate.nelson.1971

context("run cate.nelson.1971() with packaged dataset freitas1966")

cate.nelson.1971.test <- try(cate.nelson.1971(data = data_test, STV = STV, RY = RY),
                             silent = TRUE)

test_that("no error in fitting cate.nelson.1971() for the example dataset", {
  
  expect_false(inherits(cate.nelson.1971.test, "try-error"))
  
})

