context("run modALCC() with packaged dataset data_test")

data("data_test")

fit <- try(modALCC(data = data_test, STV = STV, RY = RY, target = 90, confidence = 0.95),
           silent = TRUE)

test_that("no error in fitting modALCC() for the example dataset", {
  
  expect_false(inherits(fit, "try-error"))
  
})