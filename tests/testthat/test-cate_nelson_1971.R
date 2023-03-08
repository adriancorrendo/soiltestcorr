# Test for cate_nelson_1971()

# 1
## cate_nelson_1971
## nrow<4
context("run cate_nelson_1971() with packaged dataset freitas1966")

freitas_less_4 <- soiltestcorr::freitas1966 %>% dplyr::slice_head(n=3)

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas_less_4, stv = STK, ry = RY,
                                              tidy=FALSE,
                                              plot = FALSE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_false(inherits(cate_nelson_1971.test, "try-error"))
  
})

# 2
## cate_nelson_1971
## missing stv

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas1966, ry = RY,
                                              tidy=FALSE,
                                              plot = FALSE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_true(inherits(cate_nelson_1971.test, "try-error"))
  
})

# 3
## cate_nelson_1971
## missing ry

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas1966, stv = STK, 
                                              tidy=FALSE,
                                              plot = FALSE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_true(inherits(cate_nelson_1971.test, "try-error"))
  
})



#######################################
# 4
## cate_nelson_1971
## Options plot = FALSE, tidy = FALSE
context("run cate_nelson_1971() with packaged dataset freitas1966")

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas1966, stv = STK, ry = RY, 
                                              plot = FALSE, tidy = FALSE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_false(inherits(cate_nelson_1971.test, "try-error"))
  
})

#######################################
# 5
## cate_nelson_1971
## Options plot = FALSE, tidy = TRUE
context("run cate_nelson_1971() with packaged dataset freitas1966")

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas1966, stv = STK, ry = RY, 
                                              plot = FALSE, tidy = TRUE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_false(inherits(cate_nelson_1971.test, "try-error"))
  
})

#######################################
# 6
## cate_nelson_1971
## Options plot = TRUE, tidy = FALSE
context("run cate_nelson_1971() with packaged dataset freitas1966")

cate_nelson_1971.test <- try(cate_nelson_1971(data = freitas1966, stv = STK, ry = RY, 
                                              plot = TRUE, tidy = FALSE),
                             silent = TRUE)

test_that("no error in fitting cate_nelson_1971() for the example dataset", {
  
  expect_false(inherits(cate_nelson_1971.test, "try-error"))
  
})

# 7
## boot_cate_nelson_1971
context("run boot_cate_nelson_1971() with packaged dataset freitas1966")

boot_cn_1971.test <- try(boot_cn_1971(
  data = freitas1966, stv = STK, ry = RY, n=2),
  silent = TRUE)

test_that("no error in fitting boot_cn_1971() for the example dataset", {
  
  expect_false(inherits(boot_cn_1971.test, "try-error"))
  
})