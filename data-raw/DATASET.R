## code to prepare `DATASET` dataset goes here

# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(magrittr, dplyr, usethis, data.table, here)

# data test 1 ----
data_test <- read.csv(here::here("data-raw","data_test.csv")) %>% as.data.table

# write data in correct format to data folder ----
usethis::use_data(data_test, overwrite = TRUE)

# data test 2, Freitas et al 1966. May 1966. Determination of Potassium Deficient Areas
# for Cotton. Potash Review ----
freitas.1966 <- read.csv(here::here("data-raw","freitas.1966.csv")) %>% as.data.table

# write data in correct format to data folder ----
usethis::use_data(freitas.1966, overwrite = TRUE)


