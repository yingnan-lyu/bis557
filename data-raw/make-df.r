lm_patho <- read.csv("df.csv")
dir.create("../data")
save(lm_patho, file = "../data/lm_patho.rda")

library(devtools)
ridge_test <- read.csv("~/Documents/Master2-2/BIS 557-Computational Statistics/homework 2/ridge_test.csv")
ridge_train <- read.csv("~/Documents/Master2-2/BIS 557-Computational Statistics/homework 2/ridge_train.csv")
devtools::use_data(ridge_test, ridge_train, overwrite = T)
