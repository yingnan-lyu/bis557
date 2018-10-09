lm_patho <- read.csv("ridge_train.csv")
dir.create("../data")
save(lm_patho, file = "../data/ridge_train.rda")





