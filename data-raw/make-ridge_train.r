ridge_train <- read.csv("ridge_train.csv")
dir.create("../data")
save(ridge_train, file = "../data/ridge_train.rda")





