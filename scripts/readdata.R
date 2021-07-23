rm(list = ls())

## read data ##
tmp <- read.csv("raw/bweight_train.csv")
test  <- read.csv("raw/X_test.csv")
source("scripts/preprocess.R")

train <- preprocess(tmp)
test <- preprocess(test)
rm(tmp)

train$lbw3000 <- ifelse(train$lbw3000 == TRUE,1,0)

true_y <- read.csv("raw/Y_test.csv")
tf_y <- ifelse(true_y[,2] == TRUE,1,0)


## read data ##
train <- read.csv("comperaw/wage_train.csv", stringsAsFactors = T)
tmp <- train[,c(2,3,4,5,12)]
factor <- lapply(train[,-c(2,3,4,5,12)], as.factor)
str(factor)
train <- cbind(factor, tmp)

pred_test  <- read.csv("comperaw/X_test.csv", stringsAsFactors = T)
tmp <- pred_test[,c(2,3,4,5,12)]
factor <- lapply(pred_test[,-c(1,2,3,4,5,12)], as.factor)
pred_test <- cbind(factor, tmp)



submission <- read.csv("comperaw/submission.csv")
dim(train)
head(train)
head(pred_test)
str(train)
unique(train$education)
