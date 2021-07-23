library(dplyr)
library(tree)
library(randomForest)
library(lightgbm)
library(caTools)
#######################


  
## train_test_split ##

smp_size <- floor(0.75 * nrow(train))
set.seed(514)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)

train_val <- train[train_ind, ]
test_val <- train[-train_ind, ]

dim(train_val)
dim(test_val)


## lightgbm ##

lgbm_train <- lgb.Dataset(data = as.matrix(train_val[,-1]), label = train_val[,1])
lgbm_test <- lgb.Dataset(data = as.matrix(test_val[,-1]), label = test_val[,1])
#valids = list(test = lgbm_test)

bst <- lightgbm(
  data = lgbm_train,
  label = lgbm_test,
  num_leaves = 25,
  learning_rate = 0.001,
  nrounds = 50000,
  objective = "binary",
  metric = "auc"
)
## Prediction ##

s_lgbm <- predict(bst, as.matrix(test[,-1]))
#head(s_lgbm)
#plot(s_lgbm)
tf_lgbm <- ifelse(s_lgbm >= 0.5,1,0)
#head(tf_lgbm)

## AUC Cal ##

true_y <- read.csv("raw/Y_test.csv")
tf_y <- ifelse(true_y[,2] == TRUE,1,0)

pred <- prediction(tf_lgbm, tf_y)

auc <- performance(pred, "auc")
auc@y.values[[1]]
roc <- performance(pred, "tpr", "fpr")
plot(roc)




## Prediction ##

s_lgbm <- predict(lgb.model.cv$boosters[[1]], as.matrix(test[,2:13]))
head(s_lgbm)
plot(s_lgbm)
tf_lgbm <- ifelse(s_lgbm >= 0.5,1,0)
head(tf_lgbm)

## AUC Cal ##

pred <- prediction(tf_lgbm, tf_y)

auc <- performance(pred, "auc")
auc@y.values[[1]]
roc <- performance(pred, "tpr", "fpr")
plot(roc)

## matrix test ##
data <- as.matrix(as.integer(train[,2:13]))
head(train[,2:13])
str(train[,2:13])
as.integer(train[,])
str(data)
head(data)

data <- sapply(train[,2:13], as.matrix)
str(data)
head(data)
data
