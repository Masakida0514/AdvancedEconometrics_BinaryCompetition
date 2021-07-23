library(Matrix)
library(R6)
library(lightgbm)
library(ROCR)
source("scripts/readdata.R")
####################################################

sparse.train <- sparse.model.matrix(lbw3000 ~ ., data = train)
lgb.train <- lgb.Dataset(sparse.train, label = train$lbw3000)

test$tf_y <- tf_y

sparse.test <- sparse.model.matrix(tf_y ~ ., data = test)
lgb.test <- lgb.Dataset(sparse.test, label = tf_y)


## lgbm cv ##
lgb.grid = list(objective = "binary",
                num_leaves = 25,
                learning_rate = 0.001,
                colsample_bytree = 0.25,
                metric = "auc")

lgb.model.cv = lgb.cv(params = lgb.grid,
                      data = lgb.train,
                      num_leaves = 500,
                      num_threads = 4 ,
                      nrounds = 10000,
                      early_stopping_rounds = 1000,
                      nfold = 3,
                      stratified = TRUE,
                      verbose = 1,
                      eval = "auc")
lgb.model.cv




## lightgbm ##
bst <- lightgbm(
  feature_pre_filter = FALSE,
  data = lgb.train,
  label = lgb.test,
  num_leaves = 25,
  learning_rate = 0.001,
  nrounds = 100,
  min_child_samples = 3,
  objective = "binary",
  metric = "auc")
## Prediction ##
bst

s_lgbm <- predict(bst, as.matrix(test[,-1]))
#head(s_lgbm)
#plot(s_lgbm)
#head(tf_lgbm)

## AUC Cal ##

pred <- prediction(s_lgbm, tf_y)

auc <- performance(pred, "auc")
auc@y.values[[1]]
roc <- performance(pred, "tpr", "fpr")
dev.new()
plot(roc)
dev.off()
