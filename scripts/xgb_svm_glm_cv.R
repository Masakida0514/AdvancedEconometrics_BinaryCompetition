library(tidyverse)
library(caret)
library(glmnet)
library(e1071)

set.seed(514)
data_size <- nrow(train)
train_rate <- 0.75
train_i <- sample(1:data_size, size  = data_size * train_rate, replace = FALSE)
train_data <- train[train_i, ]
test_data <- train[-train_i, ]


#並列処理、使用するコア数4(使用できるコア数を知りたい場合は「detectCores()」を実行すればわかる)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

set.seed(514)
result <- train(
  highwage ~ .,                              #formula:目的変数と説明変数の指定          
  data = train_data,                        #トレーニングデータ
  metric = "auc",
  method = "xgbTree", 
  objective = "binary:logistic",
  trControl = trainControl(method = "cv"),  #クロスバリデーション
  tuneLength = 4                            #パラメータチューニングの範囲
)


s_gb <- predict(result, test_data, type="prob")
s_gb
pred_log <- prediction(s_gb[,2], test_data[,1])
auc_log <- performance(pred_log, "auc")
auc_log@y.values[[1]]
s_gb

n_fold <- 5
result <- matrix(0,n_fold,2)


fold_i <- sample(rep(1:n_fold, length.out = nrow(train_data)))

models <- c()

for (i in 1:n_fold) {
  validate_i <- which(fold_i == i)
  train_temp <- train_data[-validate_i, ]
  validate_temp <- train_data[validate_i, ]
  
  # logit
  model <- glm(highwage ~., train_temp, family = binomial(link="logit"))
  s_log <- predict(model, newdata = validate_temp[,-1], type="response")
  pred_log <- prediction(s_log, validate_temp[,1])
  auc_log <- performance(pred_log, "auc")
  result[i,1] <- max(auc_log@y.values[[1]])
  

  
  # svm
  svm <- ksvm(highwage ~., train_temp)
  s_svm <- predict(svm, newdata=validate_temp[,-1], "prob")[,2]
  pred <- prediction(s_svm, validate_temp[,1])
  auc <- performance(pred, "auc")
  result[i,3] <- auc@y.values[[1]]
}

summary(model)

# randomForest
rf <- randomForest(highwage ~ ., importance = T, train_data)
s_rf <- predict(rf, newdata=test_data[,-1], "prob")[,2]
s_rf
pred <- prediction(s_rf, test_data[,1])
pred
auc <- performance(pred, "auc")
auc@y.values[[1]]


