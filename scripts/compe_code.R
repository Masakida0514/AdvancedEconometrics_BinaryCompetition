## read data ##
train <- read.csv("comperaw/wage_train.csv", stringsAsFactors = T)
tmp <- train[,c(2,3,4,5,12)]
factor <- lapply(train[,-c(2,3,4,5,12)], as.factor)
train <- cbind(factor, tmp)

pred_test  <- read.csv("comperaw/X_test.csv", stringsAsFactors = T)
tmp <- pred_test[,c(2,3,4,5,12)]
factor <- lapply(pred_test[,-c(1,2,3,4,5,12)], as.factor)
pred_test <- cbind(factor, tmp)


#### XGBoost ####
#並列処理、使用するコア数4(使用できるコア数を知りたい場合は「detectCores()」を実行すればわかる)
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

set.seed(514)
result <- train(
  highwage ~ .,                              #formula:目的変数と説明変数の指定          
  data = train,                        #トレーニングデータ
  metric = "auc",
  method = "xgbTree", 
  objective = "binary:logistic",
  trControl = trainControl(method = "cv"),  #クロスバリデーション
  tuneLength = 4                            #パラメータチューニングの範囲
)


s_gb <- predict(result, pred_test, type="prob")
s_gb
submission <- read.csv("comperaw/submission.csv")
submission$score <- s_gb[,2]
write.csv(submission, "subs/sub_gbdt.csv")


#### randomforest ####
rf <- randomForest(highwage ~ ., importance = T, train)
s_rf <- predict(rf, newdata=pred_test, "prob")[,2]
submission$score <- s_rf
write.csv(submission, "subs/sub_randomForest.csv")


#### ensemble ####
gb_sub <- read.csv("subs/sub_gbdt.csv")
rf_sub <- read.csv("subs/sub_randomForest.csv")
final <- gb_sub * 0.5 + rf_sub * 0.5
write.csv(final, "subs/sub_final.csv")


head(final)
Y_test <- read.csv("subs/Y_test.csv")
pred <- prediction(final[,3], Y_test[,2])
auc <- performance(pred, "auc")
auc@y.values[[1]]
head(rf_sub)

#### opt. autml ####
#install.packages("h2o")
library(h2o)

set.seed(514)
data_size <- nrow(train)
train_rate <- 0.75
train_i <- sample(1:data_size, size  = data_size * train_rate, replace = FALSE)
train_data <- train[train_i, ]
test_data <- train[-train_i, ]

# start h2o cluster
invisible(h2o.init())

# convert data as h2o type
train_h = as.h2o(train_data)
test_h = as.h2o(test_data)

# set label type
y = 'highwage'
pred = setdiff(names(train_data), y)

# Run AutoML for 20 base models
aml = h2o.automl(x = pred, y = y,
                 training_frame = train_h,
                 max_models = NULL,
                 stopping_metric = "AUC",
                 sort_metric = "AUC",
                 seed = 1,
                 max_runtime_secs = 30
)
aml
# AutoML Leaderboard
lb = aml@leaderboard
head(lb$model_id, 10)
# prediction result on test data
pred_y = h2o.predict(aml@leader, test_h[,-1]) %>% as.data.frame()

# create a confusion matrix
caret::confusionMatrix(test_data$highwage, prediction$predict)

# check auc
pred <- prediction(pred_y["p1"], test_data[,1])

auc <- performance(pred, "auc")
auc@y.values[[1]]


# close h2o connection
h2o.shutdown(prompt = F)
