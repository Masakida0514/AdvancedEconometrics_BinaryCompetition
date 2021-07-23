library(glmnet)
source("scripts/readdata.R")
library(caTools)

## Variable Selection by Lasso ##
  X <- as.matrix(train[,-1])
  Y <- as.matrix(train[,1])
  lasso <- cv.glmnet(X, Y, family = "gaussian", alpha = 1)
  #lasso$lambda.min
  bst_lasso <- glmnet(X,Y, family = "gaussian",
                        lambda = lasso$lambda.min,
                        alpha = 1)
  #bst_lasso$beta #係数を表示

  data <- X[,which(bst_lasso$beta != 0)]  
  #data <- cbind(train[,1], data)


## lightgbm ##
  

  #dataset
  ## train_test_split ##
  # smp_size <- floor(0.75 * nrow(data))
  # set.seed(514)
  # train_ind <- sample(seq_len(nrow(data)), size = smp_size)
  # 
  # train_val <- data[train_ind, ]
  # test_val <- data[-train_ind, ]
  # 
  #lgbm.train <- lgb.Dataset(data = as.matrix(train_val[,-1]), label = train_val[,1])
  #lgbm.test <- lgb.Dataset(data = as.matrix(test_val[,-1]), label = test_val[,1])
  
  ## lightgbm ##
  bst <- lightgbm(
    feature_pre_filter = FALSE,
    data = data,
    label = train$lbw3000,
    num_leaves = 25,
    learning_rate = 0.001,
    nrounds = 50000,
    min_child_samples = 3,
    early_stopping_rounds = 100,
    objective = "binary",
    metric = "auc")
  
  
  ## Prediction ##
  test <- test[,-1]
  test <- test[,which(bst_lasso$beta != 0)] 
  
  s_lgbm <- predict(bst, as.matrix(test))
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