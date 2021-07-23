library("MlBayesOpt")
library(xgboost)
install.packages("doParallel")
install.packages("caret")
library(doParallel)
library(caret)

res0 <- xgb_cv_opt(data = train,
                   label = highwage,
                   objectfun = "binary:logistic",
                   evalmetric = "auc",
                   metric = "auc",
                   n_folds = 5)

dim(train)
train[,1]

str(train)
