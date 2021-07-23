rm(list = ls())
library(ROCR)
##################

## Data ##

 train <- read.csv("raw/bweight_train.csv")
 test  <- read.csv("raw/X_test.csv")

 head(train)
 dim(train)
 str(train)
#monpre : 産前産後ケア(~カ月)
#npvis : 出産前訪問
#omaps : 1分後アふがースコア（なんかのスコアらしい）10-8が正常、7-4が軽症仮死、3~0は重症仮死
#fmaps ; 5分後のアふがースコア
#cigs : average cigarettes per day
#drink ; average drinks per week
 
 head(test)
 dim(test)

 
 
## Prediction ##

 log <- glm(lbw3000 ~., train, family = binomial)
 summary(log)
 s <- predict(log, newdata = test)
 head(s)

## AUC Cal ##
 
 true_y <- read.csv("raw/Y_test.csv")
 pred <- prediction(s, true_y)
 true_y
 auc <- performance(pred, "auc")
 auc@y.values[[1]]
 roc <- performance(pred, "tpr", "fpr")
 plor(roc)
 
## Submission ##

 submit <- read.csv("raw/submission.csv")
 submit$score <- s
 write.csv(submit, "submission.csv") 

