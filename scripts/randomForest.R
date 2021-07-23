library(randomForest)
############################################


# 0.5956947
rf <- randomForest(lbw3000 ~ ., importance = T, train_2)
s_rf <- predict(rf, newdata=test_2[,-1], "prob")[,2]
pred <- prediction(s_rf, tf_y)
auc <- performance(pred, "auc")
auc@y.values[[1]]
dev.new()
varImpPlot(rf, type=1)

roc <- performance(pred, "tpr", "fpr")

plot(roc)

s_rf
