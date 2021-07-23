library(ROCR)
source("scripts/readdata.R")
## linear ##



#0.5692235
lbw3000 <- train$lbw3000
use_col <- which(bst_lasso$beta != 0) + 1
data <- cbind(lbw3000, train[,use_col])
log <- glm(lbw3000 ~., data, family = binomial(link="logit"))
summary(log)

#0.5508696
log2 <- glm(lbw3000 ~npvis+fother, data, family = binomial(link="logit"))
summary(log2)

# all variable:0.4865791
factors <- c(2,3,4,5,6,7,8,9,10,11,47,48)
train_2 <- lapply(train[,-factors], as.factor)
train_2 <- as.data.frame(train_2)
train_2 <- cbind(train_2, train[,factors])

test_2 <- lapply(test[,-factors], as.factor)
test_2 <- as.data.frame(test_2)
test_2 <- cbind(test_2, test[,factors])

levels(test_2$bw) <- levels(train_2$bw)
levels(test_2$heavyapg) <- levels(train_2$heavyapg)
levels(test_2$f60) <- levels(train_2$f60)

log3 <- glm(lbw3000 ~.,train_2,family = binomial(link="logit"))


s_log <- predict(log3, newdata = test_2[,-1], type="response")
pred_log <- prediction(s_log, tf_y)
acc_log <- performance(pred_log, "acc")
auc_log <- performance(pred_log, "auc")
roc_log <- performance(pred_log, "tpr", "fpr")
plot(acc_log)
max(acc_log@y.values[[1]])
max(auc_log@y.values[[1]])
plot(roc_log)
