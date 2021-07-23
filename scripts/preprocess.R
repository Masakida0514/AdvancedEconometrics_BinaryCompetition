library(dplyr)
##########################

## preprocess function ##
  # read train data as other name

preprocess <- function(train){
## categorical encode  ##
  #str(train)
  #unique(train$mrace)
  #unique(train$frace)
  
  # train$mrace <- as.factor(train$mrace)
  # train$frace <- as.factor(train$frace)
  
  # categorical encoding
  #train$mrace <- ifelse(train$mrace != "white", ifelse(train$mrace == "black",1,2),0)
  #train$frace <- ifelse(train$frace != "white", ifelse(train$frace == "black",1,2),0)
  
  # one-hot encoding
  train$mblack <- ifelse(train$mrace == "black",1,0)
  train$mwhite <- ifelse(train$mrace == "white",1,0)
  train$mother <- ifelse(train$mrace == "other",1,0)
  train$fblack <- ifelse(train$frace == "black",1,0)
  train$fwhite <- ifelse(train$frace == "white",1,0)
  train$fother <- ifelse(train$frace == "other",1,0)

## feature engineering
  # race
  train$ww <- train$mwhite * train$fwhite #両親w
  train$bb <- train$mblack * train$fblack #両親b
  train$wb <- train$mwhite * train$fblack #母親w父親b
  train$bw <- train$mblack * train$fwhite #母親b父親w 
  
  # drink cig
  train$cigger <- ifelse(train$cigs > 0, 1, 0)
  train$drunker <- ifelse(train$drink > 0, 1, 0)
  train$cigdri <- train$cigger * train$drunker
  
  # apgar score
  train$safeapg <- ifelse(train$fmaps >= 8, 1, 0)
  train$lightapg <- ifelse(train$fmaps >= 4 & train$fmaps < 8,1,0)
  train$heavyapg <- ifelse(train$fmaps < 4,1,0)
  
  # educ dummy
  train$muni <- ifelse(train$meduc >= 13,1,0)
  train$funi <- ifelse(train$feduc >= 13,1,0)
  train$mfuni <- train$muni * train$funi
  
  # age dummy
  #summary(train$mage)
  train$m10 <- ifelse(train$mage < 20,1,0)
  train$m20 <- ifelse(train$mage >= 20 & train$mage < 30,1,0)
  train$m30 <- ifelse(train$mage >= 30 & train$mage < 40,1,0)
  train$m40 <- ifelse(train$mage >= 40 & train$mage < 50,1,0)
  #summary(train$fage)
  train$f10 <- ifelse(train$fage < 20,1,0)
  train$f20 <- ifelse(train$fage >= 20 & train$fage < 30,1,0)
  train$f30 <- ifelse(train$fage >= 30 & train$fage < 40,1,0)
  train$f40 <- ifelse(train$fage >= 40 & train$fage < 50,1,0)
  train$f50 <- ifelse(train$fage >= 50 & train$fage < 60,1,0)
  train$f60 <- ifelse(train$fage >= 60 & train$fage < 70,1,0)
  
  train$mu22 <- ifelse(train$mage < 23,1,0)
  train$fu22 <- ifelse(train$fage < 23,1,0)
  train$youyou22 <- train$mu22 * train$fu22
  
  train$morem <- ifelse(train$mage > train$fage,1,0)
  train$moref <- ifelse(train$fage > train$mage,1,0)
  train$mequalf <- ifelse(train$mage == train$fage,1,0)
  
  train$moremdiff <- train$morem * (train$mage - train$fage)
  train$morefdiff <- train$moref * (train$fage - train$mage)
  
## drop frace and mrace ##
  frace.idx <- which(colnames(train)=="frace")
  train <- train[,-frace.idx]
  mrace.idx <- which(colnames(train)=="mrace")
  train <- train[,-mrace.idx]
    
  return(train)
}

