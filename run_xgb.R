library(caret)
library(dplyr)
library(Matrix)
library(xgboost)

## This is the main function that runs xgboost
run_xgb <- function(dtrain, cv_or_clf, dtest=NULL, group_map=NULL) {
  
  nclass <- 12

  param <- list(max.depth=5, eta=0.01, objective="multi:softprob", eval_metric="mlogloss",
                num_class=nclass, booster="gbtree")
  
  if(cv_or_clf == "cv") {
    cv <- xgb.cv(params=param,
                  data=dtrain,
                  nrounds=1000,
                  nfold=5,
                  early.stop.round=100,
                  maximize=FALSE,
                  colsample_bytree=0.7,
                  subsample=0.7)
    
  } else if(cv_or_clf == "clf" & !is.null(dtest) & !is.null(group_map)) {
    clf <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=70,
                     verbose=1,
                     print.every.n = TRUE)
    
    preds <- predict(clf, newdata=dtest)
    
    preds <- t(matrix(preds, nrow=nclass, ncol=length(preds)/nclass))
    preds <- as.data.frame(preds)
    names(preds) <- group_map$group
    
    return(preds);
    
  } else {
    stop("Error: invalid arguments")
  }
  
  
}


## This function creates the matrix of train data in the correct format for xgboost
##
create_dtrain <- function(data.train) {
  
  # throw away unnecessary columns
  data.train$age <- NULL
  data.train$gender <- NULL
  data.train$phone_brand <- NULL
  data.train$device_model <- NULL
  
  # one-hot encoding
  dummies <- dummyVars(~ phone_brand_en + day_mode, data=data.train)
  temp <- predict(dummies, newdata=data.train)
  data.train <- data.train %>%
    select(-phone_brand_en, -day_mode) %>%
    cbind(temp)
  
  # encode missing data
  data.train[is.na(data.train)] <- -9999
  
  # longitude and latitude as integer
  data.train$longitude <- as.integer(data.train$longitude)
  data.train$latitude <- as.integer(data.train$latitude)
  
  # group map to integer
  group_map <- data.frame(group=unique(data.train$group), group_num=unique(as.numeric(data.train$group) - 1))
  group_map <- group_map[order(group_map$group_num), ]
  data.train$group <- sapply(data.train$group, function(x) (group_map$group_num[group_map$group == x]))
  
  print(names(data.train))
  
  # create sparse matrix
  train.sparse <- sparse.model.matrix(group ~. -1, data=data.train[, -1])
  dtrain <- xgb.DMatrix(data=train.sparse, label=data.train$group, missing=-9999)
  
  dtrain
}


## This function creates the test data in the correct format for xgboost
create_dtest <- function(data.test) {
  
  # throw away unnecessary columns
  data.test$phone_brand <- NULL
  data.test$device_model <- NULL
  
  # create dummy group
  data.test$group <- -1
  
  # one-hot encoding
  dummies <- dummyVars(~ phone_brand_en + day, data=data.test)
  temp <- predict(dummies, newdata=data.test)
  data.test <- data.test %>%
    select(-phone_brand_en, -day) %>%
    cbind(temp)
  
  # encode missing data
  data.test[is.na(data.test)] <- -9999
  
  # convert longitude and latitude to integer
  data.test$longitude <- as.integer(data.test$longitude)
  data.test$latitude <- as.integer(data.test$latitude)
  
  # create sparse matrix
  test.sparse <- sparse.model.matrix(group ~. -1, data=data.test[, -1])
  dtest <- xgb.DMatrix(data=test.sparse, missing=-9999)
  
  dtest
}