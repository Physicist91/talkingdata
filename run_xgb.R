library(caret)
library(dplyr)
library(Matrix)
library(xgboost)



## This function creates sparse matrices for xgboost
##
create_sparse_matrix <- function(dataset){
  
  #one-hot encoding
  dummies <- dummyVars(~ phone_brand_en, data=data.train)
  temp <- predict(dummies, newdata=dataset)
  dataset <- dataset %>%
    select(-phone_brand_en) %>%
    cbind(temp)
  

  
  # encoding missing values for xgboost
  dataset[is.na(dataset)] <- -9999
  
  # create dummy label for test data
  if(is.null(dataset$group)) {
    dataset$group <- -1
  }
  
  names(dataset) <- gsub("^-", "N_", names(dataset))
  names(dataset) <- gsub("^[0-9]", "P_\\1", names(dataset))
  
  print(summary(dataset))
  
  sparse_data <- sparse.model.matrix(group ~. -1, data=dataset[, -1])
  
  sparse_data
}




## This is the function that runs xgboost (linear)
run_xgb_linear <- function(train_sparse, labels, cv_or_clf, test_sparse=NULL, group_map=NULL) {
  
  nclass <- 12
  
  param <- list(eta=0.05, alpha=3, max_depth=6, objective="multi:softprob", eval_metric="mlogloss",
                num_class=nclass, booster="gblinear")
  
  dtrain <- xgb.DMatrix(data=train_sparse, label=labels, missing=-9999)
  
  if(cv_or_clf == "cv") {
    cv <- xgb.cv(params=param,
                 data=dtrain,
                 nrounds=2000,
                 nfold=5,
                 early.stop.round=100,
                 maximize=FALSE,
                 nthread=24)
    
  } else if(cv_or_clf == "clf" & !is.null(dtest) & !is.null(group_map)) {
    clf <- xgb.train(params=param,
                     data=dtrain,
                     nrounds=600,
                     verbose=1,
                     print.every.n = TRUE,
                     nthread=24)
    
    dtest <- xgb.DMatrix(data=test_sparse, missing=-9999)
    preds <- predict(clf, newdata=dtest)
    
    preds <- t(matrix(preds, nrow=nclass, ncol=length(preds)/nclass))
    preds <- as.data.frame(preds)
    names(preds) <- group_map$group
    
    return(preds);
    
  } else {
    stop("Error: invalid arguments")
  }
  
  
}



## This is the main function that runs xgboost (tree)
run_xgb_tree <- function(train_sparse, labels, cv_or_clf, test_sparse=NULL, group_map=NULL) {

  nclass <- 12

  param <- list(max_depth=5, eta=0.05, objective="multi:softprob", eval_metric="mlogloss",
                num_class=nclass, booster="gbtree")


  dtrain <- xgb.DMatrix(data=train_sparse, label=labels, missing=-9999)

  if(cv_or_clf == "cv") {
    cv <- xgb.cv(params=param,
                  data=dtrain,
                  nrounds=2000,
                  nfold=5,
                  early.stop.round=100,
                  maximize=FALSE,
                  colsample_bytree=0.7,
                  subsample=0.7,
                 nthread=24)

    return(cv)

  } else if(cv_or_clf == "clf" & !is.null(test_sparse) & !is.null(group_map)) {
    clf <- xgb.train(params=param,
                     data=dtrain,
                     verbose=2,
                     print.every.n = TRUE,
                     nrounds=best.n,
                     colsample_bytree=0.7,
                     subsample=0.7,
                     nthread=24)

    dtest <- xgb.DMatrix(data=test_sparse, missing=-9999)

    preds <- predict(clf, newdata=dtest)

    preds <- t(matrix(preds, nrow=nclass, ncol=length(preds)/nclass))
    preds <- as.data.frame(preds)
    names(preds) <- group_map$group

    return(preds);

  } else {
    stop("Error: invalid arguments")
  }


}

