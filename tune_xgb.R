library(xgboost)
source("preprocessing.R")
source("run_xgb.R")

# initialization
nclass <- 12

# setup
dtrain <- create_dtrain(data.train)

# Tune the hell out of xgb
max_depths <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)
etas <- c(0.01, 0.05, 0.1, 0.5)

out <- data.frame()

for(max_depth in max_depths) {
  
  cv <- tune_xgb(dtrain, max_depth, 0.1 )
  
  best <- which.min(cv$test.mlogloss.mean + cv$test.mlogloss.std)
  temp <- c(max_depth, 0.1, cv$test.mlogloss.mean[best], cv$test.mlogloss.std[best])
  out <- rbind(out, temp)
       
  cat("Max Depth: ", max_depth, "\n", temp, "\n")
  

}


names(out) <- c("max_depth", "eta", "logloss_mean", "logloss_std")

out


### Helper function to optimize xgboost

tune_xgb <- function(dtrain, max_depth, eta) {
  
  nclass <- 12
  
  param <- list(max.depth=max_depth, eta=eta, objective="multi:softprob", eval_metric="mlogloss",
                num_class=nclass, booster="gbtree")
  
  cv <- xgb.cv(params=param,
                 data=dtrain,
                 nrounds=500,
                 nfold=5,
                 early.stop.round=10,
                 maximize=FALSE,
               nthread=24)
    
  
  cv

}