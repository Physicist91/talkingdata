lambdas <- c(0, 1, 2, 3, 4, 5, 6)
lambda_biases <- c(0, 1, 2, 3, 4, 5, 6)
alphas <- c(0, 1, 2, 3, 4, 5, 6)

output <- data.frame(lambda=integer(), lambda_bias=integer(), alpha=integer(), err=numeric(), n=integer())

for(lambda in lambdas){
  for(lambda_bias in lambda_biases) {
    for(alpha in alphas) {
      cat("Running xgb with ", lambda, lambda_bias, alpha, "\n")
      param <- list(booster="gblinear",
                    num_class=length(group_name),
                    objective="multi:softprob",
                    eval_metric="mlogloss",
                    eta=0.07,
                    lambda=lambda,
                    lambda_bias=lambda_bias,
                    alpha=alpha)
      watchlist <- list(train=dtrain)
      
      set.seed(114)
      fit_cv <- xgb.cv(params=param,
                       data=dtrain,
                       nrounds=100000,
                       watchlist=watchlist,
                       nfold=5,
                       early.stop.round=3,
                       verbose=0,
                       maximize=FALSE)
      best.err <- min(fit_cv$test.mlogloss.mean + fit_cv$test.mlogloss.std)
      best.n <- which.min(fit_cv$test.mlogloss.mean + fit_cv$test.mlogloss.std)
      temp <- data.frame(lambda=lambda, lambda_bias=lambda_bias, alpha=alpha, err=best.err, n=best.n)
      output <- rbind(output, temp)
    }
  }
}
