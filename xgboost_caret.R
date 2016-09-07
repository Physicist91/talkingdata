library(caret)

control <- trainControl(method="cv", number=5, verboseIter = TRUE, returnData = FALSE,
                        summaryFunction = mnLogLoss, classProbs = TRUE,
                        savePredictions="final")



# xgboost linear
grid1 <- expand.grid(nrounds=300,
                    eta=0.01,
                    lambda=1:6,
                    alpha=1:5)
set.seed(114)
fit1 <- train(x=train_data,
             y=as.factor(paste("a", train_label, sep="_")),
             trControl = control,
             method="xgbLinear",
             metric="logLoss",
             tuneGrid=grid1,
             maximize=FALSE)




# xgboost tree
grid2 <- expand.grid(nrounds=735,
                     eta=0.07,
                     max_depth=6,
                     colsample_bytree=0.7,
                     gamma=1,
                     min_child_weight=1)
set.seed(114)
fit2 <- train(x=train_data,
             y=as.factor(paste("a", train_label, sep="_")),
             trControl = control,
             method="xgbTree",
             metric="logLoss",
             tuneGrid=grid2,
             maximize=FALSE)




# stacked model
library(caretEnsemble)
tuneList <- list(
  xgbLinear=caretModelSpec(method="xgbLinear", tuneGrid=data.frame(.nrounds=237,
                                                                   .eta=0.01,
                                                                   .lambda=6,
                                                                   .alpha=1)),
  xgbTree=caretModelSpec(method="xgbTree", tuneGrid=data.frame(.nrounds=735,
                                                               .eta=0.1,
                                                               .max_depth=2,
                                                               .colsample_bytree=0.7,
                                                               .gamma=1,
                                                               .min_child_weight=1
                                                               ))
)

set.seed(114)
models <- caretList(x=train_data,
                    y=as.factor(paste("a", train_label, sep="_")),
                    trControl=control,
                    metric="logLoss",
                    tuneList=tuneList
                    )
                    
stack <- caretStack(models, method="glm", metric="logLoss",
                    trControl=trainControl(method="cv", number=5, savePredictions="final", classProbs=TRUE, summaryFunction=mnLogLoss)) 