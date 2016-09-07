library(mlbench)
library(caret)
library(caretEnsemble)

data("Ionosphere")
dataset <- Ionosphere
dataset <- dataset[, -2]
dataset$V1 <- as.numeric(as.character(dataset$V1))

control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions = TRUE, classProbs=TRUE)
algorithmList <- rep('xgb', 5)