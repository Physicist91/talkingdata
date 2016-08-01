
# Setup
source("preprocessing.R")
source("run_xgb.R")

summary(data.train)


# creating xgb.dmatrices
train_list <- create_train_sparse(data.train)
train_sparse <- train_list[[1]]
group_map <- train_list[[2]]
labels <- train_list[[3]]
test_sparse <- create_test_sparse(data.test)

# run xgbcv
cv <- run_xgb_tree(train_sparse, labels=labels, "cv")
which.min(cv$test.mlogloss.mean + cv$test.mlogloss.std)

# run xgboost
preds <- run_xgb_tree(train_sparse, labels, "clf", test_sparse, group_map)

# append device_id to predictions
preds$device_id <- data.test$device_id

# writing the output
write.csv(preds, file=paste0("submission", Sys.Date(), ".csv"), row.names=FALSE)
