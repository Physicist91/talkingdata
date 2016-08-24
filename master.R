# Setup
setwd("~/talkingdata")
source("preprocessing.R")
source("run_xgb.R")

summary(data.train1)
summary(data.train2)

# mapping categorical group to ordered integer 0 -12
group_map <- read.csv("processed_data/group_map.csv")
group_map

data.train$group <- sapply(data.train$group, function(x) (group_map$group_num[group_map$group == x]))
labels <- data.train$group

# creating sparse matrices
train_sparse <- create_sparse_matrix(data.train)
test_sparse <- create_sparse_matrix(data.test)

# run xgbcv
cv <- run_xgb_tree(train_sparse, labels=labels, "cv")
best.n <- which.min(cv$test.mlogloss.mean + cv$test.mlogloss.std)
#cv <- run_xgb_linear(train_sparse, labels=labels, "cv")


# run xgboost
preds <- run_xgb_tree(train_sparse, labels, "clf", test_sparse, group_map)

# variable importance


# append device_id to predictions
preds$device_id <- data.test$device_id

# writing the output
write.csv(preds, file=paste0("submission", Sys.Date(), ".csv"), row.names=FALSE)
