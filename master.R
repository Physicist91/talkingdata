
# load the data
source("preprocessing.R")


# creating xgb.dmatrices
dtrain <- create_dtrain(data.train)
dtest <- create_dtest(data.test)

# run xgboost
preds <- run_xgb(dtrain, "clf", dtest, group_map)

# append device_id to predictions
preds$device_id <- data.test$device_id

# writing the output
write.csv(preds, file=paste0("submission", Sys.Date(), ".csv"), row.names=FALSE)
