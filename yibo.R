# Courtesy of Yibo: 

options(stringsAsFactors=F,scipen=99)
rm(list=ls());gc()
library(data.table)

#####################################################################
## Brands & Labels 
#####################################################################

label_train <- fread("raw_data/gender_age_train.csv",
                     colClasses=c("character","character",
                                  "integer","character"))
label_test <- fread("raw_data/gender_age_test.csv",
                    colClasses=c("character"))
label_test$gender <- label_test$age <- label_test$group <- NA
label <- rbind(label_train,label_test)
setkey(label,device_id)
rm(label_test,label_train);gc()

brand <- fread("raw_data/phone_brand_device_model.csv",
               colClasses=c("character","character","character"))
setkey(brand,device_id)
brand0 <- unique(brand, by=NULL) #remove duplicated rows
brand0 <- brand0[sample(nrow(brand0)),] #scramble the records
brand2 <- brand0[-which(duplicated(brand0$device_id)),] #remove duplicated device ids
label1 <- merge(label, brand2, by="device_id", all.x=TRUE)
rm(brand, brand0, brand2); gc()


#####################################################################
## Apps
#####################################################################

events <- fread("raw_data/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkeyv(events, c("device_id", "event_id"))
event_app <- fread("raw_data/app_events.csv",
                   colClasses=rep("character",4))
setkey(event_app,event_id)

#event_app <- event_app[event_app$is_active == 1, ]
events <- unique(events[,list(device_id,event_id)],by=NULL)
event_apps <- event_app[,list(apps=paste(unique(app_id),collapse=",")),by="event_id"]
device_event_apps <- merge(events,event_apps,by="event_id")
rm(events,event_app,event_apps);gc()

f_split_paste <- function(z){paste(unique(unlist(strsplit(z,","))),collapse=",")}
device_apps <- device_event_apps[,list(apps=f_split_paste(apps)),by="device_id"]
rm(device_event_apps,f_split_paste);gc()

tmp <- strsplit(device_apps$apps,",")
device_apps <- data.table(device_id=rep(device_apps$device_id,
                                        times=sapply(tmp,length)),
                          app_id=unlist(tmp))
rm(tmp)





######################################################################
## Location
######################################################################

events <- fread("raw_data/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkeyv(events, c("device_id", "event_id"))
ll <- events[abs(events$longitude) > 0.01 | abs(events$latitude) > 0.01,]
ll <- ll[, list(longitude=mean(longitude), latitude=mean(latitude)), by="device_id"]
rm(events)
set.seed(20)
clusters <- kmeans(ll[, list(longitude, latitude)], 75, iter.max=20, nstart=20)


######################################################################
## Time
######################################################################

##All the hours a device is being used

events <- read.csv("raw_data/events.csv", colClasses = c("character", "character", "character", "numeric", "numeric"))

events$timestamp <- strptime(events$timestamp, format="%Y-%m-%d %H:%M:%S")
events$day <- weekdays(events$timestamp)
events$hour <- as.numeric(format(events$timestamp, "%H"))

device_hour <- data.table(device_id=events$device_id, hour=events$hour)
device_day <- data.table(device_id=events$device_id, day=events$day)
rm(events)


######################################################################
## App label id
######################################################################

app_labels <- fread("raw_data/app_labels.csv",
                    colClasses = c("character", "character"))
setkey(app_labels, app_id)
event_app <- fread("raw_data/app_events.csv",
                   colClasses=rep("character",4))
setkeyv(event_app, c("event_id", "app_id"))
events <- fread("raw_data/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkeyv(events, c("device_id", "event_id"))
events <- unique(events[,list(device_id,event_id)],by=NULL)


app_labels <- app_labels[, list(labels=paste(unique(label_id), collapse=",")), by="app_id"]
id_labels <- merge(event_app, app_labels, by="app_id")


event_apps <- id_labels[,list(apps=paste(unique(labels),collapse=",")),by="event_id"]
device_event_apps <- merge(events,event_apps,by="event_id")
rm(events,event_app,event_apps, app_labels, id_labels);gc()

f_split_paste <- function(z){paste(unique(unlist(strsplit(z,","))),collapse=",")}
device_app_labels <- device_event_apps[,list(apps=f_split_paste(apps)),by="device_id"]
rm(device_event_apps,f_split_paste);gc()

tmp <- strsplit(device_app_labels$apps,",")
device_app_labels <- data.table(device_id=rep(device_app_labels$device_id,
                                              times=sapply(tmp,length)),
                                app_label=unlist(tmp))
rm(tmp)



# dummy
d1 <- label1[,list(device_id,phone_brand)]
label1$phone_brand <- NULL
d2 <- label1[,list(device_id, device_model)]
label1$device_model <- NULL
d3 <- device_apps
rm(device_apps)
d4 <- cbind(ll[, list(device_id)], location=clusters$cluster)
rm(ll, clusters)
d5 <- device_hour
rm(device_hour)
d6 <- device_day
rm(device_day)
d7 <- device_app_labels
rm(device_app_labels)
d1[,phone_brand := paste0("phone_brand:", phone_brand)]
d2[,device_model := paste0("device_model:", device_model)]
d3[,app_id:=paste0("app_id:", app_id)]
d4[, location := paste0("location:",location)]
d5[,hour:=paste0("device_hour:", hour)]
d6[,day:=paste0("device_day:", day)]
d7[,app_label:=paste0("app_label:", app_label)]
names(d1) <- names(d2) <- names(d3) <- names(d4) <- names(d5) <- names(d6) <- names(d7) <- c("device_id", "feature_name")
dd <- rbind(d1, d2, d3, d4, d5, d6, d7); gc()
rm(d1,d2,d3,d4,d5,d6,d7);gc()

library(Matrix)
ii <- unique(dd$device_id)
jj <- unique(dd$feature_name)
id_i <- match(dd$device_id, ii)
id_j <- match(dd$feature_name, jj)
id_ij <- cbind(id_i,id_j)
M <- Matrix(0,nrow=length(ii),ncol=length(jj),
            dimnames=list(ii,jj),sparse=T)
M[id_ij] <- 1
rm(ii,jj,id_i,id_j,id_ij,dd);gc()

x <- M[rownames(M) %in% label1$device_id,]
id <- label1$device_id[match(rownames(x),label1$device_id)]
y <- label1$group[match(rownames(x), label1$device_id)]
rm(M, label1)


# level reduction
x_train <- x[!is.na(y),]
tmp_cnt_train <- colSums(x_train)
x <- x[, tmp_cnt_train>0 & tmp_cnt_train<nrow(x_train)]
rm(x_train, tmp_cnt_train)

library(xgboost)
group_name <- na.omit(unique(y))
idx_train <- which(!is.na(y))
idx_test <- which(is.na(y))
train_data <- x[idx_train,]
test_data <- x[idx_test,]
train_label <- match(y[idx_train], group_name)-1
test_label <- match(y[idx_test], group_name)-1
dtrain <- xgb.DMatrix(train_data, label=train_label, missing=NA)
dtest <- xgb.DMatrix(test_data, label=test_label, missing=NA)
rm(x,y); gc()

  param_linear <- list(booster="gblinear",
                       num_class=length(group_name),
                       objective="multi:softprob",
                       eval_metric="mlogloss",
                       eta=0.01,
                       lambda=6,
                       lambda_bias=5,
                       alpha=2)
  
  param_tree <- list(booster="gbtree",
                     num_class=length(group_name),
                     objective="multi:softprob",
                     eval_metric="mlogloss",
                     eta=0.1, 
                     max_depth=2,
                     colsample_bytree=0.7,
                     subsample=0.7)
  
  watchlist <- list(train=dtrain)
  
  # set.seed(114)
  # fit_cv <- xgb.cv(params=param,
  #                  data=dtrain,
  #                  nrounds=100000,
  #                  watchlist=watchlist,
  #                  nfold=5,
  #                  early.stop.round=3,
  #                  verbose=1,
  #                  maximize=FALSE)
  # best.n <- which.min(fit_cv$test.mlogloss.mean + fit_cv$test.mlogloss.std)
  best.ntree <- 735
  best.nlinear <- 237
  
  set.seed(114)
  fit_xgb_tree <- xgb.train(params=param_tree,
                            data=dtrain,
                            nrounds=best.ntree,
                            watchlist=watchlist,
                            verbose=1,
                            nthread=24)
  fit_xgb_linear <- xgb.train(params=param_linear,
                              data=dtrain,
                              nrounds=best.nlinear,
                              watchlist=watchlist,
                              verbose=1,
                              nthread=24)
  
  
  
  
  pred <- 0.6 * predict(fit_xgb_linear, dtest) + 0.4 * predict(fit_xgb_tree, dtest)


# format submission and write to disk
pred_detail <- t(matrix(pred, nrow=length(group_name)))
res_submit <- cbind(id=id[idx_test],as.data.frame(pred_detail))
colnames(res_submit) <- c("device_id", group_name)
write.csv(res_submit, file=paste0("submission", Sys.Date(), ".csv"), row.names=FALSE, quote=FALSE)
