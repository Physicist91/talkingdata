library(dplyr)
library(reshape2)

#################################
# getting data

gender_age_train <- read.csv("raw_data/gender_age_train.csv", colClasses = c("factor", "factor", "integer", "factor"))
gender_age_test <- read.csv("raw_data/gender_age_test.csv", colClasses="factor")
events <- read.csv("raw_data/events.csv", colClasses = c("integer", "factor", "factor", "numeric", "numeric"))
app_events <- read.csv("raw_data/app_events.csv", colClasses = c("integer", "factor", "integer", "integer"))
label_categories <- read.csv("raw_data/label_categories.csv")
phone_brand_device_model <- read.csv("raw_data/phone_brand_device_model.csv", colClasses=c(rep("factor", 3)))
app_labels <- read.csv("raw_data/app_labels.csv", colClasses=c("factor", "integer"))

#################################
##Feature Engineering
##
## Below we have
## a. app_count: app count for each device
## b. top500_app: usage of top n popular apps for each device
## c. brand_model: statistics of the user demographics by device model

# Part 1: App usage behavior

# average app usage and installed app on every device
app_count <- app_events %>%
  group_by(event_id) %>%
  summarise(active_count=sum(is_active), app_count=sum(is_installed)) %>%
  merge(events, ., by='event_id') %>%
  group_by(device_id) %>%
  summarise(active_count=round(mean(active_count, na.rm=T)),
            app_count=round(mean(app_count, na.rm=T)))

# most frequent app
top500_app <- app_events %>%
  filter(is_active == 1) %>%
  group_by(app_id) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(desc(count))
top500_app <- top500_app[1:200,] #not necessarily 500 :-)
device_app <- app_events %>%
  filter(is_active == 1) %>%
  merge(events, ., by="event_id") %>%
  group_by(device_id, app_id) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(app_id %in% top500_app$app_id)
device_app_wide <- dcast(data=device_app, device_id ~ app_id, value.var = "count", fill=0)



#geo+temporal data
# events$timestamp <- strptime(events$timestamp, format="%Y-%m-%d %H:%M:%S")
# events$weekday <- weekdays(events$timestamp)
# events$hour <- as.numeric(format(events$timestamp, "%H"))
# events$date <- strftime(events$timestamp, format="%m%d")
# events <- events %>%
#   select(-timestamp) %>%
#   group_by(device_id) %>%
#   summarise(longitude=median(longitude), latitude=median(latitude),
#             day_mode=weekday[which.max(table(weekday))], hour_mode=hour[which.max(table(hour))],
#             event_count=n(), date_count=length(unique(date)),
#             event_per_day = event_count/date_count,
#             day_count=length(unique(weekday)), hour_count=length(unique(hour)))
# events[events$longitude == 0, "longitude"] <- NA
# events[events$latitude == 0, "latitude"] <- NA
# events$longitude <- as.integer(events$longitude)
# events$latitude <- as.integer(events$latitude)

# Part 2: phone characteristics
phone_brand_device_model <- phone_brand_device_model[!duplicated(phone_brand_device_model$device_id),]
cn_en <- read.csv("processed_data/cn_en.csv", stringsAsFactors = FALSE)
cn_en$chinese <- enc2utf8(cn_en$chinese)
phone_brand_device_model$phone_brand <- enc2utf8(as.character(phone_brand_device_model$phone_brand))
phone_brand_device_model$device_model <- enc2utf8(as.character(phone_brand_device_model$device_model))
phone_brand_device_model$phone_brand_en <- sapply(phone_brand_device_model$phone_brand, function(x){ifelse(any(grepl(x, cn_en$chinese, fixed=TRUE)),toupper(cn_en[grep(x, cn_en$chinese, fixed = TRUE), "english"]), "Others")})
phone_brand_device_model$phone_brand_en <- gsub(" ", "", phone_brand_device_model$phone_brand_en)

model_stats <- merge(phone_brand_device_model, gender_age_train, by="device_id", all.x=TRUE) %>%
  group_by(device_model) %>%
  summarise(model_male_count=sum(gender == "M", na.rm=TRUE),
            model_female_count=sum(gender == "F", na.rm=TRUE),
            model_gender_diff=model_male_count-model_female_count,
            model_average_age=mean(age, na.rm=TRUE),
            model_median_age=median(age, na.rm=TRUE)) %>%
  merge(phone_brand_device_model, ., by="device_model", all.x=TRUE) %>%
  select(device_id, device_model, model_male_count, model_female_count, model_gender_diff,
         model_average_age, model_median_age)

# some NAs are due to no gender/age data available for the model
model_stats <- model_stats[!is.na(model_stats$model_average_age),]

brand_stats <- merge(phone_brand_device_model, gender_age_train, by="device_id", all.x=TRUE) %>%
  group_by(phone_brand_en) %>%
  summarise(brand_male_count=sum(gender == "M", na.rm=TRUE),
            brand_female_count=sum(gender == "F", na.rm=TRUE),
            brand_gender_diff=brand_male_count-brand_female_count,
            brand_average_age=mean(age, na.rm=TRUE),
            brand_median_age=median(age, na.rm=TRUE)) %>%
  merge(phone_brand_device_model, ., by="phone_brand_en", all.x=TRUE) %>%
  select(device_id, phone_brand_en, brand_male_count, brand_female_count, brand_gender_diff,
         brand_average_age, brand_median_age)



###################################
# joining data
# 1 is with apps
# 2 is without apps

data.train1 <- gender_age_train[gender_age_train$device_id %in% events$device_id, ]
data.test1 <- gender_age_test %>% filter(gender_age_test$device_id %in% events$device_id)
data.train2 <- gender_age_train[!gender_age_train$device_id %in% events$device_id, ]
data.test2 <- gender_age_test %>% filter(!gender_age_test$device_id %in% events$device_id)

# device characteristics
data.train1 <- merge(data.train1, brand_stats, by="device_id", all.x=TRUE)
data.test1 <- merge(data.test1, brand_stats, by="device_id", all.x=TRUE)
data.train2 <- merge(data.train2, brand_stats, by="device_id", all.x=TRUE)
data.test2 <- merge(data.test2, brand_stats, by="device_id", all.x=TRUE)

data.train1 <- merge(data.train1, model_stats, by="device_id", all.x=TRUE)
data.test1 <- merge(data.test1, model_stats, by="device_id", all.x=TRUE)
data.train2 <- merge(data.train2, model_stats, by="device_id", all.x=TRUE)
data.test2 <- merge(data.test2, model_stats, by="device_id", all.x=TRUE)

# app usage
data.train1 <- merge(data.train1, app_count, by="device_id", all.x=TRUE)
data.test1 <- merge(data.test1, app_count, by="device_id", all.x=TRUE)
data.train1 <- merge(data.train1, device_app_wide, by="device_id", all.x=TRUE)
data.test1 <- merge(data.test1, device_app_wide, by="device_id", all.x=TRUE)

# geo+temporal data
# data.train <- merge(data.train, events, by="device_id", all.x=TRUE)
# data.test <- merge(data.test, events, by="device_id", all.x=TRUE)

####################################
# Post-processing

# cleanup
rm(app_count,
   app_events,
   app_labels,
   events,
   label_categories,
   phone_brand_device_model,
   cn_en,
   device_app,
   device_app_wide,
   brand_model,
   top500_app)

data.train1$gender <- NULL
data.train1$age <- NULL
data.train1$phone_brand <- NULL
data.train1$phone_brand_en <- NULL
data.train1$device_model <- NULL
data.test1$phone_brand <- NULL
data.test1$phone_brand_en <- NULL
data.test1$device_model <- NULL

data.train2$gender <- NULL
data.train2$age <- NULL
data.train2$phone_brand <- NULL
data.train2$phone_brand_en <- NULL
data.train2$device_model <- NULL
data.test2$phone_brand <- NULL
data.test2$phone_brand_en <- NULL
data.test2$device_model <- NULL
