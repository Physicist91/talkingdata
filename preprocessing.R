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
# Feature Engineering

# Part 1: App usage behavior

# average app usage and installed app on every device
app_count <- app_events %>%
  group_by(event_id) %>%
  summarise(active_count=sum(is_active), app_count=sum(is_installed)) %>%
  merge(events, ., by='event_id', all.x=TRUE) %>%
  group_by(device_id) %>%
  summarise(active_count=round(mean(active_count, na.rm=T)),
            app_count=round(mean(app_count, na.rm=T)))

# most frequent app
top100_app <- app_events %>%
  filter(is_active == 1) %>%
  group_by(app_id) %>%
  summarise(count=n()) %>%
  ungroup() %>%
  arrange(desc(count))

top100_app <- top100_app[1:100,]

device_app <- app_events %>%
  filter(is_active == 1) %>%
  merge(events, ., by="event_id") %>%
  group_by(device_id, app_id) %>%
  summarise(count=n()) %>%
  arrange(desc(count)) %>%
  filter(app_id %in% top100_app$app_id)

device_app_wide <- dcast(data=device_app, device_id ~ app_id, value.var = "count", fill=0)


# device_app$app_id <- factor(device_app$ app_id)
# 
# for(app_id in top100_app$app_id) {
#   device_app <- cbind(device_app, x=0)
#   names(device_app)[length(device_app)] <- app_id
# }
# device_app_wide <- device_app[!duplicated(device_app$device_id),]
# for(app_id in unique(device_app$app_id)){
#   device_app[, app_id] <- 
# }

# geo+temporal data
events$timestamp <- strptime(events$timestamp, format="%Y-%m-%d %H:%M:%S")
events$weekday <- weekdays(events$timestamp)
events$hour <- as.numeric(format(events$timestamp, "%H"))
events$date <- strftime(events$timestamp, format="%m%d")



events <- events %>%
  select(-timestamp) %>%
  group_by(device_id) %>%
  summarise(longitude=median(longitude), latitude=median(latitude),
            day_mode=weekday[which.max(table(weekday))], hour_mode=hour[which.max(table(hour))],
            event_count=n(), date_count=length(unique(date)),
            event_per_day = event_count/date_count,
            day_count=length(unique(weekday)), hour_count=length(unique(hour)))
events[events$longitude == 0, "longitude"] <- NA
events[events$latitude == 0, "latitude"] <- NA

# Part 2: phone characteristics
phone_brand_device_model <- phone_brand_device_model[!duplicated(phone_brand_device_model$device_id),]
cn_en <- read.csv("processed_data/cn_en.csv", stringsAsFactors = FALSE)
cn_en$chinese <- enc2utf8(cn_en$chinese)
phone_brand_device_model$phone_brand <- enc2utf8(as.character(phone_brand_device_model$phone_brand))
phone_brand_device_model$phone_brand_en <- sapply(phone_brand_device_model$phone_brand, function(x){ifelse(any(grepl(x, cn_en$chinese, fixed=TRUE)),toupper(cn_en[grep(x, cn_en$chinese, fixed = TRUE), "english"]), "Others")})
phone_brand_device_model$phone_brand_en <- gsub(" ", "", phone_brand_device_model$phone_brand_en)


###################################
# joining data

# device characteristics
data.train <- merge(gender_age_train, phone_brand_device_model, by="device_id", all.x=TRUE)
data.test <- merge(gender_age_test, phone_brand_device_model, by="device_id", all.x=TRUE)

# app usage
data.train <- merge(data.train, app_count, by="device_id", all.x=TRUE)
data.test <- merge(data.test, app_count, by="device_id", all.x=TRUE)

# geo+temporal data
data.train <- merge(data.train, events, by="device_id", all.x=TRUE)
data.test <- merge(data.test, events, by="device_id", all.x=TRUE)

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
   gender_age_train,
   gender_age_test)


