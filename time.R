
events <- read.csv("raw_data/events.csv", colClasses = c("integer", "factor", "factor", "numeric", "numeric"))

events$timestamp <- strptime(events$timestamp, format="%Y-%m-%d %H:%M:%S")
events$weekday <- weekdays(events$timestamp)
events$hour <- as.numeric(format(events$timestamp, "%H"))

library(data.table)
device_hour <- data.table(device_id=events$device_id, hour=events$hour)
device_day <- data.table(device_id=events$device_id, weekday=events$weekday)
rm(events)
