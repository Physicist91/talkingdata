# Use app category now

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
rm(events,event_app,event_apps, id_labels);gc()

f_split_paste <- function(z){paste(unique(unlist(strsplit(z,","))),collapse=",")}
device_app_labels <- device_event_apps[,list(apps=f_split_paste(apps)),by="device_id"]
rm(device_event_apps,f_split_paste);gc()

tmp <- strsplit(device_apps$apps,",")
device_app_labels <- data.table(device_id=rep(device_apps$device_id,
                                        times=sapply(tmp,length)),
                          app_label=unlist(tmp))
rm(tmp)
