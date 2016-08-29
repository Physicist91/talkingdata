library(ggplot2)

## Use geolocation information

events <- fread("raw_data/events.csv",
                colClasses=c("character","character","character",
                             "numeric","numeric"))
setkeyv(events, c("device_id", "event_id"))

# events has 60k devices, train 23k, test 35k.
sum(train_data@Dimnames[[1]] %in% unique(events$device_id))
sum(test_data@Dimnames[[1]] %in% unique(events$device_id))

# Attempt to do hierarchical clustering
# ll <- events[abs(events$longitude) > 0.01 | abs(events$latitude) > 0.01,]
# ll <- ll[, list(longitude=mean(longitude), latitude=mean(latitude)), by="device_id"]
# distances <- dist(ll[, list(longitude, latitude)])
# clusters <- hclust(distances)
# plot(clusters)
# clustercut <- cutree(clusters, 70)
# plot(ll$longitude, ll$latitude, col=clustercut)

# k-means clustering
ll <- events[abs(events$longitude) > 0.01 | abs(events$latitude) > 0.01,]
ll <- ll[, list(longitude=mean(longitude), latitude=mean(latitude)), by="device_id"]

rm(events)

set.seed(20)
clusters <- kmeans(ll[, list(longitude, latitude)], 75, iter.max=20, nstart=20)
colors <- rainbow(length(unique(clusters$cluster)))
names(colors) <- unique(clusters$cluster)
plot(ll$longitude, ll$latitude, t='n')
text(ll$longitude, ll$latitude, labels=clusters$cluster, col=colors[clusters$cluster])


# create feature with the clustered geolocations
d4 <- cbind(ll[, list(longitude, latitude)], location=clusters$cluster)
d4 <- d4[, location := paste0("location:",location)]

