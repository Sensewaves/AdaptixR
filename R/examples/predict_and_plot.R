library(AdaptixR)
library(xts)

# Create connection
conn <- AdaptixConnect(url = "https://alpha.adaptix.io/api", api.key = "my_adaptix_key")

# set the stream id
stream.id <- "my_stream_id"
# perform the prediction on a given stream
prediction <- AdaptixPredict(conn = conn, stream = stream.id, from = "2018-09-01 00:00:00", to = "2018-09-02 00:00:00",
                             seasonality="1w", rate="1h", verbose=T)


# parse the results from the response using the library's dedicated parsing methods, and convert them to an XTS object
prediction.result.xts <- AdaptixPredictionPointsToXts(AdaptixGetPredictionScenarios(prediction.response = prediction))
colnames(prediction.result.xts) <- c("prediction")

# plot using xts's plot function
plot.xts(prediction.result.xts)

# plot using dygraphs
library(dygraphs)
dygraph(prediction.result.xts) %>% dyRangeSelector()

# plot with some context

# Retrieve the last 1000 points of the stream
points <- AdaptixGetLastPoints(conn = conn, stream = stream.id, n = 1000, verbose = T)
points.xts <- xts(x = points$value, order.by = points$at)
colnames(points.xts) <- c(stream.id)

# bind the last points with the xts object containing prediction results and plot
dygraph(cbind(points.xts, prediction.result.xts)) %>% dyRangeSelector()

