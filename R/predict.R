#' Perform prediction
#'
#' @param conn a valid Adaptix connection object.
#' @param stream the ID of the stream on which to perform the prediction.
#' @param from if not NULL, the starting date of the prediction. If NULL, defaults to the last point of the stream.
#' @param to if not NULL, the closing date of the prediction. required if 'at' is NULL.
#' @param at if not NULL, the date of the prediction. required if 'from' and 'to' are NULL.
#' @param rate desired frame rate of the prediction scneario(s).
#' @param seasonality expected seasonality of the predicted data.
#' @param filter filter the data used in prediction to provided labels. Should be provided as a list.
#' @param verbose display HTTP operation details.
#' @return a data.frame with the requested collection of points.
#' @examples
#' AdaptixPredictions(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", seasonality = "1w", rate = "1h", verbose = F)
AdaptixPredict <- function(conn, stream, from = NULL, to = NULL, at = NULL, rate,
                           seasonality, filter = NULL, rebuild = FALSE, verbose = FALSE)
{
  #Check minimal parameters
  if(is.null(from) || is.null(to) && is.null(at))
    stop("missing parameters [from/to or at]")

  url <- paste0(conn@streams.apiURL, stream, "/", "predict?")
  url <- paste0(url, "seasonality=", seasonality)
  url <- paste0(url, "&rate=", rate)

  if(!is.null(from) && from != "")
    url <- paste0(url, "&from=", ConvertDateToISO8601(from))

  if(!is.null(to) && to != "")
    url <- paste0(url, "&to=", ConvertDateToISO8601(to))

  if(!is.null(at))
    url <- paste0(url, "&at=", ConvertDateToISO8601(at))

  if(!is.null(filter))
    url <- paste0(url, "&filter=[", paste(filter, collapse = ","), "]")

  if(!is.null(rebuild))
    url <- paste0(url, "&rebuild=", rebuild)

  #send request
  r <- AdaptixGetHTTPRequest(conn = conn,
                              url = url,
                              verbose = verbose)
  AdaptixCheckRequest(request = r, c("200", "201"))
  return(httr::content(r))
}


#' Convert a scenario list returned by AdaptixPredict followed by an AdaptixGetPredictionScenarios call to a data frame
#'
#' @param prediction.list a transformed list of scenarios as provided by AdaptixGetPredictionScenarios on a prediction response
#' @return a dataframe object of predicted scenarios
#' @examples
#' AdaptixPredictionPointsToDataframe(AdaptixGetPredictionScenarios(AdaptixPredict(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", seasonality = "1w", rate = "1h", verbose = F)))
AdaptixPredictionPointsToDataframe <- function(prediction.list) {
  return(AdaptixForecastPointsToDataframe(prediction.list))
}


#' Convert a scenario list returned by AdaptixPredict followed by an AdaptixGetPredictionScenarios call to an XTS object
#'
#' @param prediction.list a transformed list of scenarios as provided by AdaptixGetPredictionScenarios on a prediction response
#' @return an XTS object of the predicted scenarios
#' @examples
#' AdaptixPredictionPointsToXts(AdaptixGetPredictionScenarios(AdaptixPredict(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", seasonality = "1w", rate = "1h", verbose = F)))
AdaptixPredictionPointsToXts <- function(prediction.list) {
  return(AdaptixForecastPointsToXts(prediction.list))
}


#' Converts scenarios in a prediction response to a comprehensive list structure of scenarios
#'
#' @param prediction.response A prediction response as returned by an AdaptixPrediction call.
#' @return transformed list of scenarios
AdaptixGetPredictionScenarios <- function(prediction.response) {
  scenarios <- prediction.response$scenarios
  l <- list()
  index <- 1
  if(length(scenarios) == 0)
    return(l)
  for(i in 1:length(scenarios)) {
    #sometimes the API returns empty list of points... so we have to check
    if(length(scenarios[[i]]$points) > 0) {
      df <- data.frame(matrix(unlist(scenarios[[i]]$points), nrow = length(scenarios[[i]]$points), byrow = T), stringsAsFactors = F)
      colnames(df) <- c("value" , "at")
      df$at <- as.POSIXct(df$at, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")
      df$value <- as.numeric(df$value)

      occ <- data.frame()
      sublist <- list(points = df,
                      probability = scenarios[[i]]$probability)

      l[[index]] <- sublist
      index <- index + 1
    }
  }
  return(l)
}
