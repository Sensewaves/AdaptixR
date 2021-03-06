% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{AdaptixPredictionPointsToDataframe}
\alias{AdaptixPredictionPointsToDataframe}
\title{Convert a scenario list returned by AdaptixPredict followed by an AdaptixGetPredictionScenarios call to a data frame}
\usage{
AdaptixPredictionPointsToDataframe(prediction.list)
}
\arguments{
\item{prediction.list}{a transformed list of scenarios as provided by AdaptixGetPredictionScenarios on a prediction response}
}
\value{
a dataframe object of predicted scenarios
}
\description{
Convert a scenario list returned by AdaptixPredict followed by an AdaptixGetPredictionScenarios call to a data frame
}
\examples{
AdaptixPredictionPointsToDataframe(AdaptixGetPredictionScenarios(AdaptixPredict(conn = conn, stream = "123456abcdef", from = "2017-01-01", to = "2017-01-02", seasonality = "1w", rate = "1h", verbose = F)))
}
