#' @export
TestStrategy <- function(actionFn, returnFn, tsData, portfolio) {
  times <- index(tsData)
  
  # The first tick is special: there is no previous time, so there is no return to compute
  portfolioSnapshots <- list()
  portfolioSnapshots[[as.character(times[1])]] <- actionFn(times[1], tsData, portfolio)
  
  returns <- rollapplyr(zoo(times, order.by=times), width=2, FUN=function(timePair) {
    portfolioSnapshots[[as.character(timePair[2])]] <<- actionFn(timePair[2], tsData, portfolioSnapshots[[as.character(timePair[1])]])
    returnFn(timePair[2], timePair[1], tsData, portfolioSnapshots[[as.character(timePair[1])]])
  })
  
  testResult <- list(PortfolioSnapshots=portfolioSnapshots, ReturnSeries=returns, TimeSeries=tsData,
                     ActionFunction=actionFn, ReturnFunction=returnFn)
  class(testResult) <- c("TestResult", "list")
  testResult
}

#' @export
as.list.TestResult <- function(result) {
  class(result) <- "list"  # Class "assignment" actually does a deep copy
  result
}

#' @export
print.TestResult <- function(result) {
  printf("Total return:      %.5f\n", TotalReturn(result))
  printf("Volatility:        %.5f\n", Volatility(result))
  printf("Sharpe (0-RFR):    %.5f\n", Sharpe(result, FUN="StdDev"))
  printf("Max Drawdown:      %.5f\n", MaxDrawdown(result))
  printf("Timeseries length: %.0f\n", length(TimeSeries(result)))
  printf("Timeseries start:  %s\n", index(TimeSeries(result))[1])
  printf("Timeseries end:    %s\n", index(TimeSeries(result))[nrow(TimeSeries(result))])
}

#' @export
ReturnSeries <- function(result) {
  result[["ReturnSeries"]]
}

#' @export
PortfolioSnapshots <- function(result) {
  result[["PortfolioSnapshots"]]
}

#' @export
PortfolioSnapshot <- function(result, time) {
  PortfolioSnapshots(result)[[as.character(time)]]
}

#' @export
TimeSeries <- function(result) {
  result[["TimeSeries"]]
}

#' @export
ActionFunction <- function(result) {
  result[["ActionFunction"]]  
}

#' @export
ReturnFunction <- function(result) {
  result[["ReturnFunction"]]
}

#' @export
TotalReturn <- function(result) {
  prod(1 + as.vector(ReturnSeries(result))) - 1
}

#' @export
Volatility <- function(result) {
  sd(as.vector(ReturnSeries(result)))
}

#' @export
Sharpe <- function(result, ...) {
  SharpeRatio(R=ReturnSeries(result), ...)
}

#' @export
NAVSeries <- function(result) {
  cumprod(1 + ReturnSeries(result))
}

#' @export
MaxDrawdown <- function(result, ...) {
  maxDrawdown(R=as.xts(ReturnSeries(result)), ...)
}