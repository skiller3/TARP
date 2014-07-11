#' @export
TestStrategy <- function(actionFn, returnFn, tsData, portfolio) {
  times <- index(tsData)
  
  # The first tick is special: there is no previous time, so there is no return to compute
  portfolioSnapshots <- list()
  portfolioSnapshots[[as.character(times[1])]] <- actionFn(times[1], tsData, portfolio)
  
  returns <- rollapplyr(zoo(times, order.by=times), width=2, FUN=function(timePair) {
    portfolioSnapshots[[as.character(timePair[2])]] <<- actionFn(timePair[2], tsData, portfolioSnapshots[[as.character(timePair[1])]])
    returnFn(timePair[1], timePair[2], tsData, portfolioSnapshots[[as.character(timePair[1])]])
  })
  
  testResult <- list(PortfolioSnapshots=portfolioSnapshots, ReturnSeries=returns, TimeSeries=tsData,
                     ActionFunction=actionFn, ReturnFunction=returnFn)
  class(testResult) <- c("TestResult", "list")
  testResult
}

#' @export
as.list.TestResult <- function(testResult) {
  class(testResult) <- "list"  # Class "assignment" actually does a deep copy
  testResult
}

#' @export
print.TestResult <- function(testResult) {
  printf("Total return:      %.5f\n", TotalReturn(testResult))
  printf("Volatility:        %.5f\n", Volatility(testResult))
  printf("Sharpe (0-RFR):    %.5f\n", Sharpe(testResult, FUN="StdDev"))
  printf("Max Drawdown:      %.5f\n", MaxDrawdown(testResult))
  printf("Timeseries length: %.0f\n", length(TimeSeries(testResult)))
  printf("Timeseries start:  %s\n", index(TimeSeries(testResult))[1])
  printf("Timeseries end:    %s\n", index(TimeSeries(testResult))[nrow(TimeSeries(testResult))])
}

#' @export
ReturnSeries <- function(testResult) {
  testResult[["ReturnSeries"]]
}

#' @export
PortfolioSnapshots <- function(testResult) {
  testResult[["PortfolioSnapshots"]]
}

#' @export
PortfolioSnapshot <- function(testResult, time) {
  PortfolioSnapshots(testResult)[[as.character(time)]]
}

#' @export
TimeSeries <- function(testResult) {
  testResult[["TimeSeries"]]
}

#' @export
ActionFunction <- function(testResult) {
  testResult[["ActionFunction"]]  
}

#' @export
ReturnFunction <- function(testResult) {
  testResult[["ReturnFunction"]]
}

#' @export
TotalReturn <- function(testResult) {
  prod(1 + as.vector(ReturnSeries(testResult))) - 1
}

#' @export
Volatility <- function(testResult) {
  sd(as.vector(ReturnSeries(testResult)))
}

#' @export
Sharpe <- function(testResult, ...) {
  SharpeRatio(R=ReturnSeries(testResult), ...)
}

#' @export
NAVSeries <- function(testResult) {
  cumprod(1 + ReturnSeries(testResult))
}

#' @export
MaxDrawdown <- function(testResult, ...) {
  maxDrawdown(R=as.xts(ReturnSeries(testResult)), ...)
}