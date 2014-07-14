#' Tests a particular strategy.
#'
#' @export
TestStrategy <- function(actionFn, returnFn, tsData, portfolio, snapshotPortfolio=FALSE) {
  times <- index(tsData)
  
  origPortfolio <- portfolio
  
  # The first tick is special: there is no previous time, so there is no return to compute
  portfolio <- actionFn(times[1], tsData, portfolio)
  if (snapshotPortfolio) {
    portfolioSnapshots <- as.list(rep(NA, times=length(index(tsData))))
    names(portfolioSnapshots) <- as.character(index(tsData))
    portfolioSnapshots[[as.character(times[1])]] <- portfolio
  }
  
  returns <- rollapplyr(zoo(times, order.by=times), width=2, FUN=function(timePair) {
    returnValue <- returnFn(timePair[1], timePair[2], tsData, portfolio)
    portfolio <<- actionFn(timePair[2], tsData, portfolio)
    if (snapshotPortfolio) portfolioSnapshots[[as.character(timePair[2])]] <<- portfolio
    returnValue
  })
  
  testResult <- list(ReturnSeries=returns, TimeSeries=tsData, ActionFunction=actionFn, 
                     ReturnFunction=returnFn, OriginalPortfolio=origPortfolio)
  if (snapshotPortfolio) testResult[["PortfolioSnapshots"]] <- portfolioSnapshots
  
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
  printf("Timeseries length: %.0f\n", length(index(TimeSeries(testResult))))
  printf("Timeseries start:  %s\n", index(TimeSeries(testResult))[1])
  printf("Timeseries end:    %s\n", index(TimeSeries(testResult))[nrow(TimeSeries(testResult))])
}

#' @export
ReturnSeries <- function(testResult) {
  testResult[["ReturnSeries"]]
}

#' @export
PortfolioSnapshots <- function(testResult) {
  if (!is.null(testResult[["PortfolioSnapshots"]])) {
    testResult[["PortfolioSnapshots"]]
  }
  else {
    actionFn <- ActionFunction(testResult)
    tsData <- TimeSeries(testResult)
    portfolio <- OriginalPortfolio(testResult)
    
    Map(index(tsData), f=function(time) {
      portfolio <<- actionFn(time, tsData, portfolio)
      portfolio
    })
  }
}

#' @export
OriginalPortfolio <- function(testResult) {
  testResult[["OriginalPortfolio"]]
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