#' Test the performance of a trading strategy.
#' 
#' @description TestStrategy evaluates the performance of a particular trading strategy. While intended
#' as a useful component of this package's public API, it is also internally used by all provided 
#' optimization functions.
#' 
#' @param actionFn A function that generates a portfolio object. This function will be invoked by 
#' TestStrategy with the following parameters:
#'    time - The moment in time.  This will be one of the time objects that is part of index(tsData).
#'    tsData - The zoo object that was passed into TestStrategy.
#'    portfolio - The portfolio object dictated by this strategy just prior to the specified time 
#'    (the point of actionFn is to generate a portfolio object AT the specified time).
#' @param returnFn A function which computes the return between two "adjacent" points in time. This
#' function will be invoked with the following parameters:
#'    time0 - The first moment in time. This will be one of the time objects that is part of index(tsData).
#'    time1 - The second moment in time. This will be the time object that immediately follows time0 in
#'    index(tsData).
#'    tsData - The zoo object that was passed into TestStrategy.
#'    portfolio - The portfolio at time0.
#' @param tsData A zoo object that contains relevent timeseries data for testing the strategy.
#' @param portfolio An object that represents a portfolio. Its type is not limited in any way
#' by this package.
#' @param snapshotPortfolio Should snapshots of the portfolio be kept as returns are computed?  If FALSE, 
#' the PortfolioSnapshots function will need to to recompute all snapshots when it is invoked. Defaults
#' to FALSE to save memory.
#'
#' @return A S3 object which represents the result of this test.
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

#' Convert a TestResult object to a list.
#' 
#' @seealso \link{as.list}
#' 
#' @export
as.list.TestResult <- function(testResult) {
  class(testResult) <- "list"  # Class "assignment" actually does a deep copy
  testResult
}

#' Print a TestResult object.
#'
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

#' Extract the return series from a TestResult object.
#
#' @export
ReturnSeries <- function(testResult) {
  testResult[["ReturnSeries"]]
}

#' Extract portfolio snapshots (over time) from a TestResult object.
#'
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

#' Extract the original portfolio test parameter from a TestResult object.
#'
#' @export
OriginalPortfolio <- function(testResult) {
  testResult[["OriginalPortfolio"]]
}

#' Extract the original timeseries test parameter from a TestResult object.
#'
#' @export
TimeSeries <- function(testResult) {
  testResult[["TimeSeries"]]
}

#' Extract the original action function test parameter from a TestResult object.
#'
#' @export
ActionFunction <- function(testResult) {
  testResult[["ActionFunction"]]  
}

#' Extract the orignal return function test parameter from a TestResult object.
#'
#' @export
ReturnFunction <- function(testResult) {
  testResult[["ReturnFunction"]]
}

#' Extract the computed strategy total return from a TestResult object.
#'
#' @export
TotalReturn <- function(testResult) {
  prod(1 + as.vector(ReturnSeries(testResult))) - 1
}

#' Extract the computed strategy volatility from a TestResult object.
#'
#' @export
Volatility <- function(testResult) {
  sd(as.vector(ReturnSeries(testResult)))
}

#' Extract the computed Sharpe Ratio from a TestResult object.
#'
#' @export
Sharpe <- function(testResult, ...) {
  SharpeRatio(R=ReturnSeries(testResult), ...)
}

#' Extract the computed net asset value series from a TestResult object.
#'
#' @export
NAVSeries <- function(testResult) {
  cumprod(1 + ReturnSeries(testResult))
}

#' Extract the computed max drawdown from a TestResult object.
#'
#' @export
MaxDrawdown <- function(testResult, ...) {
  maxDrawdown(R=as.xts(ReturnSeries(testResult)), ...)
}