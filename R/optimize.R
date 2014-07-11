#' @export
OptimizeBruteForce <- function(fitnessFn, actionFactoryFn, returnFn, tsData, portfolio, ...) {
  # Create every possible permutation of our parameter ranges
  configGrid <- expand.grid(..., stringsAsFactors=FALSE)
  
  # Test a strategy configured with each parameter permutation
  testResults <- apply(configGrid, MARGIN=1, FUN=function(row) {
    actionFn <- do.call(actionFactoryFn, row)
    TestStrategy(actionFn, returnFn, tsData, portfolio)
  })
  
  # Calculate the fitness scores
  fitnessScores <- unlist(lapply(testResults, FUN=fitnessFn))
  
  optimResult <- list(Configurations=configGrid[order(fitnessScores),], TestResults=testResults[order(fitnessScores)], 
                      FitnessScores=fitnessScores)
  
  class(optimResult) <- c("OptimResult", "list")
  optimResult
}

#' @export
OptimizeGenetic <- function(fitnessFn, actionFactoryFn, returnFn, tsData, portfolio, ...) {
  
}

#' @export
as.list.OptimResult <- function(optimResult) {
  class(optimResult) <- "list"  # Class "assignment" actually does a deep copy
  optimResult
}

#' @export
print.OptimResult <- function(optimResult) {
  printf("Best Configuration:\n")
  print(BestConfiguration(optimResult))
  printf("Best Test Results:\n")
  print(BestTestResult(optimResult))
}

#' @export
TestResults <- function(optimResult, bestToWorse=TRUE) {
  
}

#' @export
Configurations <- function(optimResult, bestToWorse=TRUE) {
  
}

#' @export
BestTestResult <- function(optimResult) {
  
}

#' @export
BestConfiguration <- function(optimResult) {
  
}

