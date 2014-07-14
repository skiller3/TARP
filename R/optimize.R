#' @export
OptimizeBruteForce <- function(fitnessFn, actionFactoryFn, returnFn, tsData, portfolio, ...) {
  # Create every possible permutation of our parameter ranges
  configGrid <- expand.grid(..., stringsAsFactors=FALSE)
  # browser()
  # Test a strategy configured with each parameter permutation
  testResults <- apply(configGrid, MARGIN=1, FUN=function(row) {
    actionFn <- do.call(actionFactoryFn, as.list(row))
    TestStrategy(actionFn, returnFn, tsData, portfolio)
  })
  
  # Calculate the fitness scores
  fitnessScores <- unlist(lapply(testResults, FUN=fitnessFn))
  
  optimResult <- list(Configurations=configGrid[order(fitnessScores, decreasing=TRUE),],
                      TestResults=testResults[order(fitnessScores, decreasing=TRUE)], 
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
  printf("Best Configuration\n")
  printf("==================\n")
  bestConfig <- BestConfiguration(optimResult)
  Map(names(bestConfig), f=function(name) printf("%s: %s\n", name, as.character(bestConfig[[name]])))
  printf("\nBest Test Results\n")
  printf("=================\n")
  print(BestTestResult(optimResult))
}

#' @export
TestResults <- function(optimResult, bestToWorse=TRUE) {
  if (bestToWorse) optimResult[["TestResults"]] else rev(optimResult[["TestResults"]])
}

#' @export
Configurations <- function(optimResult, bestToWorse=TRUE) {
  configs <- optimResult[["Configurations"]]
  if (bestToWorse || nrow(configs) == 0) configs else configs[nrow(configs):1,]
}

#' @export
BestTestResult <- function(optimResult) {
  optimResult[["TestResults"]][[1]]
}

#' @export
BestConfiguration <- function(optimResult) {
  as.list(optimResult[["Configurations"]][1,])
}

