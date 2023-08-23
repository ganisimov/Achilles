chunk <- function(x, n) split(x, cut(seq_along(x), n, labels = FALSE))
papply <- function(numThreads, x, fun, ...) {
  if (numThreads == 1 || numThreads >= length(x)) {
    lapply(x, fun, ...)
  } else {
    chunkedX <- chunk(
      x = x,
      n = numThreads
    )

    cluster <- ParallelLogger::makeCluster(
      numberOfThreads = numThreads,
      singleThreadToMain = TRUE
    )

    ## preserve options
    optionsCopy <- options()
    result <- ParallelLogger::clusterApply(
      cluster = cluster,
      x = chunkedX,
      fun = function(x, fun, optionsCopy, ...) {
        options(optionsCopy)
        lapply(x, fun, ...)
      },
      fun,
      optionsCopy,
      ...
    )
    ParallelLogger::stopCluster(cluster = cluster)
    return(as.list(unlist(result)))
  }
}
