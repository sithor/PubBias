#' This runs test_one
#'
#' function for many replicates, usually 10,000.
#'
#' for example, to generate the type-2 error rate for one constituent study (not for end user).
#'
#' @param r_events_control Number of events in untreated
#' @param n_sample_size_control Sample size in untreated group
#' @param n_sample_size_treated Sample size in treated group
#' @param alpha Type-1 error rate.
#' @param n Number of iterations used to generate constituent study power; suggest use 10,000.
#' @param OR_hat Summary odds ratio from meta-analysis
#' @return Type-2 error for one constituent study
test.n <- function(r_events_control, n_sample_size_control, n_sample_size_treated, OR_hat, n, alpha) {
  x <- as.vector(1:n)
  for (i in 1:n) x[i] <- test_one(r_events_control, n_sample_size_control, n_sample_size_treated, OR_hat,alpha)
  y<- sum(x)/length(x)
  return(y)
}
NULL
