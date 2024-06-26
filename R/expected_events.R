#' calculate the number of expected events
#'
#' @description
#' from a study included in a meta-analysis assuming summary effect estimate (OR_hat) is true (not for end user).
#'
#' @details
#' Not for end user
#'
#'
#' @param vec_r_events_control an ordered vector of number of events in the untreated group of constituent studies from a meta-analysis.
#' @param vec_n_sample_size_control an ordered vector of the number of participants in the untreated group.
#' @param vec_n_sample_size_treated an ordered vector of the number of participants in the treated group.
#' @param alpha Type-1 error rate.
#' @param n Number of iterations used to generate constituent study power; suggest use 10,000.
#' @param OR_hat Summary odds ratio from meta-analysis
#' @return Expected number of events at given alpha level.
#' @export
expected_events <- function(vec_r_events_control, vec_n_sample_size_control, vec_n_sample_size_treated, OR_hat, n, alpha){
  power_ind <- as.vector(1:length(vec_r_events_control))
  for (i in 1:length(power_ind)) {
    power_ind[i] <- test.n(vec_r_events_control[i], vec_n_sample_size_control[i], vec_n_sample_size_treated[i], OR_hat, n, alpha)
  }
  expected <- sum(abs(power_ind))
  return(expected)
}
NULL
