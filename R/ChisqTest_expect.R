#' Chi-square test to test for significant difference between observed and expected number of positive studies (not for end user).
#' @param vec_r_events_control an ordered vector of number of events in the untreated group of constituent studies from a meta-analysis.
#' @param vec_n_sample_size_control an ordered vector of the number of participants in the untreated group.
#' @param vec_n_sample_size_treated an ordered vector of the number of participants in the treated group.
#' @param alpha Type-1 error rate.
#' @param OR_hat Summary odds ratio from meta-analysis
#' @param n Number of iterations used to generate constituent study power; suggest use 10,000.
#' @param vec_pos Vector of positive results from constituent studies, returned by test.n.treated() function.
#' @return Vector of p-values for difference between observed and expected number of positive studies from meta-analysis, along with vector of expected values.
#' @export
ChisqTest_expect <- function(vec_r_events_control, vec_n_sample_size_control, vec_n_sample_size_treated, OR_hat, n, alpha, vec_pos)  {
  obs<-sum(as.vector(vec_pos))
  expect <- expected_events(vec_r_events_control, vec_n_sample_size_control, vec_n_sample_size_treated, OR_hat, n, alpha)
  teststat<- ((obs-expect)^2)/expect + (obs-expect)^2/(length(vec_pos)-expect)
  a <- 1-pchisq(teststat, df=1)
  d <- data.frame(cbind(a,expect))
  return(d)
}
NULL
