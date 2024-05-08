#' Observed number of positive by testing for significance from observed findings at given alpha level (not intended for end user).
#' @description
#' by testing for significance from observed findings at given alpha level (not intended for end user).
#' @details
#' Not for end user.
#'
#' @param vec_r_events_control an ordered vector of number of events in the untreated group of constituent studies from a meta-analysis.
#' @param vec_r_events_treated an ordered vector of number of events in the treated group of constituent studies from a meta-analysis.
#' @param vec_n_sample_size_control an ordered vector of the number of participants in the untreated group.
#' @param vec_n_sample_size_treated an ordered vector of the number of participants in the treated group.
#' @param alpha Type-1 error rate.
#' @return Vector of results, 1 if positive, 0 if negative.
#' @export
test.n.treated <- function(vec_r_events_control, vec_r_events_treated,
                           vec_n_sample_size_control, vec_n_sample_size_treated, alpha) {
  xx <- as.vector(1:length(vec_r_events_control))
  for (i in 1:length(xx)) xx[i] <- test.one.treated(vec_r_events_control[i], vec_r_events_treated[i],
                                                    vec_n_sample_size_control[i], vec_n_sample_size_treated[i], alpha)
  return(xx)
}
NULL
