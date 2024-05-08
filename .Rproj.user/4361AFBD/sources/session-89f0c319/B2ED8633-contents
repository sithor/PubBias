#' For one constituent study and one simulation of its outcome, test if p<type-1 error rate (not for end user)
#'
#' @description
#' For one constituent study and one simulation of its outcome, test if p<type-1 error rate (not for end user)
#'
#' @details
#' Not for end user.
#'
#'
#' @param r_events_control Number of events in untreated
#' @param n_sample_size_control Sample size in untreated group
#' @param n_sample_size_treated Sample size in treated group
#' @param alpha Type-1 error rate.
#' @param OR_hat Summary odds ratio from meta-analysis
#' @return Number, 1 if positive, 0 if negative.
#' @export
test_one <- function(r_events_control, n_sample_size_control, n_sample_size_treated, OR_hat, alpha) {
  RR_hat<-OR_hat/(1-((sum(r_events_control))/(sum(n_sample_size_control)))+OR_hat*((sum(r_events_control))/(sum(n_sample_size_control))))
  pi_control <- r_events_control/n_sample_size_control
  pi_exposed <-1/ (1+((n_sample_size_control-r_events_control)/(RR_hat*r_events_control)))
  sim_control_event <- rbinom(1,n_sample_size_control,pi_control)
  sim_exposed_event <-rbinom(1, n_sample_size_treated, pi_exposed)
  sim_control_no_events <-n_sample_size_control-sim_control_event
  sim_exposed_no_events <- n_sample_size_treated-sim_exposed_event
  matrix1 <-matrix(c(sim_exposed_event,sim_exposed_no_events,sim_control_event, sim_control_no_events), nrow=2,ncol=2)
  test<-fisher.test(matrix1)
  positive<-ifelse(test$p.value<alpha & test$estimate<1,1,0)
  return(as.numeric(positive))
}
NULL
