#' Is one constituent study observed significant, in favour of treatment, at a given alpha level? (Not intended for end user)
#' @param r_events_control Number of events in untreated
#' @param r_events_treated Number of events in treated group
#' @param n_sample_size_control Sample size in untreated group
#' @param n_sample_size_treated Sample size in treated group
#' @param alpha Type-1 error rate
#' @return Number, 1 if positive, 0 if negative
#' @export
test.one.treated <- function(r_events_control,r_events_treated, n_sample_size_control,n_sample_size_treated, alpha=0.05){
  treated_nonevents<-n_sample_size_treated-r_events_treated
  control_nonevents<-n_sample_size_control-r_events_control
  cd<- matrix(c(r_events_treated,treated_nonevents,
                r_events_control, control_nonevents), nrow=c(2),ncol=c(2))
  test1<-fisher.test(cd)
  positive1<-ifelse(test1$p.value<alpha & test1$estimate<1,1,0)
  return(positive1)
}
NULL
