#' From a meta-analysis, analyse for publication bias
#'
#' Calculates observed and expected number of positive studies and P for difference
#'
#' Implements test for chasing of significance.
#' @param vec_r_events_control an ordered vector of number of events in the untreated group of constituent studies from a meta-analysis
#' @param vec_r_events_treated an ordered vector of number of events in the treated group of constituent studies from a meta-analysis
#' @param vec_n_sample_size_control an ordered vector of the number of participants in the untreated group
#' @param vec_n_sample_size_treated an ordered vector of the number of participants in the treated group
#' @param n Number of iterations used to generate constituent study power; suggest use 10,000
#' @param low.alpha Lower limit of type-1 error rate used to judge whether constituent studies are positive; suggest 0.001
#' @param high.alpha Upper limit of type-1 error rate used to judge whether constituent studies are positive; suggest 0.3
#' @param by.alpha Interval of type-2 error rate at which observed and expected values and P for difference evaluated
#' @return a dataframe with columns which include alpha level, observed number of positive studies, expected number, and P for difference, OR_hat (summary measure of effect for meta-analysis) with varying levels of significance for constituent studies
#' @export
#' @examples
#'   data("BMort") ## Meta-analysis of statin use; Brugts 2009, BMJ
#'  par(mar = c(5, 6, 4, 5) + 0.1)
#'  Btmort<-with(BMort,
#'    plot_chase_observed_expected(r_events_control,
#'     r_events_treated,
#'     n_sample_size_control,
#'     n_sample_size_treated,
#'     n = 10, # n set low for speed.
#'     low.alpha = 0.001,
#'     high.alpha = 0.3,
#'     by.alpha = 0.01))
#' plot(Btmort$alpha,
#'  Btmort$observed,
#'  type="l",
#'  las= 1,
#'  lwd= 2,
#'  xlim = c(0.0001, 0.3),
#'    xlab = c("Significance level"),  #### Brugts study mortality outcome;
#'    ylab = c("Number of significant studies;
#'    expected or observed"),
#'    main = "Brugts; all-cause mortality")
#' lines(Btmort$alpha, Btmort$observed)
#' lines(Btmort$alpha, Btmort$expected, lty = 3)
#' abline(v=0.05, lty=2)
#' par(new = TRUE)
#' plot(Btmort$alpha, Btmort$p.value,
#' type="l", xlab="", lty=4, lwd=2,
#' col="grey", axes=FALSE, ylab="")
#' abline(h = 0.1, lty=2)
#' axis(4,las = 1)
#' mtext(side = 4,
#'   line = 2.5,
#'   expression(paste(italic("P"),
#'   " for difference")))
#'
plot_chase_observed_expected <-function(vec_r_events_control,
                                        vec_r_events_treated,
                                        vec_n_sample_size_control,
                                        vec_n_sample_size_treated,
                                        n,
                                        low.alpha,
                                        high.alpha,
                                        by.alpha)  {
  metaOR <- NULL
  OR_hat <- NULL
  metaOR <- summary(meta.MH(vec_n_sample_size_treated,vec_n_sample_size_control,vec_r_events_treated,vec_r_events_control))
  OR_hat <- metaOR$MHci[[2]]
  if(OR_hat >= 1) {stop("The pooled odds ratio should be less than one to use this method,
                       since it is assumed that a protective effect of the intervention is claimed.")
  }
  alpha_list <- seq(low.alpha,high.alpha, by=by.alpha)
  b<- as.list(1:length(alpha_list))
  pb <- txtProgressBar(min = 0, max = length(b), style=3)
  for (i in 1:length(alpha_list)){

    b[[i]] <- test.n.treated(vec_r_events_control, vec_r_events_treated,
                           vec_n_sample_size_control, vec_n_sample_size_treated, alpha=alpha_list[i])
  }

  a<-as.list(1:length(alpha_list))
  e<-as.vector(1:length(alpha_list))

  for (i in 1:length(a)) {
    a[[i]] <- ChisqTest_expect(vec_r_events_control,
                               vec_n_sample_size_control,
                             vec_n_sample_size_treated,
                             OR_hat,
                             n,
                             alpha=alpha_list[i],
                             vec_pos=as.vector(b[[i]]))

    setTxtProgressBar(pb, i)
  }
  close(pb)
  #browser()
  a <- do.call(rbind,a)
  b <- do.call(rbind,b)
  f <- as.vector(1:nrow(a))
  for (i in 1:nrow(a)) {
    f[i]<-sum(b[i,])
  }
  o<-rep(OR_hat,length(a))
  d<-data.frame(cbind(alpha_list,a,f,o))
  names(d)<-c("alpha","p.value","expected","observed","OR_hat")
  return(d)
}
NULL

#' Brugts meta-analysis study of effect of statins on overall mortality (BMJ 2009).
#' @docType data
#' @name BMort
#' @format Brugts meta-analysis study of effect of statins on overall mortality (BMJ 2009).
#' @source Brugts JJ, Yetgin T, Hoeks SE, Gotto AM, Shepherd J, Westendorp RGJ, et al. The benefits of statins in people without established cardiovascular disease but with cardiovascular risk factors: meta-analysis of randomised controlled trials. BMJ 2009;338
#' usage data(BMort)
NULL
