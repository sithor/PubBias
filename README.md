# `PubBias`, an `R` package to look for an excess of significant study results in a meta-analysis.

I adapted a method designed by Ioannidis and Trikalinos, which
compares the observed number of positive studies in a meta-analysis with
the expected number, if the summary measure of effect, averaged over the
individual studies, were assumed true. 

Excess in the observed number of positive studies, compared to the expected, 
is taken as evidence of publication bias. The **observed** number of positive studies, at a given level
for statistical significance, is calculated by applying Fisher's exact test
to the reported 2x2 table data of each constituent study, doubling the
Fisher one-sided *P*-value to make a two-sided test. The corresponding
**expected** number of positive studies is obtained by summing the statistical
powers of each study. The statistical power depends on a given measure of
effect which, here, it is the pooled odds ratio of the meta-analysis estimated by fixed effect methods.
By simulating each constituent study, with the given odds ratio, and
the same number of treated and non-treated as in the real study, the power
of the study is estimated as the proportion of simulated studies that are
positive, again by a Fisher's exact test. 

The simulated number of events in the treated and untreated groups is carried out with binomial sampling. In the
untreated group, the binomial proportion is the percentage of actual
events reported in the study and, in the treated group, the binomial
sampling proportion is the untreated percentage multiplied by the risk
ratio derived from the assumed common pooled odds ratio. 

The statistical significance for judging a positive study may be varied and large
differences between **expected** and **observed** number of positive studies around
the level of 0.05 significance constitutes evidence of publication bias. 
The difference between the **observed** and **expected** is tested by chi-square. A
chi-square *P*-value for the difference below 0.05 is suggestive of
publication bias, however, a less stringent level of 0.1 is often used in
studies of publication bias, as the number of published studies is usually
small. 

It is assumed that pooled odds ratios will be less than one, 
indicating a protective effect of the intervention compared to the control.

Future work will adapt the method for continuous outcome meta-analysis.

## Installing the package
`install.packages("devtools")`\
`devtools::install_github("sithor/PubBias")`\
`library(PubBias)`

## Reference
Clin Trials 2007;4(3):245-53.
