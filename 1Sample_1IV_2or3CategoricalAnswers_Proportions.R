#From Coursera SD "Designing, Running, and Analyzing Experiments
############1 variable, 2 options categories possible
#pearson chi-squared test for a single variable
##works best with LOTS of data
dt=read.csv()
chi=xtabs(~columnofinerest1, data=dt)
chi
chisq.test(dt$columnofinterest1)
##reporting the results
##X^2<subscriptstart>(df, N=number of cases)<subscriptend>
   ###=test statistic (output value), p<.03

#binomial test to give an "exact test" of one variable with 
##only 2 response categories
binom.test(dt)

####################1 variable, more than 2 categorical outcomes possible
prefsABC=read.csv("prefsABC.csv")
#table and chi squared with more than 2 response categories for a single variable
chi2=xtabs(~Preference, data=prefsABC)
chi2
chisq.test(prefsABC$Preference)
#when reporting, looks like above but will have 2 df instead of 1

#exact test with one variable with muiltiple response categories
library(XNomial)
xmulti(prefsABC, c(1/3, 1/3, 1/3), stateName="Prob")
#post hoc binomial tests with correction for multiple comparisons
aa=binom.test(sum(prefsABC$Preference=="A"), nrow(prefsABC), p=1/3)
bb=binom.test(sum(prefsABC$Preference=="B"), nrow(prefsABC), p=1/3)
cc=binom.test(sum(prefsABC$Preference=="C"), nrow(prefsABC), p=1/3)
p.adjust(c(aa$p.value, bb$p.value, cc$p.value), method = "holm")
#the adjustment is similar to a bonferroni correction but called "holm