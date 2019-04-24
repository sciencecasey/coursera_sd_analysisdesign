#add voice conditition for all subjects from TTest example
#One way repeated measures Anova
voicescrollsearch=read.csv("searchscrollvoice.csv")
voicescrollsearch$Subject=factor(voicescrollsearch$Subject)
voicescrollsearch$Order=factor(voicescrollsearch$Order)
str(voicescrollsearch)

#view desc. stats by Technique
library(plyr)
ddply(voicescrollsearch, ~Technique, function(data) summary(data$Time))
ddply(voicescrollsearch, ~Technique, summarise, mean.Time=mean(Time), sd.Time=sd(Time))
#graph to explore
hist(voicescrollsearch[voicescrollsearch$Technique=="Search"]$Time)
hist(voicescrollsearch[voicescrollsearch$Technique=="Scroll"]$Time)
hist(voicescrollsearch[voicescrollsearch$Technique=="Voice"]$Time)
plot(Time~Technique, data=voicescrollsearch)

#repeated measures Anova
library(ez)
#allows us to specify the within subjects ID, the repeated measures
#IV technique, and the DV Time
m=ezANOVA(dv=Time, within=Technique, wid=Subject, data=voicescrollsearch)
#for 3 or more levels need to test spherecity of variances
m$Mauchly #p<.05 indicates a violation as doesn't fit model
#examine uncorrected ANOVA in m if no violation
#if violation, look at m$Spericity and us the Greenhouse-Geisser correction, GGE
m$ANOVA
#can see the GES as generalized eta squared (effect size)
#include correced DFs for each corrected effect
pos=match(m$"Sphericity Corrections"$Effect, m$ANOVA$Effect) #positions of within subjects effecs in m$ANOVA
m$Spericity$GGe.DFn=m$Sphericity$GGe * m$ANOVA$DFn[pos] #numerator DF Greenhouse-Geisser
m$Spericity$GGe.DFd=m$Sphericity$GGe * m$ANOVA$DFd[pos] #denominator DF
m$Spericity$GGe.DFn=m$Sphericity$GGe * m$ANOVA$DFn[pos] #numerator DF Huynh-Fedlt
m$Spericity$GGe.DFd=m$Sphericity$GGe * m$ANOVA$DFd[pos] #demoniator DF
#p value must be less than .05 to have sig result based on sphericity

#same uncorrected results are mound with:
m=aov(Time~Technique+Error(Subject/Technique), data=voicescrollsearch)
summary(m)
#doesn't give the sphericity test so not best practice

#manual pos-hoc with wide format table t tests
library(reshape2)
voicescrollsearch_widetech=dcast(voicescrollsearch, Subject~Technique, value.var = "Time")
view(voicescrollsearch_widetech)
se.sc=t.test(voicescrollsearch_widetech$Search, voicescrollsearch_widetech$Scroll, paired=TRUE)
se.vc=t.test(voicescrollsearch_widetech$Search, voicescrollsearch_widetech$Voice, paired=TRUE)
vc.sc=t.test(voicescrollsearch_widetech$Voice, voicescrollsearch_widetech$Scroll, paired=TRUE)
p.adjust(c(se.sc$p.value, se.vc$p.value, vc.sc$p.value), method = "holm")


#non-parametric equivalent
#examine Errors across 3 techniques
library(plyr)
ddply(voicescrollsearch, ~Technique, function(data) summary(data$Errors))
ddply(voicescrollsearch, ~Technique, summarise, Errors.mean=mean(Errors), Errors.sd=sd(Errors))
hist(voicescrollsearch[voicescrollsearch$Technique=="Voice",]$Errors)
hist(voicescrollsearch[voicescrollsearch$Technique=="Scroll",]$Errors)
hist(voicescrollsearch[voicescrollsearch$Technique=="Search",]$Errors)
plot(Errors~Technique, data=voicescrollsearch)

#check if errors are POisson distributed
library(fitdistrplus)
fit=fitdist(voicescrollsearch[voicescrollsearch$Technique=="Voice",]$Errors, "pois", discrete=TRUE)
gofstat(fit)
fit=fitdist(voicescrollsearch[voicescrollsearch$Technique=="Scroll",]$Errors, "pois", discrete=TRUE)
gofstat(fit)
fit=fitdist(voicescrollsearch[voicescrollsearch$Technique=="Search",]$Errors, "pois", discrete=TRUE)
gofstat(fit)
#no sig departure from Poisson as not statistically sig (yes, a poisson dist.)

#Friedman test on Errors
library(coin)
friedman_test(Errors~Technique|Subject, data=voicescrollsearch, distribution="asymptotic")
#p value less than .05 means significant difference in errors by Technique

#post-hoc Wilcoxon signed-rank test with mult. comparisons corrections
se.sc=wilcox.test(voicescrollsearch[voicescrollsearch$Technique=="Search",]$Errors, 
                  voicescrollsearch[voicescrollsearch$Technique=="Scroll"]$Errors, 
                  paired=TRUE, exact=FALSE)
se.vc=wilcox.test(voicescrollsearch[voicescrollsearch$Technique=="Search",]$Errors, 
                  voicescrollsearch[voicescrollsearch$Technique=="Voice"]$Errors, 
                  paired=TRUE, exact=FALSE)
vc.sc=wilcox.test(voicescrollsearch[voicescrollsearch$Technique=="Voice",]$Errors, 
                  voicescrollsearch[voicescrollsearch$Technique=="Scroll"]$Errors, 
                  paired=TRUE, exact=FALSE)
p.adjust(c(se.sc$p.value, se.vc$p.value, vc.sc$p.value), method="holm")

#alternative is using PMCMP for nonparametric pairwise comparison
library(PMCMR)
posthoc.friedman.conover.test(voicescrollsearch$Errrors, 
                              voicescrollsearch$Technique, voicescrollsearch$Subject, 
                              p.adjust.method="holm")

#Examine Likert perceived Effort
library(plyr)
ddply(voicescrollsearch, ~Technique, function(data) summary(data$Effort))
ddply(voicescrollsearch, ~Technique, summarise, mean.Effort=mean(Effort), sd.Effort=sd(Effort))
hist(voicescrollsearch[voicescrollsearch$Technique=="Scroll",]$Effort, breaks=c(1:7), xlim = c(1:7))
hist(voicescrollsearch[voicescrollsearch$Technique=="Search",]$Effort, breaks = c(1:7), xlim = c(1:7))
hist(voicescrollsearch[voicescrollsearch$Technique=="Voice",]$Effort, breaks = c(1:7), xlim = c(1:7))
plot(Effort~Technique, data=voicescrollsearch, ylim=c(1:7))

#friedmam test on Effort
library(coin)
friedman_test(Effort~Technique|Subject, data=voicescrollsearch, distribution="asymptotic")
#non-sig p value means no sig. difference between tests