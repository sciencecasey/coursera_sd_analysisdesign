#Generalized Linear Models (GLM)-- allows for non-normal distr. 
#(LM relates to a normal response==like an anova
#GLM can ONLY USE BETWEEN SUBJECTS analysis
#EX1we'll look at website preferences ABC by sex
#multinomial (multiple categories of responses) logistic regression
#EX2search/scroll/voice data on Effort 1-7 Likert scale, pretend it's a between sub
#analyze with ordinal logistic regression
#EX3 search/scroll/voice data on Error count, pretend it's between sub
#analyze with Poisson regression

#EX1 Multinomial
#categorical variables as response
summmary(prefsABCsex)
#analyze teh preferences by sex using nomial logistic/multinomial logistic regression
library(nnet)
library(car)
#sut sum-to-zero contrasts for Anova call
contrasts(prefsABCsex$Sex)="contr.sum"
m-multinom(Pref~Sex, data=prefsABCsex)
Anova(m, type=3) #from car not stats pkg
#if only 2 responses we would use binomial regression, same syntax as Poisson below but family=binomial
#outputs a Chi squared

#if stat sig, do differnces for independent levels proportions
ma=binom.test(sum(prefsABCsex[prefsABCsex$Sex=="M",]$Pref=="A"), 
              nrow(prefsABCsex[prefsABCsex$Sex=="M",]), p=1/3)
mb=binom.test(sum(prefsABCsex[prefsABCsex$Sex=="M",]$Pref=="B"), 
              nrow(prefsABCsex[prefsABCsex$Sex=="M",]), p=1/3)
mc=binom.test(sum(prefsABCsex[prefsABCsex$Sex=="M",]$Pref=="C"), 
              nrow(prefsABCsex[prefsABCsex$Sex=="M",]), p=1/3)
p.adjust(c(ma$p.value, mb$p.value, mc$p.value), method="holm")

fa=binom.test(sum(prefsABCsex[prefsABCsex$Sex=="F",]$Pref=="A"), 
              nrow(prefsABCsex[prefsABCsex$Sex=="F",]), p=1/3)
fb=binom.test(sum(prefsABCsex[prefsABCsex$Sex=="F",]$Pref=="B"), 
              nrow(prefsABCsex[prefsABCsex$Sex=="F",]), p=1/3)
fc=binom.test(sum(prefsABCsex[prefsABCsex$Sex=="F",]$Pref=="C"), 
              nrow(prefsABCsex[prefsABCsex$Sex=="F",]), p=1/3)
p.adjust(c(fa$p.value, fb$p.value, fc$p.value), method="holm")

#EX 2 Ordinal Logistic regression for Likert responses
##Multinomial dist. with cumulative logit link fn
summary(voicescrollsearch)
voicescrollsearch2=voicescrollsearch
#convert to between subjects
voicescrollsearch2$Subject=(1:nrow(voicescrollsearch2))
voicescrollsearch2$Subject=factor(voicescrollsearch2$Subject)
#remove Order as we're making it betweel
voicescrollsearch2$Order= NULL
str(voicescrollsearch2)

#recall Effort Likert data
library(plyr)
ddply(voicescrollsearch2, ~Technique, function(data) summary(data$Effort))
ddply(voicescrollsearch2, ~Technique, summarise, mean.Effort=mean(Effort), sd.Effort=sd(Effort))
hist(voicescrollsearch2[voicescrollsearch2$Technique=="Scroll",]$Effort, breaks=c(1:7), xlim = c(1:7))
hist(voicescrollsearch2[voicescrollsearch2$Technique=="Search",]$Effort, breaks = c(1:7), xlim = c(1:7))
hist(voicescrollsearch2[voicescrollsearch2$Technique=="Voice",]$Effort, breaks = c(1:7), xlim = c(1:7))
plot(Effort~Technique, data=voicescrollsearch2, ylim=c(1:7))

#analyze Effort rating by Technique with ordinal logistic regression
library(MASS)
library(car)
voicescrollsearch2$Effort=ordered(voicescrollsearch2$Effort) #set Effort to an "ordered" class
#set sum to zero contrasts for Anova call
contrasts(voicescrollsearch2$Technique)="cont.sum"
m=polr(Effort~Technique, data=voicescrollsearch2) #polr is ordinal logistic regression model
Anova(m, type = 3)

#post hoc comparisons
library(multcomp)
summary(glht(m, mcp(Technique="Tukey")), test-adjusted(type = "holm"))

#EX 3:: Poisson regression for count responses
#poisson distr. with log link fn

#Errror response is a count response
#recall data
ddply(voicescrollsearch2, ~Technique, function(data) summary(data$Errors))
ddply(voicescrollsearch2, ~Technique, summarize, Errors.mean=mean(Errors), Errors.sd=sd(Errors))
hist(voicescrollsearch2[voicescrollsearch2$Technique=="Scroll",]$Errors)
hist(voicescrollsearch2[voicescrollsearch2$Technique=="Search",]$Errors)
hist(voicescrollsearch2[voicescrollsearch2$Technique=="Voice",]$Errors)
plot(Errors~Technique, data=voicescrollsearch2)

#re-verify that it's POisson distributed
library(fitdistrplus)
fit=fitdist(voicescrollsearch2[voicescrollsearch2$Technique=="Search",]$Errors, "pois", discrete=TRUE)
gofstat(fit)
fit=fitdist(voicescrollsearch2[voicescrollsearch2$Technique=="Scroll",]$Errors, "pois", discrete=TRUE)
gofstat(fit)
fit=fitdist(voicescrollsearch2[voicescrollsearch2$Technique=="Voice",]$Errors, "pois", discrete=TRUE)
gofstat(fit)

#analyse Poisson regresson
#set sum to zero contrasts for Anova call
contrasts(voicescrollsearch2$Technique)="contr.sum"
#family parameter identifies the dist. and the link fn
m=glm(Errors~Technique, data=voicescrollsearch2, family=poisson)
Anova(m, type=3)
qqnorm(residuls(m)); qqline(residuals(m)) #okay to deviate, no normality assumption with Poisson

#pairwise comparisons
library(multcomp)
summary(glht(m, mcp(Technique="Tukey")), test-adjusted(type="holm"))
