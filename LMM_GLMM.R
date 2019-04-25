#If using Within Subjects must use the Linear Mixed or General Linear Mixed Models
#random effects rather AND fixed effects for LMM or GLMM
#Subject is a random effect without levels (fixed effects are factors )
#don't need sphericity anymore, instead model the form directly 
#more computationally intensive and large denominator DFs
#must consider nesting/nested effects
##if levels shouldn't be pooled by label alone, might be nested (as in trial # as there's no consistency between trial 1s across subjects/conditions)
###not meaningful to consider the levels alone== use nesting

#Linear Mixed Model with WPM
#recall the mlbtext data with iPhone/Samsung and sit/stand/walk 
##2x3 factorial anova (keyboard= between, postures=within); output was averages but with mixed models, keep all answers
mbltexttrials=read.csv("mlbtexttrials.csv")
mbltexttrials$Subject=factor(mbltexttrials$Subject)
mbltexttrials$Order=factor(mbltexttrials$Order)
mbltexttrials$Trial=factor(mbltexttrials$Trial)
summary(mbltexttrials)

#explore data
library(plyr)
ddply(mbltexttrials, ~Keyboard*Posture, function(data) summary(data$WPM))
ddply(mbltexttrials, ~Keyboard*Posture, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))
hist(mbltexttrials[mbltexttrials$Keyboard=="iPhone" & mbltexttrials$Posture=="Sit",]$WPM)
hist(mbltexttrials[mbltexttrials$Keyboard=="iPhone" & mbltexttrials$Posture=="Stand",]$WPM)
hist(mbltexttrials[mbltexttrials$Keyboard=="iPhone" & mbltexttrials$Posture=="Walk",]$WPM)
hist(mbltexttrials[mbltexttrials$Keyboard=="Samsung" & mbltexttrials$Posture=="Sit",]$WPM)
hist(mbltexttrials[mbltexttrials$Keyboard=="Samsung" & mbltexttrials$Posture=="Stand",]$WPM)
hist(mbltexttrials[mbltexttrials$Keyboard=="Samsung" & mbltexttrials$Posture=="Walks",]$WPM)
box.plot(WPM~Keyboard*Posture, data=mbltexttrials, xlab="Keyboard Posture", ylab="WPM")
with(mbltexttrials, interaction.plot(POsture, Keyboard, WPM, ylim=c(0, max(mbltexttrials$WPM))))

#LIbraries for LMM on WPM
library(lme4)
library(lmerTest)
library(car)

#set sum to zero contrasts for Anova calls (car package)
contrasts(mbltexttrials$Keyboard)="contr.sum"
contrasts(mbltexttrials$Trial)="contr.sum"
contrasts(mbltexttrials$Order)="contr.sum"
contrasts(mbltexttrials$Trial)="contr.sum"

#LMM Order effect test
#make sure counterbalanced correctly
m=lmer(WPM~(Keyboard*Order)/Trial +(1|Subject), data=mbltexttrials) #the slash Trial nests trials within keyboard and order
Anova(m, type=3, test.statistic="F")
#no difference in the interaction shows us that the order didn't affect the data and counterbalance worked

#LMM main effect fo POsture
m=lmer(WPM~(Keyboard*Posture)/Trial + (1|Subject), data=mbltexttrials)
Anova(m, type=3, test.statistic = "F")

#Why don't we use Trial random instead of a nested Fixed?
#can do this as well, make sure the results are similar
m=lmer(WPM~(Keyboard*Posture)/(1|Trial)+(1|Subject), data=mbltexttrials)
Anova(m, type=3, test.statistic = "F")

#pairwise comparistons
library(multcomp)
library(lsmeans)
summary(glht(m, lsm(pairwise~Keyboad*Posture)), test-adjusted(type="holm"))
with(mbltexttrials, interaction.plot(Posture, Keyboard, WPM, ylim=c(0, max(mbltexttrials$WPM))))
     

#now with GLMM on Error Rate to model the poisson model as a count variable 
#turn Error rate into Errors out of 100
mbltexttrials$Errors=mbltexttrials$Error_Rate*100
summary(mbltexttrials)

#explore new column
library(plyr)
ddply(mbltexttrials, ~Keyboard*Posture, function(data) summary(data$Errrors))
ddply(mbltexttrials, ~Keyboard*Posture, summarise, mean.Errors=mean(Errors), sd.Errors=sd(Errors))
boxplot(Errors~Keyboard*Posture, data=mbltexttrials, xlab="Keyboard.Posture", ylab="Errors")
with(mbltexttrials, interaction.plot(Posture, Keyboard, Errors, ylim=c(0, max(mbltexttrials$Errors))))

#see if Poisson dist.
library(fitdistrplus)
fit=fitdistr(mbltexttrials[mbltexttrials$Keyboard=="iPhone" & mbltexttrials$Posture=="Sit"]$Errors)
gofstat(fit)
fit=fitdistr(mbltexttrials[mbltexttrials$Keyboard=="iPhone" & mbltexttrials$Posture=="Stand"]$Errors)
gofstat(fit)
fit=fitdistr(mbltexttrials[mbltexttrials$Keyboard=="iPhone" & mbltexttrials$Posture=="Walk"]$Errors)
gofstat(fit)
fit=fitdistr(mbltexttrials[mbltexttrials$Keyboard=="Samsung" & mbltexttrials$Posture=="Sit"]$Errors)
gofstat(fit)
fit=fitdistr(mbltexttrials[mbltexttrials$Keyboard=="Samsung" & mbltexttrials$Posture=="Stand"]$Errors)
gofstat(fit)
fit=fitdistr(mbltexttrials[mbltexttrials$Keyboard=="Samsung" & mbltexttrials$Posture=="Walk"]$Errors)
gofstat(fit)

#load libraries for GLMMs with poisson regression for Errors
library(lme4)
library(car)

#set sum to zero contrasts for Anova
contrasts(mbltexttrials$Keyboard)="contr.sum"
contrasts(mbltexttrials$Posture)="contr.sum"
contrasts(mbltexttrials$Trial)="contr.sum"

#main GLMM tests
#Keyboard, posture, Keyboard:Posture, and trial are fixed effects
#Trial is nested within Keyboard, posture, and kayboard:posture
#Subject is a random effect (repeated effect)
#nAGQ is default 1 which completes much more slowly
m=glmer(Errors~(Keyboard*Posture)/Trial +(1|Subject), data=mbltexttrials, family=poisson, nAGQ=0)
Anova(m, type=3)

#post hoc pairwise comp.
with(mbltexttrials, interaction.plot(Posture, Keyboard, Errors, 
                                     ylim=c(0,max(mbltexttrials$Errors))))
library(multcomp)
library(lsmeans)
summary(glht(m, lsm(pairwise~Keyboard*Posture)), test-adjusted(type="holm"))
###lsmeans is outdatted and updated to eemeans; see help 'transition'
?transition



