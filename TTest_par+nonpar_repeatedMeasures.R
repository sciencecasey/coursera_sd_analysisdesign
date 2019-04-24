#repeated measures is a within subjects
#reduces variances
#potential for carryover- must be counterbalanced
#add order variable to test if there is an effect based on order (hope there isn't)

#full, latin squares, or balanced latin squares
#all sequences expressed for Full
#latin square: each condition occupy same order exact same number of times
## latin square not all conditions follow the others exact number of times #can still have carryover
#balanced latin square: every cond. follows every other exact same number of times

#All participants (20) use a phone to scroll and search(IV, 2 levels within) in different orders
#DVs: Time to land on pg (continuous), #Errors (discrete), Perceived effort (ordinal [likhert1:7])
#importdata
scrollsearch=read.csv("scrollsearch.csv")
scrollsearch$Subject=factor(scrollsearch$Subject)
scrollsearch$Order=factor(scrollsearch$Order)
str(scrollsearch)

#descriptive stats by technique factor with cont. DV time
library(plyr)
ddply(scrollsearch, ~Technique, function(data) summary(data$Time))
ddply(scrollsearch, ~Technique, summarise, mean.Time=mean(Time), sd.Time=sd(Time))

#view plots
hist(scrollsearch[scrollsearch$Technique=="Scroll",]$Time)
hist(scrollsearch[scrollsearch$Technique=="Search",]$Time)
plot(Time~Technique, data=scrollsearch)

#test normality assumption
shapiro.test(scrollsearch[scrollsearch$Technique=="Scroll",]$Time)
shapiro.test(scrollsearch[scrollsearch$Technique=="Search",]$Time)

#when fitting model to residual, must add the correlation error function telling R that Tehnique has a res. factor of Subject mult. comparisons
m=aov(Time~Technique+Error(Subject/Technique), data=scrollsearch) #subject correlates across technique factor
##this gives residuals for Suject and for Subject:Technique
shapiro.test(residuals(m$Subject))
qqnorm(residuals(m$Subject)); qqline(residuals(m$Subject))
shapiro.test(residuals(m$"Subject:Technique"))
qqnorm(residuals(m$"Subject:Technique")); qqline(residuals(m$'Subject:Technique'))

#test homoscedasticity
library(car)
leveneTest(Time~Technique, data=scrollsearch, center=median)

#test for order effect
#for within subject analysis, better in R to have one row per subject and mult trials on same row
library(reshape2)
searchscrollwideorder=dcast(scrollsearch, Subject~Order, value.var="Time") #gives subject grouped in rolls and orders in new columns
#do t test for paired comparisons to test normality of order effects
t.test(searchscrollwideorder$"1", searchscrollwide$"2", paired=TRUE, var.equal = TRUE) #paired=true makes it a paired sample 
#nonsignificant results are good as suggests no order effects


#paired samples t test
t.test(searchscrollwideorder$Search, searchscrollwideorder$Scroll, paired=TRUE, var.equal = TRUE)
plot(Time~Technique, data=scrollsearch)


#non-parametric t test with discrete dv # Errors and Effort (likert ordinal scale) 
#Errors
library(plyr)
ddply(scrollsearch, ~Technique, function(data) summary(data$Errors))
ddply(scrollsearch, ~Technique, summarise, mean.Errors=mean(Errors), sd.Errors=sd(Errors))

#view plots
hist(scrollsearch[scrollsearch$Technique=="Scroll",]$Errors)
hist(scrollsearch[scrollsearch$Technique=="Search",]$Errors)
plot(Errors~Technique, data=scrollsearch)

#fit to a poisson distribution which is discrete/normal
library(fitdistrplus)
fit=fitdist(scrollsearch[scrollsearch$Technique="Search"]$Errors, "pois", discrete=TRUE)
gofstat(fit) #goodness of fit test
fit=fitdist(scrollsearch[scrollsearch$Technique="Scroll"]$Errors, "pois", discrete=TRUE)
gofstat(fit) #goodness of fit test
#non-sig p value is good as suggests not sig. different from the poisson distribution

#Wilcox sign-ranked test on Errors 
#equivalent of parametric ttest
#use |Subject to denote that it's calc within subjects
library(coin)
wilcox.test(Errors~Technique|Subject, data=scrollsearch, distribution="exact")

#Effort from the 1-7 likert scale
ddply(scrollsearch, ~Technique, function(data) summary(data$Effort))
ddply(scrollsearch, ~Technique, summarise, mean.Effort=mean(Effort), sd.Effort=sd(Effort))
hist(scrollsearch[scrollsearch$Technique=="Scroll",]$Effort, breaks=c(1:7), xlim = c(1:7))
hist(scrollsearch[scrollsearch$Technique=="Search",]$Effort, breaks = c(1:7), xlim = c(1:7))
plot(Effort~Technique, data=scrollsearch, ylim=c(1:7))
#Wilcox sign-ranked test on Effort 
#equivalent of parametric ttest
#use |Subject to denote that it's calc within subjects
library(coin)
wilcox.test(Effort~Technique|Subject, data=scrollsearch, distribution="exact")
