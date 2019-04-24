#if can't manipulate a confound, try to control for it; if can't control, record/measure it
#Avova Assumptions::Independence; normality; homoscedaticity (similar variance)
#Anova cares about the normality of the residuals (if residuals are normally distributed, we know that it's not systematic)

#participants use VStudio or Eclipse to code
#which does tasks quicker

#look at normality assumption with distributions

ide2=read.csv("ide2.csv")

#View the histograms to see if looks normally dist.  
hist(ide2[ide2$IDE=="VStudio",]$Time)
hist(ide2[ide2$IDE=="Eclipse",]$Time)
plot(Time~IDE, data=ide2)

#Test normality assumption
shapiro.test(ide2[ide2$IDE=="VStudio",]$Time)  #sig p value suggesting we significantly differ from normal 
shapiro.test(ide2[ide2$IDE=="Eclipse",]$Time)

#test residual assumtptions
m=aov(Time~IDE, data=ide2) #fit an anova to model
shapiro.test(residuals(m)) #test the residuals 
qqnorm(residuals(m)); qqline(residuals(m)) #plot the residuals and see if deviate greatly

#Kolmogorov-Smirnov test for log normally since wasn't normal
#fit distribution to lognormal estimate fit parameters
#ks test using parameters we extracted from the fit (mean and sd)
library(MASS)
fit=fitdistr(ide2[ide2$IDE="VStudio",]$Time, "lognormal")$estimate #transform function lognormal on VStudio 
ks.test(ide2[ide2$IDE=="VStudio",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE) #plot probability normal agains the log mean and log sd
fit=fitdistr(ide2[ide2$IDE="Eclipse",]$Time, "lognormal")$estimate
ks.test(ide2[ide2$IDE=="Eclipse",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact=TRUE)

#t test for homoscedasticity
library(car)
#levene test using mean
leveneTest(Time ~IDE, data=ide2, center=mean)
#Bown-Forsythe test
#more robust to outliers
leveneTest(Time~IDE, data=ide2, center=median) 

#welch t-test for unequal variances
#used when differences in variances var.equal=FALSE
#won't solve a violation of normality
t.test(Time~IDE, data=ide2, var.equal=FALSE)

##Transform data so it conforms to normality assumption (since we say it was not normally distributed by the small pvalue)
#create new column in ide2 definted as log(Time) since we might be log distributed
ide2$logTime=log(ide2$Time)

#View the histograms to see if looks more normal than before. 
hist(ide2[ide2$IDE=="VStudio",]$logTime)
hist(ide2[ide2$IDE=="Eclipse",]$logTime)

#test the logTime for normality
shapiro.test(ide2[ide2$IDE=="VStudio",]$logTime) #no longer sig p value suggesting we don't significantly differ from normal 
shapiro.test(ide2[ide2$IDE=="Eclipse",]$logTime)
m=aov(logTime~IDE, data=ide2) #fit an anova to model
shapiro.test(residuals(m)) #test the residuals 
qqnorm(residuals(m)); qqline(residuals(m)) #plot the residuals and see if deviate greatly

#test the homoscedasticity of log 
#levene test using mean
leveneTest(logTime ~IDE, data=ide2, center=mean)
#Bown-Forsythe test
#more robust to outliers
leveneTest(logTime~IDE, data=ide2, center=median) 

#t-test with equal variances
t.test(logTime~IDE, data=ide2, var.equal=TRUE)

#Mann-Whitney U test (nonparametric TTest) to see rank of quickest

#doesn't use the underlying anova assumptions to evaluate t test
library(coin)
wilcox_test(Time~IDE, data=ide2, distribution="exact") #got a sig. p value
wilcox_test(logTime~IDE, data=ide2, distribution="exact") #why log sig value as well
#for rank tests, the log is the same