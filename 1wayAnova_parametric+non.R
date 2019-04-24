#What if add one more programming language, 1 way Anova
#1 way refers to 1 factor (IDE) 3 levels (VStudio, Eclipse, PyCharm) where each subject only does one of the 3 
ide3=read.csv("ide3.csv")
#follow same instructions as AnovaAssumptions tests but add PyCharm as an indicator
#view plots
hist(ide3[ide3$IDE=="PyCharm",]$Time)
hist(ide3[ide3$IDE=="VStudio",]$Time)
hist(ide3[ide3$IDE=="Eclipse",]$Time)

#Test normality assumption
shapiro.test(ide3[ide3$IDE=="PyCharm",]$Time) 
shapiro.test(ide3[ide3$IDE=="VStudio",]$Time) 
shapiro.test(ide3[ide3$IDE=="Eclipse",]$Time) 

#test residuals assumption
m=aov(Time~IDE, data=ide3)
shapiro.test(residuals(m))
qqnorm(residuals(m)); qqline(residuals(m))

#Kolmogorov-Smirnov test for log normality since wasn't normal
#fit distribution to lognormal estimate fit parameters
#ks test using parameters we extracted from the fit (mean and sd)
library(MASS)
fit=fitdistr(ide3[ide3$IDE=="PyCharm",]$Time, "lognormal")$estimate
ks.test(ide3[ide3$IDE=="PyCharm",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact = TRUE)
fit=fitdistr(ide3[ide3$IDE=="VStudio",]$Time, "lognormal")$estimate
ks.test(ide3[ide3$IDE=="VStudio",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact = TRUE)
fit=fitdistr(ide3[ide3$IDE=="Eclipse",]$Time, "lognormal")$estimate
ks.test(ide3[ide3$IDE=="Eclipse",]$Time, "plnorm", meanlog=fit[1], sdlog=fit[2], exact = TRUE)

#compute new logcolumn and retest
ide3$logTime=log(ide3$Time) #add column for log
shapiro.test(ide3[ide3$IDE=="PyCharm",]$logTime)
m=aov(logTime~IDE, data=ide3) #fit model
shapiro.test(residuals(m))
qqnorm(residuals(m)); qqline(residuals(m))

#test homoscedasticity
library(car)
leveneTest(logTime~IDE, data=ide3, center=median)

#now use log for 1 way anova
m=aov(logTime~IDE, data=ide3) #fit model #repeated to ensure using correct m variable
anova(m)

#the omnibus anova was sig so we do post hoc ind. samples t test between levels
plot(Time~IDE, data=ide3) #explore
library(multcomp)
summary(glht(m, mcp(IDE="Tukey")), test=adjusted(type = "holm")) #Tukey means compare all pairs
#OR can use pairwise comparisons for same result [below]
summary(glht(m, lsm(pairwise=IDE)), test=adjusted(type="holm"))

#non-parametric equivalent of One-Way ANOVA
library(coin)
kruskal_test(Time~IDE, data=ide3, distribution="asymptotoic") #can't use exact with 3 levels
kruskal_test(logTime~IDE, data=ide3, distribution="asymptotoic") #can't use exact with 3 levels

#Manual post hoc Mann Whitney U comparisons
vs.ec=wilcox.test(ide3[ide3$IDE=="VStudio"]$Time, ide3[ide3$IDE=="Eclipse"]$Time, exact=FALSE)
vs.py=wilcox.test(ide3[ide3$IDE=="VStudio"]$Time, ide3[ide3$IDE=="PyCharm"]$Time, exact=FALSE)
ec.py=wilcox.test(ide3[ide3$IDE=="Eclipse"]$Time, ide3[ide3$IDE=="PyCharm"]$Time, exact=FALSE)
p.adjust(c(vs.es$p.value, vs.py$p.value, ec.py$p.value), method="holm")

