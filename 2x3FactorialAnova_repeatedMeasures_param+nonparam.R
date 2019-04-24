#interaction effects for repeated measures (2x3 mixed factorial design)
#IV1keyboard: between-- apple, samsung
#IV2posture: within (do all of them)-- sitting, standing, walking
#DVs: WPM, Error rate %
#24 subject, 20 phrases averaged in each of the 3 postures

mbltext=read.csv("mbltext.csv")
mbltext$Subject=factor(mbltext$Subject)
mbltext$Order=factor(mbltext$Order)
str(mbltext)

#explore
library(plyr)
ddply(mbltext, ~Keyboard*Posture, function(data) summary(data$WPM))
ddply(mbltext, ~Keyboard*Posture, summarise, WPM.mean=mean(WPM), WPM.sd=sd(WPM))
hist(mbltext[mbltext$Keyboard=="iPhone" & mbltext$Posture=="Sit",]$WPM)
hist(mbltext[mbltext$Keyboard=="iPhone" & mbltext$Posture=="Stand",]$WPM)
hist(mbltext[mbltext$Keyboard=="iPhone" & mbltext$Posture=="Walk",]$WPM)
hist(mbltext[mbltext$Keyboard=="Samsung" & mbltext$Posture=="Sit",]$WPM)
hist(mbltext[mbltext$Keyboard=="Samsung" & mbltext$Posture=="Stand",]$WPM)
hist(mbltext[mbltext$Keyboard=="Samsung" & mbltext$Posture=="Walk",]$WPM)
boxplot(WPM~Keyboard*Posture, data=mbltext, xlab="Keyboard.Posture", ylab="WPM")
with(mbltext, interaction.plot(Posture, Keyboard, WPM, ylim=c(0, max(mbltext$WPM)))) #interactionplot

#double check the counterbalance for Posture Order
library(ez)
m=ezANOVA(dv=WPM, between=Keyboard, within=Order, wid=Subject, data=mbltext)
m$Mauchly #make sure not stat. sig difference from sphericity model
m$ANOVA #want to make sure no interaction significance to show counterbalance worked

#Anova on Posture
m=ezANOVA(dv=WPM, between=Keyboard, within=Posture, wid=Subject, data=mbltext)
m$Mauchly #make sure not stat. sig difference from sphericity model
m$ANOVA
#note "ges" is the generalized eta-squared,effect size

#corrected DFS for the corrected effects
pos=match(m$"Sphericity Correction"$Effect, m$ANOVA$Effect)
m$Spericity$GGe.DFn=m$Sphericity$GGe*m$ANOVA$DFn[pos]
m$Spericity$GGe.DFd=m$Sphericity$GGe*m$ANOVA$DFn[pos]
m$Spericity$HFe.DFn=m$Sphericity$GGe*m$ANOVA$DFn[pos]
m$Spericity$HFe.DFd=m$Sphericity$GGe*m$ANOVA$DFn[pos]
m$Sphericity #view results

#post hoc pairwise on sig main and interaction effects 
library(reshape2)
#make wide format table
mbltext_wide=dcast(mbltext, Subject +Keyboard~Posture, value.var = "WPM")
sit=t.test(mbltext_wide$Sit~Keyboard, data=mbltext_wide) #Iphone v Samsung WPM sitting
std=t.test(mbltext_wide$Stand~Keyboard, data=mbltext_wide)
wlk=t.test(mbltext_wide$Walk~Keyboard, data=mbltext_wide)
p.adjust(c(sit$p.value, std$p.value, wlk$p.value), method="holm")

#test within iPhone
t.test(mbltext_wide[mbltext_wide$Keyboard=="iPhone",]$Sit, 
       mbltext_wide[mbltext_wide$Keyboard=="iPhone",]$Walk, paired=TRUE)
boxplot(mbltext_wide[mbltext_wide$Keyboard=="iPhone",]$Walk,xlab="iPhone Sit v iPhone Walk", ylab="WPM")

#Non-parametric measures of Error rate %
##ART Aligned Rank Transform procedure
library(plyr)
ddply(mbltext, ~Keyboard*Posture, function(data) summary(data$Error))
ddply(mbltext, ~Keyboard*Posture, summarise, WPM.mean=mean(WPM), WPM.sd=sd(Error))
hist(mbltext[mbltext$Keyboard=="iPhone" & mbltext$Posture=="Sit",]$Error)
hist(mbltext[mbltext$Keyboard=="iPhone" & mbltext$Posture=="Stand",]$Error)
hist(mbltext[mbltext$Keyboard=="iPhone" & mbltext$Posture=="Walk",]$Error)
hist(mbltext[mbltext$Keyboard=="Samsung" & mbltext$Posture=="Sit",]$Error)
hist(mbltext[mbltext$Keyboard=="Samsung" & mbltext$Posture=="Stand",]$Error)
hist(mbltext[mbltext$Keyboard=="Samsung" & mbltext$Posture=="Walk",]$Error)
boxplot(WPM~Keyboard*Posture, data=mbltext, xlab="Keyboard.Posture", ylab="Error_Rate")
with(mbltext, interaction.plot(Posture, Keyboard, WPM, ylim=c(0, max(mbltext$WPM)))) #interactionplot

#Aligned Rank Transform
library(ARTool)
m=art(Error~Keyboard*Posture +(1|Subject), data=mbltext) #uses the linear mixed model
anova(m)
shapiro.test(residuals(m)) #check if normal
qqnorm(residuals(m)); qqline(residuals(m)) #check if normal

#conduct post hoc pairwise comparisons
with(mbltext, interaction.plot(Posture, Keyboard, Error, ylim=c(0, max(mbltext$Error))))
library(lsmeans)
lsmeans(artlm(m, "Keyboard"), pairwise~Keyboard) #contrasts the galaxy and iphone
lsmeans(artlm(m, "Posture"), pairwise~Posture) #contrasts the postures
#to test interaction, must use a different approach
library(phia) #looks at the difference between differences
testInteractions(artlm(m, "Keyboard:Postures"), pairwise=c("Keyboard", "Posture"), adjustment="holm")
#is difference between A-B sig different than difference between C-D

