#Notes
#basic design::::y~x +e
#example: #ofpages~site+e
#variable types can be: 
#  - numeric
#  - categorical/nominal
#IVs are also called Factors and can include Levels and be between or within subject
#between subject: each subj experiences only one level of a factor
#within subject: each subj experiences multiple levels of a factor

#A/B Test: Independent Samples T Test
#each subject lands on one page, A or B
#which has more views
pgviews=read.csv("pgviews.csv")
pgviews$Subject=factor(pgviews$Subject)
summary(pgviews)

#Descriptive Stats
library(plyr)
ddply(pgviews, ~Site, function(data) summmary(data$Pages))
ddply(pgviews, ~Sites, summarise, Pages.mean=mean(Pages), Pages.sd=sd(Pages))

#graph
hist(pgviews[pgviews$Site=="A"]$Pages)
hist(pgviews[pgviews$Site=="B"]$Pages)
plot(Pages~Site, data=pgviews)

#independent samples T test
t.test(Pages~Site, data=pgviews, var.equal=TRUE)

