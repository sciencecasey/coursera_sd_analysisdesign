####2 IV populations, one outcome of interest with 2 possible catgorical responses
prefsABsex=read.csv("prefsABsex")
plot(prefsABsex[prefsABsex$Sex == "M"]$Pref)
plot(prefsABsex[prefsABsex == "F"]$Pref)

#make a table and chi squared
prefs=xtabs(Pref+Sex, data=prefsABsex) #plus for 2 variables of interest
view(prfs)
chisq.test(prefs)

# G test-- like a chi squared but more accurate
library(RVAideMemoire)
G.test(prfs)

#Fisher's exact test (for 2x2 tests or for any number)
fisher.test(prfs)

#####2IV, one outcome of interest with 3 (or more) possible categorical responses
prefsABCsex=read.csv("prefsABCsex")
plot(prefsABsex[prefsABCsex$Sex == "M"]$Pref)
plot(prefsABsex[prefsABCsex == "F"]$Pref)

#make a table and chi squared
prefs=xtabs(Pref+Sex, data=prefsABCsex) #plus for 2 variables of interest
view(prfs)
chisq.test(prefs)

# G test-- like a chi squared but more accurate
G.test(prfs)

#Fisher's exact test (for 2x2 tests or for any number)
fisher.test(prfs)

#manual post hoc binomial tests for (m)ales
##see how much and where the differences lie
ma=binom.test(sum(prefsABCsex[prefsABCsex$Pref == "A"]), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
ma=binom.test(sum(prefsABCsex[prefsABCsex$Pref == "B"]), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
ma=binom.test(sum(prefsABCsex[prefsABCsex$Pref == "C"]), nrow(prefsABCsex[prefsABCsex$Sex == "M",]), p=1/3)
p.adjust(c(ma$p.value, mb$p.valus, mc$p.value), method="holm") #mult. comparisons adjustment


##manual post hoc binomial tests for (fem)ales
ma=binom.test(sum(prefsABCsex[prefsABCsex$Pref == "A"]), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
ma=binom.test(sum(prefsABCsex[prefsABCsex$Pref == "B"]), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
ma=binom.test(sum(prefsABCsex[prefsABCsex$Pref == "C"]), nrow(prefsABCsex[prefsABCsex$Sex == "F",]), p=1/3)
p.adjust(c(fa$p.value, fb$p.valus, fc$p.value), method="holm") #mult. comparisons adjustment
