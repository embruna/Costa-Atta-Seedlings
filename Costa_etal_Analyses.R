
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN: 

setwd("/Users/emiliobruna/Dropbox/Alan/Planilhas doutorado/Capitulo2")

library(gdata)
library(lme4)
library(ggplot2)
library(reshape)
library(maps)
library(WDI)
library(RColorBrewer)
library(lattice)
library(coefplot2) ## for coefplot2
library(reshape)
library(plyr)
library(gridExtra)
library(emdbook)
#CLear out everything from the environment
rm(list=ls())
##################
#################
###DATA ENTRY AND CLEANUP
##################
#################
#Step 1: load the individual CSV files and save them as dataframes
#This is the pooled data
NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6_dez2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#Need to do some summaruzing before binding the following two to the pooled data
VEG<-read.csv("1-ActiveNests_CensoVeg_dez2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
AIRTEMPHUMID<-read.csv("7-ActiveNests_TempAirHumid_dez2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )


summary(NEST.DATA)
summary(VEG)
summary(AIRTEMPHUMID)

NEST.DATA_both<-NEST.DATA[NEST.DATA$habitat=="CR"|NEST.DATA$habitat=="CD",]
NEST.DATA_CR<-NEST.DATA[NEST.DATA$habitat=="CR",]
NEST.DATA_CD<-NEST.DATA[NEST.DATA$habitat=="CD",]

NEST.DATA_both <- droplevels(NEST.DATA_both)
NEST.DATA_CR <- droplevels(NEST.DATA_CR)
NEST.DATA_CD <- droplevels(NEST.DATA_CD)
#histogram(NEST.DATA_CD$grass.bmass)
av=aov(grass.bmass ~ location*habitat + nest, data = NEST.DATA_both) 
summary(av)
#glm((NEST.DATA$grass.bmass+0.5) ~ NEST.DATA$location + NEST.DATA$habitat, family=Gamma)
#fm1 <- lmer((grass.bmass+0.5) ~ location + (1 | nest/habitat), data=NEST.DATA_CD)
#print(fm1)
#(fm1M <- update(fm1, REML = FALSE))

str(NEST.DATA_both)
str(NEST.DATA_CR)  #need to check to see if factors are corrrctly coded as factors
str(NEST.DATA_CD)#to make "nest" and "plot" factors, and also reorder variables so that "nest" and CR are the reference variables use transform()

NEST.DATA_both <- transform(NEST.DATA_both,
                              nest=factor(nest),
                              plot=factor(plot),
                              location=factor(location,levels=c("nest","close", "far")))



NEST.DATA_CR <- transform(NEST.DATA_CR,
                    nest=factor(nest),
                    plot=factor(plot),
                    location=factor(location,levels=c("nest","close", "far")))

NEST.DATA_CD <- transform(NEST.DATA_CD,
                              nest=factor(nest),
                              plot=factor(plot),
                              location=factor(location,levels=c("nest","close", "far")))





fm1 <- lmer((grass.bmass+1) ~ location * habitat +  (1 | nest), data=NEST.DATA_both)
summary(fm1)
(fm1M <- update(fm1, REML = FALSE))

fm2 <- lmer((grass.bmass+1) ~ habitat + (1 | nest),  data=NEST.DATA_both)
summary(fm2)
(fm2M <- update(fm2, REML = FALSE))

fm3 <- lmer((grass.bmass+1) ~ location + (1 | nest),  data=NEST.DATA_both)
summary(fm3)
(fm3M <- update(fm3, REML = FALSE))


anova(fm1,fm2)
anova(fm1,fm3)
anova(fm2,fm3)

plot(fm1)
#qqnorm(fm1, ~ranef(., level=2))
qqnorm(resid(fm1))

mp1 <- glmer((grass.bmass+1) ~ location + (1 | nest), data=NEST.DATA_CD, family="Gamma")








step 2: bind the dataframes of all journals together
ALLJOURNALS<-rbind(BITR, ARES, AGRON, NAJFM, AJB, CONBIO, ECOLOGY, BIOCON, JECOL, JTE)
#use only 1985 on
ALLJOURNALS<-ALLJOURNALS[ALLJOURNALS$YEAR>=1985,]
#2x that it all looks ok
summary(ALLJOURNALS)
#2x check - are there any with gender missing?
MISSING=subset(ALLJOURNALS, subset=(GENDER=="?"))
MISSING
#######################
#Some basic summary stats and their depiction in Figure 1
#Men and Women (# and oproportion) for each category of Editor:
#(Editor-in-Chief (EIC), Subject Editor (SE), and Associate Editor (AE))
#######################
#NEED to subset the data to do some counts of the numbers of EICs
countEIC<-ALLJOURNALS[ALLJOURNALS$CATEGORY=="EIC",]
#name the columns
countEIC<-countEIC[, c("NAME", "GENDER", "JOURNAL", "YEAR")]
#EICs can be EIC for >1 year, so we remove the duplicate names to make sure we count each EIC only once
deduped.countEIC <- unique( countEIC[ , 1:3 ] )
#2x check you now have a list with each EIC listed only once
deduped.countEIC
#count them up
#of Male EICs in our sample
maleEIC<-sum(deduped.countEIC$GENDER=="M")
#of Female EICs in our sample
femaleEIC<-sum(deduped.countEIC$GENDER=="F")
# Percent of EICs in our sample that are male
percentEICmale<-(maleEIC/(maleEIC+femaleEIC))*100
# Percent of EICs in our sample that are female
percentEICfemale<-100-percentEICmale
#NEED to subsetting the data to do some counts of the numbers of AE
countAE<-ALLJOURNALS[ALLJOURNALS$CATEGORY=="AE",]
#name the columns
countAE<-countAE[, c("NAME", "GENDER", "JOURNAL", "YEAR")]
#AEs can be AE for >1 year, so we remove the duplicate names to make sure we count each AE only once
deduped.countAE <- unique( countAE[ , 1:3 ] )
#2x check you now have a list with each AE listed only once
deduped.countAE
#count them up
#of Male AEs in our sample
maleAE<-sum(deduped.countAE$GENDER=="M")
#of Female AEs in our sample
femaleAE<-sum(deduped.countAE$GENDER=="F")
# Percent of AEs in our sample that are male
percentAEmale<-(maleAE/(maleAE+femaleAE))*100
# Percent of AEs in our sample that are female
percentAEfemale<-100-percentAEmale
######NEED to subsetting the data to do some counts of the numbers of SE
countSE<-ALLJOURNALS[ALLJOURNALS$CATEGORY=="SE",]
#name the columns
countSE<-countSE[, c("NAME", "GENDER", "JOURNAL", "YEAR")]
#SEs can be SEs for >1 year, so we remove the duplicate names to make sure we count each SE only once
deduped.countSE <- unique( countSE[ , 1:3 ] )
#2x check you now have a list with each SE listed only once
deduped.countSE
#count them up
#of Male SEs in our sample
maleSE<-sum(deduped.countSE$GENDER=="M")
#of Female SEs in our sample
femaleSE<-sum(deduped.countSE$GENDER=="F")
# Percent of SEs in our sample that are male
percentSEmale<-(maleSE/(maleSE+femaleSE))*100
# Percent of SEs in our sample that are male
percentSEfemale<-100-percentSEmale
#Figure 1: Pie charts of results
#Need data in lomg form
par(mfrow=c(3,1), mai=c(.4, .8, .3, .8))
SE_summary<-table(deduped.countSE$GENDER)
pctSE <- round(SE_summary/sum(SE_summary)*100)
lblsSE <- paste(names(SE_summary), " (" , pctSE, "%, N=", SE_summary, ")" , sep="")
PIE_SE<-pie(SE_summary,labels = lblsSE, col=c("midnightblue", "lightsteelblue2"),
            main="(A) Subject Editors")
AE_summary<-table(deduped.countAE$GENDER)
pctAE <- round(AE_summary/sum(AE_summary)*100)
lblsAE <- paste(names(AE_summary), " (" , pctAE, "%, N=", AE_summary, ")" , sep="")
PIE_AE<-pie(AE_summary, labels=lblsAE, main="(B) Associate Editors",
            col=c("midnightblue", "lightsteelblue2"))
EIC_summary<-table(deduped.countEIC$GENDER)
pctEIC <- round(EIC_summary/sum(EIC_summary)*100)
lblsEIC <- paste(names(EIC_summary), " (" , pctEIC, "%, N=", EIC_summary, ")" , sep="")
PIE_EIC<-pie(EIC_summary, labels=lblsEIC, main="(C) Editors-in-Chief",
             col=c("midnightblue", "lightsteelblue2"))
#######################
#Men and Women (# and oproportion) in category of Editor, this time seperated by journal
#Results for the Number are shown in Figure 3, but I include here the code to plot proportions
#######################
#need to put data in long form
EIC_journals<-table(deduped.countEIC$GENDER, deduped.countEIC$JOURNAL)
AE_journals<-table(deduped.countAE$GENDER, deduped.countAE$JOURNAL)
SE_journals<-table(deduped.countSE$GENDER, deduped.countSE$JOURNAL)
#name thw rows
rownames(EIC_journals) <- c("Female","Male")
rownames(AE_journals)<- c("Female","Male")
rownames(SE_journals)<- c("Female","Male")
###If you want bar charts of counts, use this (Fig 3)
par(mfrow=c(3,1),mar=c(2.5,4.1,3.6,8), xpd=TRUE)
barplot(EIC_journals, main="(A) Editors-in-Chief",
        xlab="Journal", ylab="N", col=c("midnightblue", "lightsteelblue2"),
        names.arg=c("Biotropica","AREES","Agronomy","NAJFM","Am. J. Botany","Cons. Biol.","Ecology","Biol. Cons.","J. Ecology","J Trop. Ecology"), font=3)
barplot(AE_journals, main="(B) Associate Editors",
        xlab="Journal", ylab="N",col=c("midnightblue", "lightsteelblue2"),
        names.arg=c("Biotropica","AREES","Agronomy","NAJFM","Am. J. Botany","Cons. Biol.","Ecology","Biol. Cons.","J. Ecology","J Trop. Ecology"), font=3)
legend(y=40,x=12.1, legend = c("Female", "Male"), fill = c("midnightblue", "lightsteelblue2"))
barplot(SE_journals, main="(C) Subject Editors",
        xlab="Journal", ylab="N",col=c("midnightblue", "lightsteelblue2"),
        names.arg=c("Biotropica","AREES","Agronomy","NAJFM","Am. J. Botany","Cons. Biol.","Ecology","Biol. Cons.","J. Ecology","J Trop. Ecology"), font=3)
###If you want a bar chart of proportions, use this
###here you are asking sweep to divide each element of table by the column sum
EIC_perc=sweep(EIC_journals,2,colSums(EIC_journals),`/`)
AE_perc=sweep(AE_journals,2,colSums(AE_journals),`/`)
SE_perc=sweep(SE_journals,2,colSums(SE_journals),`/`)
###Plots
###thr mfrow lines them up top to bottom, mar sets the bottom, left, top, and right margins so that you can put the legend in without overlapping the bars
### default margins in R = 5.1,4.1,4.1,2.1
###XPD=TRUE allows you to plot in the area ourside the plot margins
par(mfrow=c(3,1),mar=c(2.5,4.1,3.6,8), xpd=TRUE)
barplot(EIC_perc, main="Editors-in-Chief",
        xlab="Journal", ylab="proportion", col=c("midnightblue", "lightsteelblue2"),
        names.arg=c("Biotropica","AREES","Agronomy","NAJFM","Am. J. Botany","Cons. Biol.","Ecology","Biol. Cons.","J. Ecology","J Trop. Ecology"), font=3)
barplot(AE_perc, main="Associate Editors",
        xlab="Journal", ylab="proportion", col=c("midnightblue", "lightsteelblue2"),
        names.arg=c("Biotropica","AREES","Agronomy","NAJFM","Am. J. Botany","Cons. Biol.","Ecology","Biol. Cons.","J. Ecology","J Trop. Ecology"), font=3)
legend(y=0.7,x=12.1, legend = c("Female", "Male"), fill = c("midnightblue", "lightsteelblue2"))
barplot(SE_perc, main="Subject Editors",
        xlab="Journal", ylab="proportion", col=c("midnightblue", "lightsteelblue2"),
        names.arg=c("Biotropica","AREES","Agronomy","NAJFM","Am. J. Botany","Cons. Biol.","Ecology","Biol. Cons.","J. Ecology","J Trop. Ecology"), font=3)
########################################################
##Changes in proportion of women on editorial Editorial boards over time.
#NOTE EDITORIAL BOARD = EIC'S + AES + SE
########################################################
#create a summary table of gender by year and journal
GENDER<-ftable(ALLJOURNALS$JOURNAL, ALLJOURNALS$YEAR,ALLJOURNALS$GENDER)
GENDER
#Convert to a dataframe
GENDER<-as.data.frame(GENDER)
summary(GENDER)
colnames(GENDER) <- c("journal", "year", "gender", "count")
#covert from long to wide form
DATA<-reshape(GENDER, idvar = c("year", "journal"), timevar = "gender", direction = "wide")
#rename the columns
colnames(DATA) <- c("journal", "year", "Female", "Male")
DATA
DATA$year<-as.numeric(levels(DATA$year))[DATA$year]
#Calclulate and add the %female to dataframe
DATA[, "percent_female"] <- (DATA[, "Female"]/(DATA[, "Female"] +DATA[, "Male"]))*100
#COnvert year to number from category
DATA<-drop.levels(DATA)
#SELECT ONLY FROM 1987 ON TO BE CONSISTENT
summary(DATA)
#plot percent_female by year, seperated by journal (Fig. 2)
par(mfrow=c(3,4), mar=c(5,2.5,3,2), xpd=FALSE)
plot(lwd=2, DATA[DATA$journal=="AGRONOMY",]$year, DATA[DATA$journal=="AGRONOMY",]$percent_female, type="l", main="Agronomy", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="NAJFM",]$year, DATA[DATA$journal=="NAJFM",]$percent_female, type="l", main="N. Am. J. Fisheries. Manag.", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="BITR",]$year, DATA[DATA$journal=="BITR",]$percent_female, type="l", main="Biotropica", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="AREES",]$year, DATA[DATA$journal=="AREES",]$percent_female, type="l", main="Ann. Rev. Ecol., Evol., & Syst.", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="AJB",]$year, DATA[DATA$journal=="AJB",]$percent_female, type="l", main="Am J. Bot", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="CONBIO",]$year, DATA[DATA$journal=="CONBIO",]$percent_female, type="l", main="Cons. Biol.", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="BIOCON",]$year, DATA[DATA$journal=="BIOCON",]$percent_female, type="l", main="Biol. Cons.", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="ECOLOGY",]$year, DATA[DATA$journal=="ECOLOGY",]$percent_female, type="l", main="Ecology", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="JECOL",]$year, DATA[DATA$journal=="JECOL",]$percent_female, type="l", main="J. Ecology", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)
plot(lwd=2, DATA[DATA$journal=="JTE",]$year, DATA[DATA$journal=="JTE",]$percent_female, type="l", main="J. Trop. Ecology", ylim=c(0,100), xlab="year", ylab="% of ed board female", col="midnightblue", bty="n", font.main=4)
abline(h=50,col="midnightblue",lty=2)

