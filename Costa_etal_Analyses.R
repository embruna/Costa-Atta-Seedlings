
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN: Costa el al MS


# library(gdata)
# library(lme4)
# library(ggplot2)
# library(reshape)
# library(maps)
# library(WDI)
# library(RColorBrewer)
# library(lattice)
# library(coefplot2) ## for coefplot2
# library(reshape)
# library(gridExtra)
# library(emdbook)

library(dplyr)
library(tidyr)
#CLear out everything from the environment
rm(list=ls())


##################
#################
###DATA ENTRY AND CLEANUP
##################
#################
#Step 1: load the individual CSV files and save them as dataframes
setwd("/Users/emiliobruna/Dropbox/Alan/Data/Capitulo2")
NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
VEG<-read.csv("ActiveNests_CensoVeg_1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
AIRTEMPHUMID<-read.csv("ActiveNests_TempAirHumid_7.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )


summary(NEST.DATA)
summary(VEG)
summary(AIRTEMPHUMID)

str(NEST.DATA)
str(VEG)
str(AIRTEMPHUMID)

# Use Cerrado Ralo (CR) and Cerrado Denso (CD) in analyse
NEST.DATA_both<-NEST.DATA[NEST.DATA$habitat=="CR"|NEST.DATA$habitat=="CD",] #both habitats
NEST.DATA_both <- droplevels(NEST.DATA_both)

NEST.DATA_CR<-NEST.DATA[NEST.DATA$habitat=="CR",] #only CR
NEST.DATA_CR <- droplevels(NEST.DATA_CR)

NEST.DATA_CD<-NEST.DATA[NEST.DATA$habitat=="CD",] #only CD
NEST.DATA_CD <- droplevels(NEST.DATA_CD)

# histogram(NEST.DATA_CD$grass.bmass)





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




