
###########################################################
####### COUNT OF SEEDLINGS AND SPP RICH IN PLOTS ##########
###########################################################
library(ggplot2)
library(dplyr)
library(tidyr)

#CLear out everything from the environment
rm(list=ls())

setwd("/Users/emiliobruna/Dropbox/Alan/Data/Capitulo2")

NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
VEG<-read.csv("ActiveNests_CensoVeg_1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )


summary(VEG)
VEG$location=factor(VEG$location, levels=c("nest","adjacent", "far"), ordered=TRUE)

VEG_both<-VEG[VEG$habitat=="CR"|VEG$habitat=="CD",] #both habitats
VEG_both <- droplevels(VEG_both)
VEG_both$ht_cm<-as.numeric(VEG_both$ht_cm)
VEG_both$nest<-as.factor(VEG_both$nest)
summary(VEG_both)
str(VEG_both)


# Hacking my way around to count how many seedlings in each plot
# CORRECT WAY: 
# A<-na.omit(VEG_both)
# B<-select(A, nest, location, species)
# count.df<-A %>%
#   group_by(nest, location) %>%
#   summarise(count = n())
# count.df
# but this leaves out the ones thathad 0, so you cant bind it easily to the vector of % cover.  
# What you need is plyr's .drop=FALSE, but it doesn't exist in dplyr, so the following yould be how you work around.
# dplyr solution to having .drop=FALSE to count plots with NA as having 0 plants in them
# http://stackoverflow.com/questions/22523131/dplyr-summarise-equivalent-of-drop-false-to-keep-groups-with-zero-length-in
# As an alternative, I will include plot.id, group, and count. This gives you the number of plots in your dataframe (with each plant having 
# its associated plot in the long form table.  Yoiu then convert all 1 to 0 -> plots with no plants were counted as a single row
# with an NA in it.  Ugly, but works
VEG_both_hack<-select(VEG_both, nest, location, species, plot.id)
count.df<-VEG_both_hack %>%
  group_by(nest, location) %>%
  summarise(count = n())
names(count.df)[3]<-"sdlg.no"
count.df$sdlg.no[count.df$sdlg.no==1]<-0
count.df

#to see hpw many species each plot had, first reshape so that the data are in wide fomr
spp.df.long<-select(VEG_both, nest, location, species, plot.id) #is there a dplyr equivalent?????
spp.df.wide<-dcast(spp.df.long, nest+location ~ species)
spp.df.wide<-spp.df.wide[,c(3:197)]
spp.df<-as.data.frame(rowSums(spp.df.wide != 0))
names(spp.df)[1]<-"spp.no" #rename the column
spp.df$spp.no[spp.df$spp.no==1]<-0
#checking to make sure number of rows is the same to cbind
dim(count.df)
dim(spp.df)
#bind
sdlgs.NAF<-cbind(count.df, spp.df) #Nest Adjacent Far
sdlgs.NAF<-droplevels(sdlgs.NAF)

#sdlgs.NAF<-filter(sdlgs.NAF, location =="nest" | location == "far")

str(sdlgs.NAF)
#sdlgs.NAF<-filter(sdlgs.NAF, location =="nest" | location == "far")
str(NEST.DATA)
NEST.DATA.CDCR<-filter(NEST.DATA, habitat =="CD" | habitat == "CR")
NEST.DATA.CDCR<-droplevels(NEST.DATA.CDCR)
str(NEST.DATA.CDCR)

head(NEST.DATA.CDCR, 10)
head(sdlgs.NAF,10)
NEST.DATA.CDCR<-na.omit(NEST.DATA.CDCR)

ALLDATA.CDCR<-cbind(sdlgs.NAF,NEST.DATA.CDCR)
ALLDATA.CDCR[6]<-NULL
ALLDATA.CDCR[2]<-NULL
str(ALLDATA.CDCR)
# 
# #Select the nest, perc.cover, and location from NEST.DATA.PCA
# # cover.NF <- NEST.DATA.PCA[, 2:5]
# # cover.NF<-filter(cover.NF, location =="nest" | location == "far")
# # cover.NF<-droplevels(cover.NF)
# cover.NF$plot.id<-NULL
# cover.NF$nest<-as.factor(cover.NF$nest)
# 
# str(cover.NF)
# str(sdlgs.NF)
# 
# 
# #you can't do the Inner_join by a factor, so convert nest to chr
# sdlgs.NF$nest<-as.character(sdlgs.NF$nest)
# cover.NF$nest<-as.character(cover.NF$nest)
# PCA.SDLGS<-inner_join(sdlgs.NF,cover.NF, by = c("nest"="nest","location"="location"))  #note syntax c(X=Y, B=Z) for joining by two columns, otherwise get weird output
# str(cover.NF)
# str(sdlgs.NF)
# str(PCA.SDLGS)
# #now convert it back to do the analyses
# PCA.SDLGS$nest<-as.factor(PCA.SDLGS$nest)
# 
# 
# #FINALLLY, you can bind the PCA scores to the seedling number and richness data + cover
# PCA.SDLGS<-cbind(PCA.SDLGS,PCA.scores)


############
# ##############NEED TO FIX, MODELS BELOW ARE COPIED BUT MAY NOT BE RIGHT
# ANALYSES OF ALL PLOTS, USING JUST % CANOPY COVER intstead of PCA1
# ############
############
############


#GLMS


glmperc.sdlg0 = glm(sdlg.no ~ 1 ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg0)

glmperc.sdlg1 = glm(sdlg.no ~ perc.cover ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg1)

anova(glmperc.sdlg0,glmperc.sdlg1,test="Chisq")

glmperc.sdlg2 = glm(sdlg.no ~ nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg2)

anova(glmperc.sdlg1,glmperc.sdlg2,test="Chisq")

glmperc.sdlg3 = glm(sdlg.no ~ location ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg3)

anova(glmperc.sdlg2,glmperc.sdlg3,test="Chisq")

glmperc.sdlg4 = glm(sdlg.no ~ perc.cover+nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg4)

glmperc.sdlg5 = glm(sdlg.no ~ perc.cover+location ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg5)

glmperc.sdlg6 = glm(sdlg.no ~ nest+location ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg6)

glmperc.sdlg7 = glm(sdlg.no ~ perc.cover+location+nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.sdlg7)


# glmperc.sdlg3 = glm(sdlg.no ~ perc.cover+nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(glmperc.sdlg3)

# glmperc.sdlg4 = glm(sdlg.no ~ perc.cover*perc.cover+nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(glmperc.sdlg4)

AIC(glmperc.sdlg0,glmperc.sdlg1,glmperc.sdlg2,glmperc.sdlg3,glmperc.sdlg4,glmperc.sdlg4.5,glmperc.sdlg5)
anova(glmperc.sdlg0,glmperc.sdlg1,glmperc.sdlg2,glmperc.sdlg3,glmperc.sdlg4,glmperc.sdlg4.5,glmperc.sdlg5,test="Chisq")

#ANOva of best fit model (not correct but useful at this stage)
# bestfit_sdlgno =aov(spp.no ~ nest+location ,data=ALLDATA.CDCR) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(bestfit_sdlgno)


# SPP COUNTS GLMS

#GLMS

glmperc.spp0 = glm(spp.no ~ perc.cover ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp0)

glmperc.spp1 = glm(spp.no ~ nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp1)

glmperc.spp2 = glm(spp.no ~ location ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp2)

glmperc.spp3 = glm(spp.no ~ perc.cover+nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp3)

glmperc.spp4 = glm(spp.no ~ perc.cover+location ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp4)

glmperc.spp4.5 = glm(spp.no ~ nest+location ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp4.5)

glmperc.spp5 = glm(spp.no ~ perc.cover+location+nest ,data=ALLDATA.CDCR,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glmperc.spp5)




AIC(glmperc.spp0,glmperc.spp1,glmperc.spp2,glmperc.spp3,glmperc.spp4,glmperc.spp4.5,glmperc.spp5)
anova(glmperc.spp0, glmperc.spp1,glmperc.spp2,glmperc.spp3,glmperc.spp4,glmperc.spp4.5,glmperc.spp5,test="Chisq")

# #ANOva of best fit model (not correct but useful at this stage)
# bestfit_sppno =aov(spp.no ~ location ,data=ALLDATA.CDCR) #Recall * is syntax syntax shortcue of both main effects + interaction
# # summary(bestfit_sppno)

