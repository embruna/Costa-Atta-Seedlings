#####Take the data on seedling counts and richness, PCA scores for plots, and join together,
# Used http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/ as a guide
library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(reshape2)
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
#make plot locations an ordered factor nest<adjacent<far
NEST.DATA$location=factor(NEST.DATA$location, levels=c("nest","adjacent", "far"), ordered=TRUE)

head(NEST.DATA, 3)
str(NEST.DATA)

# #Select only Cerrado Denso and Cerrado Ralo
NEST.DATA.PCA<-filter(NEST.DATA, habitat =="CD" | habitat == "CR")
#Delete the ones with missing values. 
#Note there are techniques to impute missing values can look into 
NEST.DATA.PCA<-na.omit(NEST.DATA.PCA) 
# log transform 
# env.vars <- log(NEST.DATA.PCA[, 5:18]+1)
# no transformation as per HLV
env.vars <- NEST.DATA.PCA[,5:18]
site.cats <- NEST.DATA.PCA[, 1:5]
# Exclude the following: grass biomass, canopy.cover, deep soil humidity 
#env.vars$grass.bmass<-NULL
env.vars$humid.soil.deep<-NULL
#env.vars$peak.soil.temp<-NULL
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
nest.env.pca <- prcomp(env.vars,
                       center = TRUE,
                       scale. = TRUE) 
#Visualizing the results.
# The print method returns the standard deviation of each of the four PCs, 
# and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables.
print(nest.env.pca)
# The Figure below is useful to decide how many PCs to retain for further analysis. 
# I.E., which PCs explain most of the variability in the data.
plot(nest.env.pca, type = "l")
# The summary method describe the importance of the PCs. 
# The first row describe again the standard deviation associated with each PC. 
# The second row shows the proportion of the variance in the data explained by each component.
# The third row describe the cumulative proportion of explained variance. 
# summary method
summary(nest.env.pca)
#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scores<-(nest.env.pca$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.scores<-as.data.frame(pca.plot.scores[,1:2]) #this saves the 1st column - PCA Axis 1 - as a dataframe
dim(PCA.scores)



VEG<-read.csv("ActiveNests_CensoVeg_1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
summary(VEG)
VEG$location=factor(VEG$location, levels=c("nest","adjacent", "far"), ordered=TRUE)

VEG_both<-VEG[VEG$habitat=="CR"|VEG$habitat=="CD",] #both habitats
VEG_both <- droplevels(VEG_both)
###########################################################
####### COUNT OF SEEDLINGS AND SPP RICH IN PLOTS ##########
###########################################################

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
# Recall for we only have soils etc for plots on nests and far, so need to select the spdlg counts and spp no. for ONLY those plots
sdlgs.NF<-filter(sdlgs.NAF, location =="nest" | location == "far")
sdlgs.NF<-droplevels(sdlgs.NF)

# dim(sdlgs.NF)
# dim(NEST.DATA.PCA)
# summary(count.df)
# summary(spp.df)

#Select the nest, perc.cover, and location from NEST.DATA.PCA
cover.NF <- NEST.DATA.PCA[, 2:5]
cover.NF<-filter(cover.NF, location =="nest" | location == "far")
cover.NF<-droplevels(cover.NF)
cover.NF$plot.id<-NULL
cover.NF$nest<-as.factor(cover.NF$nest)

str(cover.NF)
str(sdlgs.NF)


#you can't do the Inner_join by a factor, so convert nest to chr
sdlgs.NF$nest<-as.character(sdlgs.NF$nest)
cover.NF$nest<-as.character(cover.NF$nest)
PCA.SDLGS<-inner_join(sdlgs.NF,cover.NF, by = c("nest"="nest","location"="location"))  #note syntax c(X=Y, B=Z) for joining by two columns, otherwise get weird output
str(cover.NF)
str(sdlgs.NF)
str(PCA.SDLGS)
#now convert it back to do the analyses
PCA.SDLGS$nest<-as.factor(PCA.SDLGS$nest)


#FINALLLY, you can bind the PCA scores to the seedling number and richness data + cover
PCA.SDLGS<-cbind(PCA.SDLGS,PCA.scores)




#GLMS
glm.sdlg0 = glm(sdlg.no ~ PC1 ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg0)


glm.sdlg1 = glm(sdlg.no ~ nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg1)

glm.sdlg2 = glm(sdlg.no ~ location ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg2)

glm.sdlg3 = glm(sdlg.no ~ PC1+nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg3)

glm.sdlg4 = glm(sdlg.no ~ PC1+location ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg4)

glm.sdlg4.5 = glm(sdlg.no ~ nest+location ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg4.5)

glm.sdlg5 = glm(sdlg.no ~ PC1+location+nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg5)


# glm.sdlg3 = glm(sdlg.no ~ perc.cover+nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(glm.sdlg3)

# glm.sdlg4 = glm(sdlg.no ~ PC1*perc.cover+nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(glm.sdlg4)

AIC(glm.sdlg0,glm.sdlg1,glm.sdlg2,glm.sdlg3,glm.sdlg4,glm.sdlg4.5,glm.sdlg5)
anova(glm.sdlg0,glm.sdlg1,glm.sdlg2,glm.sdlg3,glm.sdlg4,glm.sdlg4.5,glm.sdlg5,test="Chisq")

#ANOva of best fit model (not correct but useful at this stage)
# bestfit_sdlgno =aov(spp.no ~ nest+location ,data=PCA.SDLGS) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(bestfit_sdlgno)


# SPP COUNTS GLMS

#GLMS

glm.spp0 = glm(spp.no ~ PC1 ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp0)

glm.spp1 = glm(spp.no ~ nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp1)

glm.spp2 = glm(spp.no ~ location ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp2)

glm.spp3 = glm(spp.no ~ PC1+nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp3)

glm.spp4 = glm(spp.no ~ PC1+location ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp4)

glm.spp4.5 = glm(spp.no ~ nest+location ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp4.5)

glm.spp5 = glm(spp.no ~ PC1+location+nest ,data=PCA.SDLGS,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.spp5)




AIC(glm.spp0,glm.spp1,glm.spp2,glm.spp3,glm.spp4,glm.spp4.5,glm.spp5)
anova(glm.spp0, glm.spp1,glm.spp2,glm.spp3,glm.spp4,glm.spp4.5,glm.spp5,test="Chisq")

# #ANOva of best fit model (not correct but useful at this stage)
# bestfit_sppno =aov(spp.no ~ location ,data=PCA.SDLGS) #Recall * is syntax syntax shortcue of both main effects + interaction
# # summary(bestfit_sppno)









PCA.sdlgs.fig<-ggplot(PCA.SDLGS, aes(x = PC1, y = sdlg.no, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("seedling abundance") +
  xlab("PCA1")+
  ggtitle("A")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines
PCA.sdlgs.fig<-PCA.sdlgs.fig + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
#plot.sdlg.no<-plot.sdlg.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
PCA.sdlgs.fig<-PCA.sdlgs.fig + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4))
PCA.sdlgs.fig<- PCA.sdlgs.fig + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        #legend.position = 'none',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        legend.position = c(0.7,0.7),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
PCA.sdlgs.fig



PCA.spp.fig<-ggplot(PCA.SDLGS, aes(x = PC1, y = spp.no, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("species richness") +
  xlab("PCA1")+
  ggtitle("B")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines
PCA.spp.fig<-PCA.spp.fig + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
#plot.sdlg.no<-plot.sdlg.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
PCA.spp.fig<-PCA.spp.fig + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4))
PCA.spp.fig<- PCA.spp.fig + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        #legend.position = 'none',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        legend.position = c(0.7,0.7),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
PCA.spp.fig

