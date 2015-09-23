# #PCA of environmental data in R
# Used http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/ as a guide

library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
library(dplyr)
library(tidyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(reshape2)


#CLear out everything from the environment
rm(list=ls())

##################
#################
###DATA ENTRY AND CLEANUP
##################
#################
#Step 1: load the individual CSV files and save them as dataframes
setwd("/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Data/Capitulo2")
NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#make plot locations an ordered factor nest<adjacent<far
NEST.DATA$location=factor(NEST.DATA$location, levels=c("nest","adjacent", "far"), ordered=TRUE)
# Make Nest ID a factor
NEST.DATA$nest<-as.factor(NEST.DATA$nest)

head(NEST.DATA, 3)
str(NEST.DATA)

# #Select only Cerrado Denso and Cerrado Ralo
NEST.DATA.both<-filter(NEST.DATA, habitat =="CD" | habitat == "CR")
NEST.DATA.both<-droplevels(NEST.DATA.both)
str(NEST.DATA.both)

# #Select only Cerrado Denso 
# NEST.DATA.CD<-filter(NEST.DATA, habitat =="CD")
# #Select only Cerrado Ralo
# NEST.DATA.CR<-filter(NEST.DATA, habitat == "CR")
# 
# Which of the habitats are you going to analyze? CD, CR, or both?
NEST.DATA.PCA.ALL<-NEST.DATA.both #NEST.DATA.CD NEST.DATA.both NEST.DATA.CR 
# 
# Correlations between variales
correlations<-cor(NEST.DATA.PCA.ALL[,5:15])

lb<-NEST.DATA.PCA.ALL$litter.bmass
sp<-NEST.DATA.PCA.ALL$soil.pen
gb<-NEST.DATA.PCA.ALL$grass.bmass
sh<-NEST.DATA.PCA.ALL$soil.humid.surface
pc<-NEST.DATA.PCA.ALL$perc.cover

# hist(lb)
# hist(sp)
# hist(gb)
# hist(sh)
# hist(pc)


####CORRELATIONS BTWEN ENVT VARIABLES (NON SOIL) 
cor.test(lb, sp, method="spearman")
cor.test(lb,gb, method="spearman")
cor.test(lb,sh, method="spearman")
cor.test(gb,sh, method="spearman")
# NOT STRONGLY CORRELATED WITH EACH OTHER, BUT SEE BELOW - MOST CORRELATED WITH PC


####CORRELATIONS btwn ENVT VARIABLES (NON SOIL) and PERCENT COVER
cor.test(pc,lb, method="spearman")
cor.test(pc,sp)
cor.test(pc,gb, method="spearman")
cor.test(pc,sh)
# Percent cover strongly correlateds with 3 of these 4

####CORRELATIONS BTWEN SOIL VARIABLES
correlationSOIL<-cor(NEST.DATA.PCA.ALL[,9:15])
ph<-NEST.DATA.PCA.ALL$ph
P<-NEST.DATA.PCA.ALL$P
K<-NEST.DATA.PCA.ALL$K
Ca<-NEST.DATA.PCA.ALL$Ca
Mg<-NEST.DATA.PCA.ALL$Mg
Al<-NEST.DATA.PCA.ALL$Al
OM<-NEST.DATA.PCA.ALL$org.mat

# hist(ph)
# hist(P)
# hist(K)
# hist(Ca)
# hist(Mg)
# hist(Al)
# hist(OM)

cor.test(ph,P)
cor.test(ph,K)
cor.test(ph,Ca)
cor.test(ph,Mg)
cor.test(ph,Al)
cor.test(ph,OM)
cor.test(ph,P)

cor.test(P,K)
cor.test(K,Ca)
cor.test(P,Mg)
cor.test(P,Al)
cor.test(P,OM)

cor.test(K,Ca)
cor.test(K,Mg)
cor.test(K,Al)
cor.test(K,OM)

cor.test(Ca,Mg)
cor.test(Ca,Al)
cor.test(Ca,OM)

cor.test(Mg,Al)
cor.test(Mg,OM)

cor.test(Al,OM)

# Is canopy corfrelated with these?
cor.test(pc,ph)
cor.test(pc,P)
cor.test(pc,K)
cor.test(pc,Ca)
cor.test(pc,Mg)
cor.test(pc,Al)
cor.test(pc,OM)
cor.test(pc,P)
# Not infdividually

# 
# ####THE RESULTS ABOVE SUGGEST ALL THE CORRELATIONS BETWEEN ENVT'L VARIABLES AND LIGHT WITH THE BIOLOGICAL ONES.  DO TWO PCAS
# 1) ONE OF ALL VARIABLES - SOIL, OVER, BIOMASS+: THIS WILL HAVE A SMALLER NUMBER OF NESTS BECAUSE SOILS DATA FOR FEWER
# 2) ONE OF COVER AND BMASS+ BUT NO SOILS (LARGER NUMBER OF NESTS)

#This is missing in a bunch of them, so will reduce sample size too much
NEST.DATA.PCA.ALL$peak.soil.temp<-NULL
NEST.DATA.PCA.ALL$time.peak.soil.temp<-NULL
NEST.DATA.PCA.ALL$date.peak.soil.tem<-NULL
NEST.DATA.PCA.ALL$humid.soil.deep<-NULL

NEST.DATA.PCA.NOSOILS<-NEST.DATA.PCA.ALL
# # WILL THIS INCLUDE THE SOILS DATA?  TOGGLE ON OFF HERE
NEST.DATA.PCA.NOSOILS$ph<-NULL
NEST.DATA.PCA.NOSOILS$P<-NULL
NEST.DATA.PCA.NOSOILS$K<-NULL
NEST.DATA.PCA.NOSOILS$Ca<-NULL
NEST.DATA.PCA.NOSOILS$Mg<-NULL
NEST.DATA.PCA.NOSOILS$Al<-NULL
NEST.DATA.PCA.NOSOILS$org.mat<-NULL    


# Will this inclide % COver? Toggle on off here ACTUALLY< A BETTER WAY IS TO INCLUDE IN SITE CAT vs ENVT VAR IN PCA
#NEST.DATA.PCA$perc.cover<-NULL

#Clear all those with missing values to do PCA
NEST.DATA.PCA.ALL<-na.omit(NEST.DATA.PCA.ALL) 
NEST.DATA.PCA.NOSOILS<-na.omit(NEST.DATA.PCA.NOSOILS) 

droplevels(NEST.DATA.PCA.ALL)
droplevels(NEST.DATA.PCA.NOSOILS)

str(NEST.DATA.PCA.ALL)
summary(NEST.DATA.PCA.ALL)
str(NEST.DATA.PCA.NOSOILS)
summary(NEST.DATA.PCA.NOSOILS)

# log transform 
# env.vars <- log(NEST.DATA.PCA[, 5:18]+1)
# no transformation as per HLV

# PAY ATTENTION - FIX THIS DEPENDING ON IF PCA ALL OR PCA "NO SOILS"

# #FOR PCA ALL (WITH SOILS)
env.vars.all <- NEST.DATA.PCA.ALL[,5:16]
site.cats.all <- NEST.DATA.PCA.ALL[, 1:4]

# #FOR PCA ALL (NO SOILS)
env.vars.nosoil <- NEST.DATA.PCA.NOSOILS[,5:9]
site.cats.nosoil <- NEST.DATA.PCA.NOSOILS[, 1:4]


#################################
### PCA ALL DATA - WITH SOILS ##
#################################
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
nest.env.pca.all <- prcomp(env.vars.all,
                       center = TRUE,
                       scale. = TRUE) 

#Visualizing the results.
# The print method returns the standard deviation of each of the four PCs, 
# and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables.
print(nest.env.pca.all)


# The Figure below is useful to decide how many PCs to retain for further analysis. 
# I.E., which PCs explain most of the variability in the data.
plot(nest.env.pca.all, type = "l")

# The summary method describe the importance of the PCs. 
# The first row describe again the standard deviation associated with each PC. 
# The second row shows the proportion of the variance in the data explained by each component.
# The third row describe the cumulative proportion of explained variance. 
# summary method
summary(nest.env.pca.all)

#################################
### PCA ALL DATA - NO SOILS  ####
#################################

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
nest.env.pca.nosoil <- prcomp(env.vars.nosoil,
                           center = TRUE,
                           scale. = TRUE) 

#Visualizing the results.
# The print method returns the standard deviation of each of the four PCs, 
# and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables.
print(nest.env.pca.nosoil)


# The Figure below is useful to decide how many PCs to retain for further analysis. 
# I.E., which PCs explain most of the variability in the data.
plot(nest.env.pca.nosoil, type = "l")

# The summary method describe the importance of the PCs. 
# The first row describe again the standard deviation associated with each PC. 
# The second row shows the proportion of the variance in the data explained by each component.
# The third row describe the cumulative proportion of explained variance. 
# summary method
summary(nest.env.pca.nosoil)


#################################
###   PCA FIG - WITH SOILS    ##
#################################

# The Figure below is a biplot generated by the function ggbiplot of the ggbiplot package available on https://github.com/vqv/ggbiplot
# TO install: 
# library(devtools)
# install_github("ggbiplot", "vqv")
# library(ggbiplot)
# ?ggbiplot for more info on arguments you can change and % used to draw ellipses aroudnd points
location.all<-site.cats.all$location
cover.all<-env.vars.all$perc.cover
nest.all<-site.cats.all$nest

# FIGURE - PCA WITH SOILS (REDUCED NUMBER OF PLOTS)
point.size<-cover.all*0.1
g_soils <- ggbiplot(nest.env.pca.all, obs.scale = 1, var.scale = 1, 
                    group = location.all, ellipse = TRUE, 
                    circle = TRUE, varname.size=3)+
  scale_colour_manual(values=c("darkred", "darkblue")) +
  #geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06
  geom_point(aes(color=location.all, size = point.size)) + scale_size_identity()
#I chose my own colors for the lines
g_soils<-g_soils + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-5, 4)) # I adjusted X axis so that I could read the larger labels on arrows
g_soils<g_soils + scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4,4)) # I adjusted Y axis so that I could read the larger labels on arrows


g_soils <-g_soils + theme_classic()+theme(legend.direction = 'horizontal', 
                                          legend.position = 'top',
                                          axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
                                          legend.title = element_blank(), #remove title of legend
                                          legend.text = element_text(color="black", size=22, vjust =2),
                                          plot.margin =unit(c(1,1,1,1.5), "lines")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

print(g_soils)

#################################
###   PCA FIG - NO SOILS    ##
#################################

location.nosoil<-site.cats.nosoil$location
cover.nosoil<-env.vars.nosoil$perc.cover
nest.nosoil<-site.cats.nosoil$nest

# FIGURE - PCA WITH SOILS (REDUCED NUMBER OF PLOTS)
point.size<-cover.nosoil*0.1
g_NOsoils <- ggbiplot(nest.env.pca.nosoil, obs.scale = 1, var.scale = 1, 
                    group = location.nosoil, ellipse = TRUE, 
                    circle = TRUE, varname.size=3)+
  scale_colour_manual(values=c("darkred","orchid", "darkblue")) +
  #geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06
  geom_point(aes(color=location.nosoil, size = point.size)) + scale_size_identity()
#I chose my own colors for the lines
g_NOsoils<-g_NOsoils + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-5, 4)) # I adjusted X axis so that I could read the larger labels on arrows
g_NOsoils<g_NOsoils + scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4,4)) # I adjusted Y axis so that I could read the larger labels on arrows


g_NOsoils <-g_NOsoils + theme_classic()+theme(legend.direction = 'horizontal', 
                                          legend.position = 'top',
                                          axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
                                          legend.title = element_blank(), #remove title of legend
                                          legend.text = element_text(color="black", size=22, vjust =2),
                                          plot.margin =unit(c(1,1,1,1.5), "lines")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

print(g_NOsoils)


#########################################
## ANALYSES BASED ON PCA - WITH SOILS
#########################################

#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scores.all<-(nest.env.pca.all$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1.all<-as.data.frame(pca.plot.scores.all[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2.all<-pca.plot.scores.all[,2]#this saves the 2nd column - PCA Axis 2 - as a dataframe
GLM.DATA.all<-as.data.frame(cbind(nest.all,location.all, cover.all, PCA.1.all, PCA.2.all))
names(GLM.DATA.all)[4]<-"PCA1.all" #reaname the column
names(GLM.DATA.all)[5]<-"PCA2.all" #rename the column


#########################################
## ANALYSES BASED ON PCA - NO SOILS
#########################################

#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scores.nosoil<-(nest.env.pca.nosoil$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1.nosoil<-as.data.frame(pca.plot.scores.nosoil[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2.nosoil<-pca.plot.scores.nosoil[,2]#this saves the 2nd column - PCA Axis 2 - as a dataframe
GLM.DATA.nosoil<-as.data.frame(cbind(nest.nosoil,location.nosoil, cover.nosoil, PCA.1.nosoil, PCA.2.nosoil))
names(GLM.DATA.nosoil)[4]<-"PCA1.nosoil" #reaname the column
names(GLM.DATA.nosoil)[5]<-"PCA2.nosoil" #rename the column




####################################
###DATA ENTRY AND CLEANUP: SEEDLINGS
####################################

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
#Remove the NA
# VEG_both_summary<-na.omit(VEG_both)
# VEG_both_summary<-as.data.frame(VEG_both_summary)
# summary(VEG_both_summary)
# str(VEG_both_summary)

# can test with this: if it works, and gives you groups, then all is good. if not restart
# iris %>%
#   group_by(Species) %>%
#   summarize(meanSepLength=mean(Sepal.Length)) 

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
spp.df<-rowSums(spp.df.wide != 0)


#checking to make sure number of rows is the same to cbind
dim(count.df)
dim(spp.df.wide)
dim(NEST.DATA.both) 

# add the column "plot.id" from NEST.DATA_both to create new summary df seedlings.  NB: THERE IS HIGH POTENTIAL FOR FUNCKING THINGS 
# UP HERE, SO BE CAREFUL
sdlgs<-cbind(NEST.DATA.both$plot.id, count.df)
names(sdlgs)[1]<-"plot.id" #rename the column

# THIS CREATES A DATAFRAME YOU CAN BIND TO THE % COVER
sdlgs<-cbind(sdlgs, spp.df)
names(sdlgs)[5]<-"spp.no" #rename the column
str(sdlgs)

# BIND THEM UP....add % cover!!!
sdlgs.perc.cover<-select(NEST.DATA.both, plot.id, perc.cover)
dim(sdlgs.perc.cover) #make sure same size as sdlgs dataframe
sdlgs<-left_join(sdlgs, sdlgs.perc.cover, by = "plot.id")
sdlgs<-na.omit(sdlgs)


dim(sdlgs)
dim(GLM.DATA.all)
dim(GLM.DATA.nosoil)

str(sdlgs)
str(GLM.DATA.all)
str(GLM.DATA.nosoil)

# WILL USE THE FOLLOWING TO DO GLMS W/ NO SOILS IN PCA (i.e., larger sample size)
sdlgs.nosoil<-cbind(sdlgs, GLM.DATA.nosoil)
sdlgs.nosoil$nest.nosoil<-NULL
sdlgs.nosoil$cover.nosoil<-NULL
sdlgs.nosoil$location.nosoil<-NULL

str(sdlgs.nosoil)

# WILL USE THE FOLLOWING TO DO GLMS ***WITH*** SOILS IN PCA (i.e., larger sample size)
sdlgs.all<-filter(sdlgs, location =="far" | location == "nest")
# clean up column names and sort to do join of two dataframes of different sizes
sdlgs.all$plot.id<-NULL
sdlgs.all<-left_join(sdlgs.all,GLM.DATA.all, by = c("nest" = "nest.all","location" = "location.all"))
#NOW CHABGE NAMES BACK TO SIMPOLIFY THE ANALYSES BELOW
sdlgs.all$cover.all<-NULL
names(sdlgs.all)[5]<-"cover"

####################################################################
################# WHAT FACTORS INFLUENCE SEEDLING COUNT?  
####################################################################
# Nice overview of GLMs here: http://plantecology.syr.edu/fridley/bio793/glm.html


##################
### NO SOILS DATA, IE, ALL THE PLOTS - PCA AXIS 1
##################


glm1A = glm(sdlg.no ~ nest, data=sdlgs.nosoil,family=gaussian)
summary(glm1A)

glm1B = glm(sdlg.no ~ location, data=sdlgs.nosoil,family=gaussian)
summary(glm1B)

glm1C = glm(sdlg.no ~ PCA1.nosoil, data=sdlgs.nosoil,family=gaussian)
summary(glm1C)

glm1D = glm(sdlg.no ~ location+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm1D)

glm1E = glm(sdlg.no ~ PCA1.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm1E)

glm1F = glm(sdlg.no ~ PCA1.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm1F)

glm1G = glm(sdlg.no ~ location+PCA1.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm1G)

AIC.NoSoils.PCA1.Sdlg<-AIC(glm1A,glm1B,glm1C, glm1D, glm1E, glm1F, glm1G)
AIC.NoSoils.PCA1.Sdlg
#LOWEST AIC - EFFECT OF LOCATION IS ALL YOU NEED TO FIT DATA FOR ABUNDNACE 


##################
### WITH SOILS DATA, IE, FEWER PLOTS - PCA AXIS 1
##################

glm2A = glm(sdlg.no ~ nest, data=sdlgs.all,family=gaussian)
summary(glm2A)

glm2B = glm(sdlg.no ~ location, data=sdlgs.all,family=gaussian)
summary(glm2B)

glm2C = glm(sdlg.no ~ PCA1.all, data=sdlgs.all,family=gaussian)
summary(glm2C)

glm2D = glm(sdlg.no ~ location+nest, data=sdlgs.all,family=gaussian)
summary(glm2D)

glm2E = glm(sdlg.no ~ PCA1.all+nest, data=sdlgs.all,family=gaussian)
summary(glm2E)

glm2F = glm(sdlg.no ~ PCA1.all+nest, data=sdlgs.all,family=gaussian)
summary(glm2F)

glm2G = glm(sdlg.no ~ location+PCA1.all+nest, data=sdlgs.all,family=gaussian)
summary(glm2G)

AIC.Soils.PCA1.Sdlg<-AIC(glm2A,glm2B,glm2C, glm2D, glm2E, glm2F, glm2G)
AIC.Soils.PCA1.Sdlg
#LOWEST AIC - EFFECT OF LOCATION and NEST, but still no PCA (GRADIENT) EFFECT!!!



##################
### NO SOILS DATA, IE, ALL THE PLOTS - PCA AXIS 2
##################


glm3A = glm(sdlg.no ~ nest, data=sdlgs.nosoil,family=gaussian)
summary(glm3A)

glm3B = glm(sdlg.no ~ location, data=sdlgs.nosoil,family=gaussian)
summary(glm3B)

glm3C = glm(sdlg.no ~ PCA2.nosoil, data=sdlgs.nosoil,family=gaussian)
summary(glm3C)

glm3D = glm(sdlg.no ~ location+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm3D)

glm3E = glm(sdlg.no ~ PCA2.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm3E)

glm3F = glm(sdlg.no ~ PCA2.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm3F)

glm3G = glm(sdlg.no ~ location+PCA2.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm3G)

AIC.NoSoils.PCA2.Sdlg<-AIC(glm3A,glm3B,glm3C, glm3D, glm3E, glm3F, glm3G)
AIC.NoSoils.PCA2.Sdlg
#LOWEST AIC - EFFECT OF LOCATION IS ALL YOU NEED TO FIT DATA FOR ABUNDNACE WITH PCA 2



##################
### WITH SOILS DATA, IE, FEWER PLOTS - PCA AXIS 2
##################

glm4A = glm(sdlg.no ~ nest, data=sdlgs.all,family=gaussian)
summary(glm4A)

glm4B = glm(sdlg.no ~ location, data=sdlgs.all,family=gaussian)
summary(glm4B)

glm4C = glm(sdlg.no ~ PCA2.all, data=sdlgs.all,family=gaussian)
summary(glm4C)

glm4D = glm(sdlg.no ~ location+nest, data=sdlgs.all,family=gaussian)
summary(glm4D)

glm4E = glm(sdlg.no ~ PCA2.all+nest, data=sdlgs.all,family=gaussian)
summary(glm4E)

glm4F = glm(sdlg.no ~ PCA2.all+nest, data=sdlgs.all,family=gaussian)
summary(glm4F)

glm4G = glm(sdlg.no ~ location+PCA2.all+nest, data=sdlgs.all,family=gaussian)
summary(glm4G)

AIC.Soils.PCA2.Sdlg<-AIC(glm4A,glm4B,glm4C, glm4D, glm4E, glm4F, glm4G)
AIC.Soils.PCA2.Sdlg
#LOWEST AIC - EFFECT OF OCAm ALONE AND IN COMBINATION WITH LOCATION. GRADIENT 



####################################################################
################# WHAT FACTORS INFLUENCE SEEDLING SPP RICHNESS?  
####################################################################


##################
### NO SOILS DATA, IE, ALL THE PLOTS - PCA AXIS 1
##################


glm5A = glm(spp.no ~ nest, data=sdlgs.nosoil,family=gaussian)
summary(glm5A)

glm5B = glm(spp.no ~ location, data=sdlgs.nosoil,family=gaussian)
summary(glm5B)

glm5C = glm(spp.no ~ PCA1.nosoil, data=sdlgs.nosoil,family=gaussian)
summary(glm5C)

glm5D = glm(spp.no ~ location+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm5D)

glm5E = glm(spp.no ~ PCA1.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm5E)

glm5F = glm(spp.no ~ PCA1.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm5F)

glm5G = glm(spp.no ~ location+PCA1.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm5G)

AIC.NoSoils.PCA1.Spp<-AIC(glm5A,glm5B,glm5C, glm5D, glm5E, glm5F, glm5G)
AIC.NoSoils.PCA1.Spp
#LOWEST AIC - EFFECTS OF LOCATION< NEST, AND COMBO OF NEST, GRADIENT, AND LOCATION. In other words, gradient effects diversity, but not abudnance
#this makes sense - species have niche requirements that will vary (hoffmann)


##################
### WITH SOILS DATA, IE, FEWER PLOTS - PCA AXIS 1
##################

glm6A = glm(spp.no ~ nest, data=sdlgs.all,family=gaussian)
summary(glm6A)

glm6B = glm(spp.no ~ location, data=sdlgs.all,family=gaussian)
summary(glm6B)

glm6C = glm(spp.no ~ PCA1.all, data=sdlgs.all,family=gaussian)
summary(glm6C)

glm6D = glm(spp.no ~ location+nest, data=sdlgs.all,family=gaussian)
summary(glm6D)

glm6E = glm(spp.no ~ PCA1.all+nest, data=sdlgs.all,family=gaussian)
summary(glm6E)

glm6F = glm(spp.no ~ PCA1.all+nest, data=sdlgs.all,family=gaussian)
summary(glm6F)

glm6G = glm(spp.no ~ location+PCA1.all+nest, data=sdlgs.all,family=gaussian)
summary(glm6G)

AIC.Soils.PCA1.Spp<-AIC(glm6A,glm6B,glm6C, glm6D, glm6E, glm6F, glm6G)
AIC.Soils.PCA1.Spp
#LOWEST AIC - E...BUT DON't OVER INTERPET ABOVE. ITS REALLY SOILS THAT DRIVE DIVERSITY - you see that when you include the soils data in the PCA - and THIS IS INFLUENCED BY ANTS



##################
### NO SOILS DATA, IE, ALL THE PLOTS - PCA AXIS 2
##################


glm7A = glm(spp.no ~ nest, data=sdlgs.nosoil,family=gaussian)
summary(glm7A)

glm7B = glm(spp.no ~ location, data=sdlgs.nosoil,family=gaussian)
summary(glm7B)

glm7C = glm(spp.no ~ PCA2.nosoil, data=sdlgs.nosoil,family=gaussian)
summary(glm7C)

glm7D = glm(spp.no ~ location+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm7D)

glm7E = glm(spp.no ~ PCA2.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm7E)

glm7F = glm(spp.no ~ PCA2.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm7F)

glm7G = glm(spp.no ~ location+PCA2.nosoil+nest, data=sdlgs.nosoil,family=gaussian)
summary(glm7G)

AIC.NoSoils.PCA2.Spp<-AIC(glm7A,glm7B,glm7C, glm7D, glm7E, glm7F, glm7G)
AIC.NoSoils.PCA2.Spp
#LOWEST AIC - LOCATION AND NEST AGAIN COME OUR IMPORTANT WHEN YOU DONT INCLUDE SOILS....



##################
### WITH SOILS DATA, IE, FEWER PLOTS - PCA AXIS 2
##################

glm8A = glm(spp.no ~ nest, data=sdlgs.all,family=gaussian)
summary(glm8A)

glm8B = glm(spp.no ~ location, data=sdlgs.all,family=gaussian)
summary(glm8B)

glm8C = glm(spp.no ~ PCA2.all, data=sdlgs.all,family=gaussian)
summary(glm8C)

glm8D = glm(spp.no ~ location+nest, data=sdlgs.all,family=gaussian)
summary(glm8D)

glm8E = glm(spp.no ~ PCA2.all+nest, data=sdlgs.all,family=gaussian)
summary(glm8E)

glm8F = glm(spp.no ~ PCA2.all+nest, data=sdlgs.all,family=gaussian)
summary(glm8F)

glm8G = glm(spp.no ~ location+PCA2.all+nest, data=sdlgs.all,family=gaussian)
summary(glm8G)

AIC.Soils.PCA2.Spp<-AIC(glm8A,glm8B,glm8C, glm8D, glm8E, glm8F, glm8G)
AIC.Soils.PCA2.Spp
#LOWEST AIC - ...BUT AXIS TWO AGAIN IMPORTATN WHEN YOU SOILS AGAIN PLAY A ROLE (THOUGH LESS FOR AXIS 1 than AXIS 2)











































































PC1vCover<-ggplot(GLM.DATA, aes(x=cover, y=PCA1, col=location, fill=location))+
  theme_classic()+
  geom_point(shape=16, size=3)+
  #   facet_grid(variable ~ .)+
  ylab("PCA1 Score") +  
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines, Don't add shaded confidence region
PC1vCover<-PC1vCover + scale_colour_manual(values=c("darkred", "orchid", "darkblue"))  #I chose my own colors for the lines
#PC1vCover<-PC1vCover + scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(-10, 1500))
# PC1vCover<-PC1vCover + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
PC1vCover





# #################
# ## FOR PCA AXIS 2: effects of nest, location on envtl conditions
# #################
# add a continuous predictor variable, fit the new glm and test it against a model with only an intercept:
glmD = glm(PCA2 ~ nest, data=GLM.DATA,family=gaussian)
summary(glmD)

glmE = glm(PCA2 ~ location, data=GLM.DATA,family=gaussian)
summary(glmE)

anova(glmD,glmE,test="Chisq")
#Result: model 2 not a significantly better fit

glmF = glm(PCA2 ~ location+nest, data=GLM.DATA,family=gaussian)
summary(glmF)
anova(glmD,glmF,test="Chisq")
#Result: model 3 IS a significantly better fit than just 1

AIC(glmD,glmE,glmF)
#Result: model 2 better fit
# 
#Anova of best fit model, not sure if this is the right thing to show in paper
# aov_best2 = aov(PCA2 ~ location+nest, data=GLM.DATA)
# summary(aov_best2)

# 
# #################
# ## FOR PCA AXIS 1
# #################
# 
# #
# #
# # DOES +nest need to be included, i.e., do you need to treat nest as a block?  
# #
# #
# 
# 
# # GLM to these data with just an intercept (overall mean):
# glm1 = glm(PCA1~1,family = gaussian, data = GLM.DATA)
# summary(glm1)
# 
# # add a continuous predictor variable, fit the new glm and test it against a model with only an intercept:
# glm2 = glm(PCA1 ~ location +nest, data=GLM.DATA,family=gaussian)
# anova(glm1,glm2,test="Chisq")
# #Result: model 2 better fit
# 
# #Add Percent cover as a covariate
# glm3 = glm(PCA1 ~ location + cover + nest,data=GLM.DATA,family=gaussian)
# summary(glm3)
# anova(glm2,glm3,test="Chisq")
# # looks like including cover is better than just location
# 
# # Is there an interaction?
# glm4 = glm(PCA1 ~ location * cover +nest,data=GLM.DATA,family=gaussian) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(glm4)
# anova(glm3,glm4,test="Chisq")
# #Doesn't look like including interaction provides better fit
# 
# glm5 = glm(PCA1 ~ cover +nest,data=GLM.DATA,family=gaussian)
# anova(glm1,glm5,test="Chisq")
# #Result: model 2 better fit
# 
# glm6 = glm(PCA1~nest,family = gaussian, data = GLM.DATA)
# summary(glm6)
# 
# 
# AIC(glm1, glm2,glm3,glm4, glm5, glm6)
# 
# #################
# ## FOR PCA AXIS 2
# #################
# 
# 
# # GLM to these data with just an intercept (overall mean):
# glm2.1 = glm(PCA2~1,family = gaussian, data = GLM.DATA)
# summary(glm2.1)
# 
# # add a continuous predictor variable, fit the new glm and test it against a model with only an intercept:
# glm2.2 = glm(PCA2 ~ location +nest, data=GLM.DATA,family=gaussian)
# anova(glm2.1,glm2.2,test="Chisq")
# #Result: model 2 better fit
# 
# #Add Percent cover as a covariate
# glm2.3 = glm(PCA2 ~ location + cover + nest,data=GLM.DATA,family=gaussian)
# summary(glm2.3)
# anova(glm2.2,glm2.3,test="Chisq")
# # looks like including cover is better than just location
# 
# # Is there an interaction?
# glm2.4 = glm(PCA2 ~ location * cover +nest,data=GLM.DATA,family=gaussian) #Recall * is syntax syntax shortcue of both main effects + interaction
# summary(glm2.4)
# anova(glm2.3,glm2.4,test="Chisq")
# #Doesn't look like including interaction provides better fit
# 
# glm2.5 = glm(PCA2 ~ cover +nest,data=GLM.DATA,family=gaussian)
# anova(glm2.1,glm2.5,test="Chisq")
# #Result: model 2 better fit
# 
# glm2.6 = glm(PCA2~nest,family = gaussian, data = GLM.DATA)
# summary(glm2.6)
# 
# 
# AIC(glm2.1, glm2.2,glm2.3,glm2.4, glm2.5, glm2.6)
# 
#################
## PLOTS of PCA scores v Canopy COver, etc.
#################

PCAfigData<-gather(GLM.DATA, "Axis", "PCA.Score", 4:5)

# my_grob_ENV1 = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))


PCAfig<-ggplot(PCAfigData, aes(x=cover, y=PCA.Score, col=location, fill=location)) + 
  geom_point(shape=16, size=3)+
  #   facet_grid(variable ~ .)+
  facet_wrap(~Axis,nrow = 2,scales = "free")+
  ylab("PCA Score") +  
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines, Don't add shaded confidence region
# +annotation_custom(my_grob_ENV1)
PCAfig<-PCAfig + scale_colour_manual(values=c("blue", "red"))  #I chose my own colors for the lines
PCAfig<-PCAfig + scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4.5, 4.5))
PCAfig<-PCAfig + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
# 


PCAfig<- PCAfig + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        panel.margin = unit(2, "lines"), #space between facets
        axis.line = element_line(colour = "black"), #sets colors of axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),
        legend.position = c(0.9,0.95),
        strip.text.x = element_text(size=18, colour="black", face="bold", vjust=-1.4, hjust=.05),
        strip.background = element_blank(),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'), #box around legend
        plot.margin =unit(c(0,1,2,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

PCAfig

# 
# PAIRED LINES so you ccan compare cover or PCA scores for each nest


arrange(PCAfigData, nest) #sort them by nest so that ggplot can make the plot correctly
filter(PCAfigData, Axis == "PCA1") #select which of the PCA scores you will be mapping, if any

# y=PCA.Score
# y=cover

ggplot(data=PCAfigData, aes(x=location, y=PCA.Score, group=nest)) +
  geom_line() +
  geom_point()




# for quesiton on dot colors posted on stack overflow
# http://stackoverflow.com/questions/30968563/ggbiplot-how-to-maintain-group-colors-after-changing-point-size

env.vars<-data.frame(replicate(5,sample(0:10,20,rep=TRUE)))
cover<-c(89, 92, 72, 53, 88, 89, 71, 83, 71, 66, 23, 30,  5, 15, 57, 54,0, 23, 9, 16)
location<-c("location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2")
point.size<-cover*0.1





# #########
# PCA 1 v canopy cover IF caniopy cover not in PCA
# ############

# #################
# ## FOR PCA AXIS 1: effects of nest, location on envtl conditions
# #################
# add a continuous predictor variable, fit the new glm and test it against a model with only an intercept:
plot(GLM.DATA$cover,GLM.DATA$PCA1)

ggplot(GLM.DATA, aes(x=cover, y=PCA1, col=location)) + geom_point(shape=16, size = 3)+  ylab("PCA Score") +   xlab("Canopy cover (%)")+geom_smooth(method=lm, se=FALSE)    # Don't add shaded confidence region


glmAA = glm(PCA1 ~ nest, data=GLM.DATA,family=gaussian)
summary(glmAA)

glmBB = glm(PCA1 ~ location, data=GLM.DATA,family=gaussian)
summary(glmBB)

glmCC = glm(PCA1 ~ cover, data=GLM.DATA,family=gaussian)
summary(glmBB)

glmDD = glm(PCA1 ~ location+nest+cover, data=GLM.DATA,family=gaussian)
summary(glmDD)

glmEE = glm(PCA1 ~ location+cover, data=GLM.DATA,family=gaussian)
summary(glmEE)

glmFF = glm(PCA1 ~ location+nest, data=GLM.DATA,family=gaussian)
summary(glmDD)

glmGG = glm(PCA1 ~ nest+cover, data=GLM.DATA,family=gaussian)
summary(glmGG)


AIC(glmAA,glmBB,glmCC, glmDD, glmEE, glmFF, glmGG)


#Result: model 2 better fit
# 
