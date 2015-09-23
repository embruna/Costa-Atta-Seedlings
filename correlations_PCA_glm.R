# #PCA of environmental data in R
# Used http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/ as a guide

library(devtools)
#install_github("ggbiplot", "vqv")
library(ggbiplot)
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
NEST.DATA.both<- droplevels(NEST.DATA.both)
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

# NOW CHOOSE WHIHC DATASET TO USE FOR PCA - WITH OR WITHOUT SOILS DATA?
#NEST.DATA.PCA<-NEST.DATA.PCA.ALL
NEST.DATA.PCA<-NEST.DATA.PCA.NOSOILS

#Clear all those with missing values to do PCA
NEST.DATA.PCA<-na.omit(NEST.DATA.PCA) 
str(NEST.DATA.PCA)
summary(NEST.DATA.PCA)


# log transform 
# env.vars <- log(NEST.DATA.PCA[, 5:18]+1)
# no transformation as per HLV

# PAY ATTENTION - FIX THIS DEPENDING ON IF PCA ALL OR PCA "NO SOILS"

# #FOR PCA ALL (WITH SOILS)
# env.vars <- NEST.DATA.PCA[,5:16]
# site.cats <- NEST.DATA.PCA[, 1:4]

# #FOR PCA ALL (NO SOILS)
env.vars <- NEST.DATA.PCA[,5:9]
site.cats <- NEST.DATA.PCA[, 1:4]

# ARE THERE ANY ENV VARIABLE YOU WANT TO EXCLUDE? If so toggle off












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




# The Figure below is a biplot generated by the function ggbiplot of the ggbiplot package available on https://github.com/vqv/ggbiplot
# TO install: 
# library(devtools)
# install_github("ggbiplot", "vqv")
# library(ggbiplot)
# ?ggbiplot for more info on arguments you can change and % used to draw ellipses aroudnd points
location<-site.cats$location
cover<-env.vars$perc.cover
nest<-site.cats$nest

# FIGURE - PCA WITH SOILS (REDUCED NUMBER OF PLOTS)
point.size<-cover*0.1
g_soils <- ggbiplot(nest.env.pca, obs.scale = 1, var.scale = 1, 
                    group = location, ellipse = TRUE, 
                    circle = TRUE, varname.size=3)+
  scale_colour_manual(values=c("darkred", "darkblue")) +
  #geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06
  geom_point(aes(color=location, size = point.size)) + scale_size_identity()
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


# FIGURE - PCA WITH NO SOILS (MORE PLOTS)
point.size<-cover*0.1
g__no_soils <- ggbiplot(nest.env.pca, obs.scale = 1, var.scale = 1, 
                    group = location, ellipse = TRUE, 
                    circle = TRUE, varname.size=3)+
  scale_colour_manual(values=c("darkred", "orchid", "darkblue")) +
  #geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06
  geom_point(aes(color=location, size = point.size)) + scale_size_identity()
#I chose my own colors for the lines
g__no_soils<-g__no_soils + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-5, 4)) # I adjusted X axis so that I could read the larger labels on arrows
g__no_soils<g__no_soils + scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4,4)) # I adjusted Y axis so that I could read the larger labels on arrows


g__no_soils <-g__no_soils + theme_classic()+theme(legend.direction = 'horizontal', 
                                          legend.position = 'top',
                                          axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
                                          legend.title = element_blank(), #remove title of legend
                                          legend.text = element_text(color="black", size=22, vjust =2),
                                          plot.margin =unit(c(1,1,1,1.5), "lines")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

print(g__no_soils)


#########################################
## ANALYSES BASED ON PCA
#########################################

#nest.env.pca$x =  scores for each of the plots for each PCA


pca.plot.scores<-(nest.env.pca$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1<-as.data.frame(pca.plot.scores[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2<-pca.plot.scores[,2]#this saves the 2nd column - PCA Axis 2 - as a dataframe
GLM.DATA<-as.data.frame(cbind(nest,location, cover, PCA.1, PCA.2))
names(GLM.DATA)[4]<-"PCA1" #reaname the column
names(GLM.DATA)[5]<-"PCA2" #rename the column































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




# Nice overview of GLMs here: http://plantecology.syr.edu/fridley/bio793/glm.html
# #################
# ## FOR PCA AXIS 1: effects of nest, location on envtl conditions
# #################
# add a continuous predictor variable, fit the new glm and test it against a model with only an intercept:


glmB = glm(PCA1 ~ cover, data=GLM.DATA,family=gaussian)
summary(glmB)

# glmC = glm(PCA1 ~ location, data=GLM.DATA,family=gaussian)
# summary(glmC)

glmA = glm(PCA1 ~ nest, data=GLM.DATA,family=gaussian)
summary(glmA)

# glmD = glm(PCA1 ~ cover+location, data=GLM.DATA,family=gaussian)
# summary(glmD)

glmE = glm(PCA1 ~ cover+nest, data=GLM.DATA,family=gaussian)
summary(glmE)

# glmF = glm(PCA1 ~ location+nest, data=GLM.DATA,family=gaussian)
# summary(glmF)

# glmG = glm(PCA1 ~ cover+location+nest, data=GLM.DATA,family=gaussian)
# summary(glmG)

AIC(glmA,glmB,glmC, glmD, glmE, glmF, glmG)
AIC(glmA, glmE)

anova(glmA,glmE,test="Chisq")

anova(glmA,glmC,test="Chisq")
#Result: model 3 IS a significantly better fit than just 1

#Anova of best fit model, not sure if this is the right thing to show in paper
# aov_best1 = aov(PCA1 ~ location+nest, data=GLM.DATA)
# summary(aov_best1)



#Result: model 2 better fit
# 


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
