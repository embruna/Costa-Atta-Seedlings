#=============================================================================================================#
# Script created by Emilio M. Bruna, embruna@ufl.edu
# Script created in R version 3.3.1
# Code to conduct the analyses and generate the figures in Costa et al. PeerJ

# Notes on analysis
# For PCA of environmental data in R Used http://www.r-bloggers.com/computing-and-visualizing-pca-in-r/ as a guide
#=============================================================================================================#

# For PCA figures used: ggbiplot package available here: https://github.com/vqv/ggbiplot
# To install it:
# library(devtools)
# library (ggbiplot)
# install_github("ggbiplot", "vqv")
# BUT I forked the original ggbiplot and edited it to change the arrow colors, so...
library(tidyverse) #Data Manipulations+ggplo1
library(devtools)
install_github("embruna/ggbiplot") 
library(ggbiplot)
library(gridExtra) #user-level functions to work with "grid" graphics (e.g., arrange multiple grid-based plots on page, draw tables).
library(reshape2)  #was still learning to use pdlyr, so used some melt and cast
library(lme4)      #Fit linear and generalized linear mixed-effects models. 
library(MuMIn)     #Model selection and model averaging based on information criteria (AICc and alike).
library(arm)       #R functions for processing 'lm', 'glm', 'svy.glm', 'merMod' and 'polr' outputs.
library(broom)

#library(grid) #NO LONGER ON CRAN!
# library(broom) #ONLY NEEDED UNTIL CAN MAKE CHANGE TO R CODE IN GGBIPLOT
# http://stackoverflow.com/questions/3384598/how-to-edit-and-debug-r-library-sources
# trace("ggbiplot",edit=TRUE)


#Clear out everything from the environment
# rm(list=ls())

############################################################################################################
### DATA ENTRY AND CLEANUP
############################################################################################################

#########################################
#  Nest data import and selection
#########################################

# Step 1: load the individual CSV files and save them as dataframes
NEST.DATA<-read.csv("./Data/ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
# NEST.SIZE.DATA<-read.csv("./Data/nest-size-data.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

# make plot locations an ordered factor nest<adjacent<far
NEST.DATA$location=factor(NEST.DATA$location, levels=c("nest","adjacent", "far"), ordered=TRUE)

# Make Nest ID a factor
NEST.DATA$nest<-as.factor(NEST.DATA$nest)
# To double check ok toggle
# head(NEST.DATA, 3)
# str(NEST.DATA)
# SELECT WHICH VEGETATION TYPE YOU WANT TO WORK WITH
# NEST.DATA.CD<-filter(NEST.DATA, habitat =="CD")  # Select only Cerrado Denso 
# NEST.DATA.CR<-filter(NEST.DATA, habitat == "CR") # Select only Cerrado Ralo
nest_data<-filter(NEST.DATA, habitat =="CD" | habitat == "CR") # Select only Cerrado Denso and Cerrado Ralo
nest_data<-droplevels(nest_data)
str(nest_data)


#########################################
# Vegetation Census data and cleanup
#########################################
VEG<-read.csv("./Data/ActiveNests_CensoVeg_1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
VEG$location=factor(VEG$location, levels=c("nest","adjacent", "far"), ordered=TRUE)

VEG <-dplyr::rename(VEG, spp.code=species)
VEG$spp.code <-tolower(VEG$spp.code)
VEG$spp.code<-as.factor(VEG$spp.code)
VEG$spp.code[VEG$spp.code=="pro.epy"] <- "pro.epi"
VEG$spp.code[VEG$spp.code=="manihot.sp"] <- "man.sp"
VEG$spp.code<-droplevels(VEG$spp.code)

veg_data<-VEG[VEG$habitat=="CR"|VEG$habitat=="CD",] #both habitats
# veg_data<-VEG[VEG$habitat=="CR",] # CR habitat only
# veg_data<-VEG[VEG$habitat=="CD",] # CD habitat only
veg_data<- droplevels(veg_data)

#########################################
# Species List: Import, cleanup, add to veg_data, and delete
#########################################
spp_list<-read.csv("./Data/Spp_ID_Habit.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE , strip.white=TRUE, stringsAsFactors = FALSE)
spp_list$GS <- as.factor(paste(spp_list$Genus, spp_list$Species, sep=" "))
spp_list$spp.code<-tolower(spp_list$spp.code)
spp_list$spp.code[spp_list$spp.code=="manihot.sp"] <- "man.sp"
spp_list$spp.code<-as.factor(spp_list$spp.code)
spp_list$spp.code<-droplevels(spp_list$spp.code)
levels(spp_list$Genus)
levels(spp_list$GS)

# Add genus, species, etc. to the dataset
veg_data<-full_join(spp_list,veg_data,by="spp.code")
veg_data<-veg_data %>% dplyr::select(-Author)
# veg_data<- veg_data %>% dplyr::select(-Genus,-Species,-Author)
# can now remove a bunch of datasets to cleanup the environment()
rm(spp_list,VEG,NEST.DATA)


#########################################
# PREPARE DATASET FOR ANALYSES
#########################################
# Select the plant types: (herb, shrub, vine, tree)

# veg_data<-veg_data %>% dplyr::filter(Habit=="herb") #ONLY HERBS
veg_data<-veg_data %>% dplyr::filter(Habit!="herb") # NO HERBS
veg_data$Habit<-as.factor(veg_data$Habit)
veg_data$Habit<-droplevels(veg_data$Habit)
summary(veg_data)

#########################################
# SUMMARY STATS
#########################################

#Number of individual plants 
Total_sdlgs<-nrow(veg_data)
Total_sdlgs

# how many genera
DistinctGenera<-summarise(veg_data,n_distinct(Genus))
DistinctGenera

# how many species (including morphospp)
DistinctSpecies<-summarise(veg_data,n_distinct(GS))
DistinctSpecies

# how many morphospecies and what % of species id's are morpho species
DistinctMorpho<-veg_data %>% filter(Genus=="Morphospecies") %>% summarise(n_distinct(GS))
DistinctMorpho

# what proportion of species are morphospecies?
# prop.morpho<-DistinctMorpho/DistinctSpecies

# percentage of stems by veg type (shrub, tree, vine, herbaceous)
veg_type_stems<-veg_data %>% dplyr::select(Habit) %>% dplyr::group_by(Habit) %>% dplyr::summarize(n=n()) 
veg_type_stems$perc<-veg_type_stems$n/sum(veg_type_stems$n)*100
veg_type_stems<-veg_type_stems %>% arrange(desc(perc))
veg_type_stems

# percentage of species by veg type (shrub, tree, vine, herbaceous)
# veg_type_spp<-veg_data %>% dplyr::select(GS, Habit) %>% dplyr::distinct(GS,Habit) %>% dplyr::group_by(Habit) %>% dplyr::summarize(n=n()) 
# veg_type_spp$perc<-veg_type_spp$n/sum(veg_type_spp$n)*100
# veg_type_spp<-veg_type_spp %>% arrange(desc(perc))
# veg_type_spp

# Most common species percentage by each species, and cumulative sum of most common
top_species<-veg_data %>% dplyr::select(GS) %>% ungroup(top_species)  
top_species<-veg_data %>% dplyr::group_by(GS) %>% dplyr::summarize(n=n()) %>% ungroup(top_species)
top_species<-arrange(top_species,desc(n))
top_species<-na.omit(top_species)
top_species$perc<-top_species$n/sum(top_species$n)*100
top_species<-ungroup(top_species)
top_species<-as.data.frame(top_species)
sum(top_species$perc)
top_species<-mutate(top_species,cumulative=cumsum(perc))
# sum(top_species$n) # 2x you didn't miss any, should = number of stems
#table of 20 most common species
top_twenty<-slice(top_species,1:20)

# seedling height
plant_ht<-veg_data %>% dplyr::select(ht_cm) %>% ungroup(plant_ht)  
plant_ht<-arrange(plant_ht,ht_cm)
plant_ht<-na.omit(plant_ht)
mean_ht<-mean(plant_ht$ht_cm)
sd_ht<-sd(plant_ht$ht_cm)
# n_plant_ht<-nrow(plant_ht)
# range_plant_ht<-range(plant_ht$ht_cm)

# Proportion in different height classes
breaks = seq(0, 125, by=10)
ht.cut = cut(plant_ht$ht_cm, breaks, right=FALSE)
ht.freq = table(ht.cut)
ht.freq<-cbind(ht.freq) 
ht.freq<-as.data.frame(ht.freq)
ht.freq$percentage<-ht.freq$ht.freq/sum(ht.freq$ht.freq)*100
ht.freq<-mutate(ht.freq,cumulative=cumsum(percentage))
ht.freq
rm(ht.cut,breaks)

# Average number of seedlings per plot - all plots combined

# FIRST NEED A dataframe with all plots - nest, location,. plot id no
all_plots<-dplyr::select(nest_data, habitat, nest, location, plot.id, perc.cover)
all_plots<-droplevels(na.omit(all_plots))
all_plots<-dplyr::select(all_plots,-perc.cover)
all_plots$plot.id<-as.factor(all_plots$plot.id)
# summarize the number of plants n each plot (note that some plots aren't represented bc there were zero seedlings there)
avg_per_plot<-veg_data %>% group_by(plot.id) %>% dplyr::summarise(N=n_distinct(number)) 
avg_per_plot$plot.id<-as.factor(avg_per_plot$plot.id)
avg_per_plot<-as.data.frame(avg_per_plot)
# join up the two, replace NA in N with zero
avg_per_plot<-full_join(all_plots, avg_per_plot, by="plot.id")
avg_per_plot$N[is.na(avg_per_plot$N)] <- 0
# dim(avg_per_plot)

# SEEDLING NUMBER PER PLOT: AVG, SD, Range (all LOCATIONS AND HABITATS POOLED) 
avg_sdlgs<-mean(avg_per_plot$N)
SD_sdlgs<-sd(avg_per_plot$N)
range_sdlgs<-range(avg_per_plot$N)

# SEEDLING NUMBER PER PLOT: AVG, SD BY LOCATION, ALL HABITATS POOLED
avg_by_loc<-avg_per_plot %>% group_by(location) %>% dplyr::summarise(mean=mean(N)) 
sd_by_loc<-avg_per_plot %>% group_by(location) %>% dplyr::summarise(sd=sd(N)) 
avg_sd_bylocation<-full_join(avg_by_loc,sd_by_loc,by="location")


# Species per plot by location
# summarize the number of spp n each plot; note that some plots aren't represented bc there were zero seedlings there)
spp.no.df<-veg_data %>% group_by(plot.id) %>% dplyr::summarise(spp_N=n_distinct(spp.code)) 
spp.no.df$plot.id<-as.factor(spp.no.df$plot.id)
spp.no.df<-as.data.frame(spp.no.df)
# join up the two, replace NA in N with zero
spp.no.df<-full_join(all_plots, spp.no.df, by="plot.id")
spp.no.df$spp_N[is.na(spp.no.df$spp_N)] <- 0

avg_spp_plot_loc<-spp.no.df %>% group_by(location) %>% dplyr::summarise(mean=mean(spp_N)) 
sd_spp_plot_loc<-spp.no.df %>% group_by(location) %>% dplyr::summarise(sd=sd(spp_N)) 
avg_sd_spp_plot_bylocation<-full_join(avg_spp_plot_loc,sd_spp_plot_loc,by="location")
avg_sd_spp_plot_bylocation

rm(avg_by_loc,sd_by_loc)
rm(avg_sdlgs,SD_sdlgs,range_sdlgs,Total_sdlgs)
rm(mean_ht, sd_ht, range_plant_ht,n_plant_ht,plant_ht)

############################################################################################################
### TABLE 1: Does Canopy Cover vary with Proximity to ant nests? 
############################################################################################################

# analyses
str(nest_data)
coverxhab<-dplyr::select(nest_data, habitat, nest, location, perc.cover, nest.area)
coverxhab<-droplevels(na.omit(coverxhab))
coverxhab %>% group_by(location) %>% summarise(mean.perc.cover=mean(perc.cover))
coverxhab %>% group_by(location) %>% summarise(var.perc.cover=var(perc.cover))

# Nest identity is a random effect RANDOM<-(1|nest)
coverxhab$cover.prop<-coverxhab$perc.cover/100
coverxhab <- coverxhab[order(coverxhab$cover.prop),] 

# logit trasnform and add smallest value to correct for zero 
# as per http://www.esajournals.org/doi/full/10.1890/10-0340.1#appB
coverxhab$logit.cover<-log10((coverxhab$cover.prop+0.01)/(1-coverxhab$cover.prop+0.01))
hist(coverxhab$logit.cover)
qqnorm(coverxhab$logit.cover)
qqline(coverxhab$logit.cover)

# Bartlett test of homogeneity of variances
bartlett.test(coverxhab$logit.cover ~ coverxhab$location)            
# Shapiro-Wilk normality test Result: not normally distributed (but we knew that) 
shapiro.test(resid(aov(coverxhab$logit.cover ~ coverxhab$location))) 

# GLMM w/ CANOPY COVER AS FIXED
options(na.action = "na.fail") #for calc of QAIC see page 42: https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf
# only random effect
cover1<-lmer((logit.cover) ~ (1|nest), data = coverxhab, REML = TRUE)
summary(cover1)
# zero slopes, different intercepts
cover2<-lmer(logit.cover ~ location + (1|nest), data = coverxhab, REML = TRUE)
summary(cover2)
# specifies single line
cover3<-lmer(logit.cover ~  nest.area + (1|nest), data = coverxhab, REML = TRUE)
summary(cover3)
# specifies parallel slopes
cover4<-lmer(logit.cover ~ location + nest.area + (1|nest), data = coverxhab, REML = TRUE)
summary(cover4)
# different intercepts and slopes
cover5<-lmer(logit.cover ~ location * nest.area + (1|nest), data = coverxhab, REML = TRUE)
summary(cover5)

#plot deviance residuals against fitted values
plot(cover5)

AIC(cover1,cover2, cover3, cover4, cover5)
anova(cover2, cover1, test = "Chisq")
anova(cover3, cover1, test = "Chisq")
anova(cover4, cover1, test = "Chisq")
anova(cover5, cover1, test = "Chisq")
anova(cover5, cover3, test = "Chisq")
anova(cover5)

# calclulating QAIC: Calculate a modification of Akaike's Information Criterion for overdispersed count data 
# (or its version corrected for small sample, “quasi-AICc”), for one or several fitted model objects.
(chat <- deviance(cover5) / df.residual(cover5))
dredge(cover5, rank = "QAIC", chat = chat)
dredge(cover5, rank = "AIC")

# Need to generate table of: # random effect of nest identity (1)
# plot proximity to ant nests, nest area, and nest identity (4)
# main effects of nest area and plot location, their interaction, and nest identity (5)
# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report

summary.table.cover <- do.call(rbind, lapply(list(cover1, cover2, cover3, cover4, cover5), broom::glance))
summary.table.cover[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table <- summary.table.cover[table.cols]
names(reported.table) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table[['dAIC']] <-  with(reported.table, AIC - min(AIC))
reported.table[['wAIC']] <- with(reported.table, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table$AIC <- NULL
############################################################################################################
### TABLE 1: GLMM canopy cover v plot priximity  
############################################################################################################
write.csv(reported.table, file="./Output/Table1_CoverxLocTable.csv", row.names = F) #export it as a csv file









############################################################################################################
### PCA: Data Prep
############################################################################################################

#This is missing in a bunch of them, so will reduce sample size too much
nest_data$peak.soil.temp<-NULL
nest_data$time.peak.soil.temp<-NULL
nest_data$date.peak.soil.tem<-NULL
nest_data$soil.moisture.deep<-NULL

NEST.DATA.PCA.NOSOILS<-nest_data
NEST.DATA.PCA.NOSOILS$ph<-NULL
NEST.DATA.PCA.NOSOILS$P<-NULL
NEST.DATA.PCA.NOSOILS$K<-NULL
NEST.DATA.PCA.NOSOILS$Ca<-NULL
NEST.DATA.PCA.NOSOILS$Mg<-NULL
NEST.DATA.PCA.NOSOILS$Al<-NULL
NEST.DATA.PCA.NOSOILS$org.mat<-NULL    

#Clear all those with missing values to do PCA
# nest_data<-na.omit(nest_data) 
NEST.DATA.PCA.NOSOILS<-na.omit(NEST.DATA.PCA.NOSOILS) 
# droplevels(nest_data)
droplevels(NEST.DATA.PCA.NOSOILS)
dim(NEST.DATA.PCA.NOSOILS)


# REMOVE CANOPY COVER FROM THE PCA 
# (recall we are testing if canopy cover is independent in effects on env't and seedlings). 
# BUT FIRST CREATE A VECTOR OF CANOPY COVER AND NEST AREA IN CASE YOU NEED IT LATER
cover.nosoils<-NEST.DATA.PCA.NOSOILS$perc.cover
nest.area.nosoils<-NEST.DATA.PCA.NOSOILS$nest.area

NEST.DATA.PCA.NOSOILS$perc.cover<-NULL
NEST.DATA.PCA.NOSOILS$nest.area<-NULL

# FOR PCA ALL (NO SOILS)
env.vars.nosoil <- dplyr::select(NEST.DATA.PCA.NOSOILS,litter.bmass,soil.pen,grass.bmass,soil.moisture.surface)  
env.vars.nosoil<-na.omit(env.vars.nosoil)

site.cats.nosoil <- dplyr::select(NEST.DATA.PCA.NOSOILS,habitat,nest,plot.id,location)
site.cats.nosoil<-na.omit(site.cats.nosoil)

dim(env.vars.nosoil)
dim(site.cats.nosoil)



# str(nest_data)
# summary(nest_data)
# str(NEST.DATA.PCA.NOSOILS)
# summary(NEST.DATA.PCA.NOSOILS)


# nest_data$perc.cover<-NULL
# NEST.DATA.PCA.NOSOILS$perc.cover<-NULL

# nest_data$nest.area<-NULL
# NEST.DATA.PCA.NOSOILS$nest.area<-NULL

# if you wanted to log transform your variables you would do so here. 
# However, we aren't transforming as per convo with HLV
# env.vars <- log(NEST.DATA.PCA[, 5:18]+1)




# FOR PCA ALL (WITH SOILS)
NEST.DATA.PCA.WITH_SOILS <- nest_data
NEST.DATA.PCA.WITH_SOILS<-dplyr::select(NEST.DATA.PCA.WITH_SOILS,perc.cover,nest.area,habitat,nest,plot.id,location,litter.bmass,soil.pen,grass.bmass,ph,P,K,Ca,Mg,Al,org.mat,soil.moisture.surface)  
NEST.DATA.PCA.WITH_SOILS<-na.omit(NEST.DATA.PCA.WITH_SOILS)
coverwithsoil<- NEST.DATA.PCA.WITH_SOILS %>% dplyr::select(perc.cover,plot.id)
nest.areawithsoil<-NEST.DATA.PCA.WITH_SOILS %>% dplyr::select(nest,nest.area)

env.varswithsoil<-dplyr::select(NEST.DATA.PCA.WITH_SOILS,litter.bmass,soil.pen,grass.bmass,ph,P,K,Ca,Mg,Al,org.mat,soil.moisture.surface)  
env.varswithsoil <- na.omit(env.varswithsoil)

site.catswithsoil <- dplyr::select(NEST.DATA.PCA.WITH_SOILS,habitat,nest,plot.id,location)
site.catswithsoil<-na.omit(site.catswithsoil)
dim(env.varswithsoil)
dim(site.catswithsoil)




############################################################################################################
### PCA-NO.SOIL (no soils chem, all plots and nests)
############################################################################################################

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


############################################################################################################
### PCA-WITH SOIL (with soils chem, only plots "on" and "far")
############################################################################################################

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
nest.env.with.soilchem <- prcomp(env.varswithsoil,
                           center = TRUE,
                           scale. = TRUE) 

# Visualizing the results.
# The print method returns the standard deviation of each of the four PCs, 
# and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables.
print(nest.env.with.soilchem)

# The Figure below is useful to decide how many PCs to retain for further analysis. 
# I.E., which PCs explain most of the variability in the data.
plot(nest.env.with.soilchem, type = "l")

# The summary method describe the importance of the PCs. 
# The first row describe again the standard deviation associated with each PC. 
# The second row shows the proportion of the variance in the data explained by each component.
# The third row describe the cumulative proportion of explained variance. 
# summary method
summary(nest.env.with.soilchem)




############################################################################################################
### TABLE 2: PCA Factor Loadings (no soils data, all plots)
############################################################################################################
print(nest.env.pca.nosoil)
names<-dimnames(nest.env.pca.nosoil$rotation)
names<-as.data.frame(names[[1]])
values<-as.data.frame(nest.env.pca.nosoil$rotation)
nest.env.pca.nosoil.df<-cbind(names,values)
write.csv(as.data.frame(nest.env.pca.nosoil.df), file="./Output/Table2_PCA_LOADS_NOSOIL.csv", row.names = F) #export it as a csv file

############################################################################################################
### TABLE 3: PCA Factor Loadings (soils, subset of plots)
############################################################################################################
print(nest.env.with.soilchem)
names<-dimnames(nest.env.with.soilchem$rotation)
names<-as.data.frame(names[[1]])
values<-as.data.frame(nest.env.with.soilchem$rotation)
nest.env.with.soilchem.df<-cbind(names,values)
write.csv(nest.env.with.soilchem.df, file="./Output/Table3_PCA_LOADS_NOSOIL.csv", row.names = F) #export it as a csv file

# rm(names,values,nest.env.with.soilchem.df)



############################################################################################################
############################################################################################################
### ANALYSES OF SEEDLING # AND DIV VS PCA SCORES
############################################################################################################
############################################################################################################


############################################################################################################
## START BY EXTRACTING COMPONENT SCORES - PCA NO SOIL CHEM
############################################################################################################

location.nosoil<-as.data.frame(site.cats.nosoil$location)
cover.nosoil<-as.data.frame(cover.nosoils)
nest.nosoil<-as.data.frame(site.cats.nosoil$nest)


#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scores.nosoil<-(nest.env.pca.nosoil$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1.nosoil<-as.data.frame(pca.plot.scores.nosoil[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2.nosoil<-as.data.frame(pca.plot.scores.nosoil[,2])#this saves the 2nd column - PCA Axis 2 - as a dataframe
GLM.DATA.nosoil<-as.data.frame(cbind(nest.nosoil,location.nosoil, cover.nosoils, PCA.1.nosoil, PCA.2.nosoil))
names(GLM.DATA.nosoil)[1]<-"nest" #reaname the column
names(GLM.DATA.nosoil)[2]<-"location" #rename the column
names(GLM.DATA.nosoil)[3]<-"cover" #rename the column
names(GLM.DATA.nosoil)[4]<-"PCA1.nosoil" #reaname the column
names(GLM.DATA.nosoil)[5]<-"PCA2.nosoil" #rename the column

dim(GLM.DATA.nosoil)
############################################################################################################
## START BY EXTRACTING COMPONENT SCORES - PCA WITH SOIL CHEM
############################################################################################################
locationwithsoil<-site.catswithsoil %>% dplyr::select(plot.id,location)
dim(locationwithsoil)

coverwithsoil<-as.data.frame(coverwithsoil)
dim(coverwithsoil)

nestwithsoil<-site.catswithsoil %>% dplyr::select(plot.id,nest)
dim(nestwithsoil)

GLM.DATAwithsoil<-full_join(locationwithsoil,coverwithsoil,nestwithsoil, by="plot.id")
GLM.DATAwithsoil<-na.omit(GLM.DATAwithsoil)
GLM.DATAwithsoil<-as.data.frame(GLM.DATAwithsoil)
#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scoreswithsoil<-(nest.env.with.soilchem$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1withsoil<-as.data.frame(pca.plot.scoreswithsoil[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2withsoil<-pca.plot.scoreswithsoil[,2]#this saves the 2nd column - PCA Axis 2 - as a dataframe

GLM.DATAwithsoil<-cbind(GLM.DATAwithsoil,PCA.1withsoil, PCA.2withsoil)
# GLM.DATAwithsoil<-as.data.frame(cbind(nestwithsoil,locationwithsoil, coverwithsoil, PCA.1withsoil, PCA.2withsoil))
names(GLM.DATAwithsoil)[4]<-"PCA1withsoil" #reaname the column
names(GLM.DATAwithsoil)[5]<-"PCA2withsoil" #rename the column
names(GLM.DATAwithsoil)[3]<-"cover" #rename the column

############################################################################################################
#### COUNT OF SEEDLINGS AND SPP RICH IN PLOTS 
############################################################################################################

veg_data$ht_cm<-as.numeric(veg_data$ht_cm)
veg_data$nest<-as.factor(veg_data$nest)
summary(veg_data)
str(veg_data)
# SppList<-veg_data %>% select(species) %>%  group_by(species) %>% mutate(N=n_distinct(species)) %>% group_by(species) %>% summarize(N=sum(N)) %>% arrange(desc(N))
# sum(SppList$N)
# summarize(count(species))(N=count(species)) %>% arrange(N) 
# SppList<-distinct(SppList)
# write.csv(SppList, file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan//Paper 2 thesis Ch2 (PeerJ)/PeerJ v3/SppList.csv", row.names = F) #export it as a csv file



# #Remove the NA
#  veg_data_summary<-na.omit(veg_data)
#  veg_data_summary<-as.data.frame(veg_data_summary)
#  summary(veg_data_summary)
#  str(veg_data_summary)

# can test with this: if it works, and gives you groups, then all is good. if not restart
# iris %>%
#   group_by(Species) %>%
#   summarize(meanSepLength=mean(Sepal.Length)) 

# Hacking my way around to count how many seedlings in each plot
# CORRECT WAY: 
# A<-na.omit(veg_data)
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
# its associated plot in the long form table.  You then convert all 1 to 0 -> plots with no plants were counted as a single row
# with an NA in it.  Ugly, but works
# select <- dplyr::select
# count.df<-select(veg_data,nest,location,spp.code,plot.id)
# count.df<-na.omit(count.df)
# count.df<-count.df %>%  group_by(nest, location) %>% dplyr::summarise(sdlg.no = n()) %>% ungroup()
# dim(count.df)

count.df<-avg_per_plot

# spp.no.df<-select(veg_data,nest,location,spp.code,plot.id)
# spp.no.df<-na.omit(spp.no.df)
# spp.no.df<-spp.no.df %>%  group_by(nest, location) %>% dplyr::summarise(spp.no = n_distinct(spp.code)) %>% ungroup()

str(count.df)
str(spp.no.df)
sdlgs<-full_join(count.df,spp.no.df, by=c("nest","location"))
dim(sdlgs)
sdlgs<-sdlgs %>% dplyr::rename(habitat=habitat.x, plot.id=plot.id.x,sdlg.no=N,spp.no=spp_N) %>% dplyr::select(-habitat.y,-plot.id.y)


# all.plots.df<-select(veg_data,nest,location,plot.id) %>% group_by(nest, location,plot.id) %>% dplyr::summarise(sdlg.no = n()) %>% select(-sdlg.no) %>% ungroup()
# dim(all.plots.df)
# sdlgs<-full_join(all.plots.df,sdlgs, by=c("nest", "location"))
# dim(sdlgs)
# 
# dim(nest_data) 



# BIND THEM UP....add % cover and nest area!!!
sdlgs.perc.cover<-dplyr::select(nest_data, plot.id, nest, location,perc.cover, nest.area)
sdlgs.perc.cover<-na.omit(sdlgs.perc.cover)
dim(sdlgs.perc.cover) #make sure same size as sdlgs dataframe
sdlgs.perc.cover$plot.id<-as.factor(sdlgs.perc.cover$plot.id)
sdlgs$plot.id<-as.factor(sdlgs$plot.id)
sdlgs<-full_join(sdlgs.perc.cover,sdlgs, by = "plot.id") %>% arrange(plot.id,nest.x,location.x) 
sdlgs<-dplyr::rename(sdlgs,nest=nest.x,location=location.x)
sdlgs<-sdlgs %>% dplyr::select(-nest.y,-location.y) 
#replace any NA in col of sdlg count with zero (those are true zeros)
# sdlgs$sdlg.no[is.na(sdlgs$sdlg.no)] <- 0
# sdlgs$spp.no[is.na(sdlgs$spp.no)] <- 0




# Still need?
# sdlgs<-na.omit(sdlgs)


dim(sdlgs)
dim(GLM.DATAwithsoil)
dim(GLM.DATA.nosoil)

str(sdlgs)
str(GLM.DATAwithsoil)
str(GLM.DATA.nosoil)

# WILL USE THE FOLLOWING TO DO GLMS W/ NO SOILS IN PCA (i.e., larger sample size)
sdlgs<-arrange(sdlgs,nest,location)
GLM.DATA.nosoil<-arrange(GLM.DATA.nosoil,nest.nosoil,location.nosoil)

sdlgs.nosoil<-inner_join(GLM.DATA.nosoil,sdlgs,by=c("nest","location"))
sdlgs.nosoil$perc.cover<-NULL
# 
# sdlgs.nosoil<-cbind(sdlgs, GLM.DATA.nosoil)
# sdlgs.nosoil$nest.nosoil<-NULL
# sdlgs.nosoil$cover.nosoil<-NULL
# sdlgs.nosoil$location.nosoil<-NULL

dim(sdlgs.nosoil)

# WILL USE THE FOLLOWING TO DO GLMS ***WITH*** SOILS IN PCA (i.e., larger sample size)
sdlgswithsoil<-filter(sdlgs, location =="far" | location == "nest")
# clean up column names and sort to do join of two dataframes of different sizes
# sdlgswithsoil$plot.id<-NULL
sdlgswithsoil$plot.id<-as.factor(sdlgswithsoil$plot.id)
GLM.DATAwithsoil$plot.id<-as.factor(GLM.DATAwithsoil$plot.id)
sdlgswithsoil<-full_join(sdlgswithsoil,GLM.DATAwithsoil, by = "plot.id")
sdlgswithsoil<-na.omit(sdlgswithsoil)
#NOW CHABGE NAMES BACK TO SIMPOLIFY THE ANALYSES BELOW
sdlgswithsoil$coverwithsoil<-NULL
names(sdlgswithsoil)[10]<-"cover2" #rename the column
sdlgswithsoil<-dplyr::rename(sdlgswithsoil, location=location.x, cover=perc.cover)
sdlgswithsoil<-dplyr::select(sdlgswithsoil, -location.y, -cover2)








############################################################################################################
## TABLE 4 GLMM: proximity to ant nests on environmental conditions (NO SOILS in PCA)
############################################################################################################

# Testing for correlation between canopy cover and axis scores of NO SOIL PCA 
DATA2<-droplevels(na.omit(sdlgs.nosoil))
sdlgs.nosoil$location<-factor(sdlgs.nosoil$location, levels=c("nest","adjacent", "far"), ordered=TRUE)
# DATA2<-dplyr::rename(DATA2,perc.cover=cover.nosoils)
#DATA3<-DATA2[!DATA2$location %in% c("adjacent"),] 
cor.test(sdlgs.nosoil$cover,sdlgs.nosoil$PCA1.nosoil)
cor.test(sdlgs.nosoil$cover,sdlgs.nosoil$PCA2.nosoil)


############################################################################################################
### TABLE 4: GLMM: proximity to ant nests on environbmental conditions (PCA NO SOILS DATA)
############################################################################################################


# Nest identity is a random effect RANDOM<-(1|nest)

#DOES INCLUDING COVARIATE 2 HELP?
pcaNOSOIL.nest<-lmer(PCA1.nosoil ~  (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.nest)
pcaNOSOIL.cover<-lmer(PCA1.nosoil ~ cover + (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.cover)
AIC(pcaNOSOIL.nest,pcaNOSOIL.cover) #mnodel with nest best fit than nest area
anova(pcaNOSOIL.nest, pcaNOSOIL.cover, test = "Chisq")
#YES!!! 

# Nest identity is a random effect RANDOM<-(1|nest)
# random effect only
pcaNOSOIL.1<-lmer(PCA1.nosoil ~ (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.1)
# effect of location + random
pcaNOSOIL.2<-lmer(PCA1.nosoil ~ location + (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.2)
# effect of cover + random
pcaNOSOIL.3<-lmer(PCA1.nosoil ~ cover + (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.3)
#effect of both + random (no interaction)
pcaNOSOIL.4<-lmer(PCA1.nosoil ~ location + cover + (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.4)
#effect of both, their interaction,and random
pcaNOSOIL.5<-lmer(PCA1.nosoil ~ location * cover + (1|nest), data = sdlgs.nosoil)
summary(pcaNOSOIL.5)
# pcaNOSOIL.6<-lmer(PCA1.nosoil ~ location + cover + nest.area + (1|nest), data = sdlgs.nosoil)
# summary(pcaNOSOIL.6)
# pcaNOSOIL.6<-lmer(PCA1.nosoil ~ location * cover + nest.area + (1|nest), data = sdlgs.nosoil)
# summary(pcaNOSOIL.6)
# pcaNOSOIL.7<-lmer(PCA1.nosoil ~ location * cover * nest.area + (1|nest), data = sdlgs.nosoil)
# summary(pcaNOSOIL.7)
# pcaNOSOIL.8<-lmer(PCA1.nosoil ~ location + cover * nest.area + (1|nest), data = sdlgs.nosoil)
# summary(pcaNOSOIL.8)



AIC(pcaNOSOIL.1,pcaNOSOIL.2,pcaNOSOIL.3,pcaNOSOIL.4,pcaNOSOIL.5)  #,pcaNOSOIL.6,pcaNOSOIL.7,pcaNOSOIL.8
anova(pcaNOSOIL.1,pcaNOSOIL.2, pcaNOSOIL.3,pcaNOSOIL.4,pcaNOSOIL.5,test = "Chisq") #,pcaNOSOIL.6,pcaNOSOIL.7,pcaNOSOIL.8

#Need table for models with
#random effect of nest ID only (1)
# effect of plot location + radnom
# effect of canopy cover + radnom
# effect of plot, canopy, random, but no interaction
# but no interaction with canopy cover covariate 
# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.pcaNOSOIL <- do.call(rbind, lapply(list(pcaNOSOIL.1, pcaNOSOIL.2,pcaNOSOIL.3, pcaNOSOIL.4, pcaNOSOIL.5), broom::glance)) #,pcaNOSOIL.6,pcaNOSOIL.7,pcaNOSOIL.8
summary.table.pcaNOSOIL[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.pcaNOSOIL <- summary.table.pcaNOSOIL[table.cols]
names(reported.table.pcaNOSOIL) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.pcaNOSOIL[['dAIC']] <-  with(reported.table.pcaNOSOIL, AIC - min(AIC))
reported.table.pcaNOSOIL[['wAIC']] <- with(reported.table.pcaNOSOIL, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.pcaNOSOIL$AIC <- NULL
reported.table.pcaNOSOIL <- reported.table.pcaNOSOIL[order(reported.table.pcaNOSOIL$dAIC),]
reported.table.pcaNOSOIL
write.csv(reported.table.pcaNOSOIL, file="./Output/Table4_NO SOIL.csv", row.names = F) #export it as a csv file



############################################################################################################
### TABLE 5 GLMM: proximity to ant nests on environmental conditions (PCA WITH SOILS)
############################################################################################################


# Testing for correlation between canopy cover and PCA axis scores
sdlgswithsoil<-droplevels(na.omit(sdlgswithsoil))
 cor.test(sdlgswithsoil$cover,sdlgswithsoil$PCA2withsoil)


# analyses

RESPONSE<-sdlgswithsoil$PCA1withsoil
FIXED<-sdlgswithsoil$location
#DOES INCLUDING NEST AREA IMPROVE THE FIT OVER JUST random effect of NEST?
pcaWITHSOIL.nest<-lmer(RESPONSE ~  (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.nest)


pcaWITHSOIL.cover<-lmer(RESPONSE ~ cover + (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.cover)

pcaWITHSOIL.nestarea<-lmer(RESPONSE ~ nest.area + (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.nestarea)

AIC(pcaWITHSOIL.nest, pcaWITHSOIL.cover, pcaWITHSOIL.nestarea)

anova(pcaWITHSOIL.cover,pcaWITHSOIL.nest, test = "Chisq")
anova(pcaWITHSOIL.nestarea,pcaWITHSOIL.nest, test = "Chisq")
anova(pcaWITHSOIL.cover, pcaWITHSOIL.nestarea,pcaWITHSOIL.nest, test = "Chisq")
#NO, SO DON't INCLUDE

# Nest identity is a random effect RANDOM<-(1|nest)
pcaWITHSOIL.1<-lmer(RESPONSE ~   (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.1)
pcaWITHSOIL.2<-lmer(RESPONSE ~ FIXED + (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.2)
pcaWITHSOIL.3<-lmer(RESPONSE ~ cover + (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.3)
pcaWITHSOIL.4<-lmer(RESPONSE ~ FIXED + cover + (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.4)
pcaWITHSOIL.5<-lmer(RESPONSE ~ FIXED * cover + (1|nest), data = sdlgswithsoil)
summary(pcaWITHSOIL.5)
# pcaWITHSOIL.6<-lmer(RESPONSE ~ FIXED + cover +  nest.area + (1|nest), data = sdlgswithsoil)
# summary(pcaWITHSOIL.6)
# pcaWITHSOIL.7<-lmer(RESPONSE ~ FIXED * cover *  nest.area + (1|nest), data = sdlgswithsoil)
# summary(pcaWITHSOIL.7)
# pcaWITHSOIL.8<-lmer(RESPONSE ~ FIXED + nest.area *  cover + (1|nest), data = sdlgswithsoil)
# summary(pcaWITHSOIL.8)

AIC(pcaWITHSOIL.5,pcaWITHSOIL.4,pcaWITHSOIL.3,pcaWITHSOIL.2, pcaWITHSOIL.1) #pcaWITHSOIL.8,pcaWITHSOIL.7,pcaWITHSOIL.6,
anova(pcaWITHSOIL.5,pcaWITHSOIL.4,pcaWITHSOIL.3,pcaWITHSOIL.2, pcaWITHSOIL.1, test = "Chisq") #pcaWITHSOIL.8,pcaWITHSOIL.7,pcaWITHSOIL.6,


#Need table for models with
#random effect of nest ID only (1)
# effect of plot location + radnom
# effect of canopy cover + radnom
# effect of plot, canopy, random, but no interaction
# but no interaction with canopy cover covariate 
# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.pcaWITHSOIL <- do.call(rbind, lapply(list(pcaWITHSOIL.1, pcaWITHSOIL.2,pcaWITHSOIL.3, pcaWITHSOIL.4, pcaWITHSOIL.5), broom::glance)) #,pcaWITHSOIL.6,pcaWITHSOIL.7,pcaWITHSOIL.8
summary.table.pcaWITHSOIL[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.pcaWITHSOIL <- summary.table.pcaWITHSOIL[table.cols]
names(reported.table.pcaWITHSOIL) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.pcaWITHSOIL[['dAIC']] <-  with(reported.table.pcaWITHSOIL, AIC - min(AIC))
reported.table.pcaWITHSOIL[['wAIC']] <- with(reported.table.pcaWITHSOIL, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.pcaWITHSOIL$AIC <- NULL
reported.table.pcaWITHSOIL <- reported.table.pcaWITHSOIL[order(reported.table.pcaWITHSOIL$dAIC),]
reported.table.pcaWITHSOIL 
write.csv(reported.table.pcaWITHSOIL, file="./Output/Table5_PCAWITHSOIL_TABLE5.csv", row.names = F) #export it as a csv file


############################################################################################################
### GLMMs: spp richness and sdlg abundnace vs plot proximity to nests, environmental conditions
############################################################################################################

# Nice overview of GLMs here: http://plantecology.syr.edu/fridley/bio793/glm.html
#  Need to consider nest a random effect
# below is based on Grueber et al. 2011. J Evol Biol. 24:699-711.  
# Another good source is http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
# for more on lmer/glmer error message:
# http://stats.stackexchange.com/questions/35841/using-glmer-to-replicate-result-from-lmer-for-mulitlevel-modelling-in-r

############################################################################################################
# SEEDLING NUMBER VS PCA **NO SOIL** TABLE 6A 
############################################################################################################

DATA<-sdlgs.nosoil
# PCA AXIS 1 or 2?
PCA<-DATA$PCA1.nosoil
# PCA<-DATA$PCA2.nosoil
# Seedling No or Spp richness?
RESPONSE<-sdlgs.nosoil$sdlg.no
# WHAT FIXED (main) EFFECT?
FIXED<-DATA$location #Plot location: on nests, adjacent to nests, or far from nests
# Nest identity is a random effect RANDOM<-(1|nest)
# distribution family: poisson for starters because both are counts
# may have to use quasi-poisson die to overdispersion

#DOES IT HELP TO INCLUDE NEST AREA?
global.cov3<-glmer(RESPONSE ~ nest.area + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3)

global.nest<-glmer(RESPONSE ~  (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.nest)

AIC(global.cov3,global.nest)
anova(global.cov3,global.nest, test = "Chisq")
# Including nest area doesn't improve fit

global.cov3.1<-glmer(RESPONSE ~ PCA + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.1)
anova(global.cov3.1,global.nest, test = "Chisq")

global.cov3.2<-glmer(RESPONSE ~ cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.2)
anova(global.cov3.2,global.nest, test = "Chisq")

AIC(global.cov3.1,global.cov3.2,global.nest)
# including PCA improves fit over random 
# 
# global.model<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
# summary(global.model)



# testing for overdipsersion: http://glmm.wikidot.com/faq, Section "How can I deal with overdispersion in GLMMs?"
source("overdisp_fun.R")
overdisp_fun(global.model)

# #from http://glmm.wdfiles.com/local--files/examples/Owls.pdf
# Check for overdispersion (Pearson residuals):
rdev <- sum(residuals(global.model)^2)
mdf <- length(fixef(global.model))
rdf <- length(unique(DATA$nest))-mdf ## residual df [NOT accounting for random effects]
rdev/rdf
# #Overdispersion is quite a bit if > 1 . . . significance test:
(prob.disp <- pchisq(rdev,rdf,lower.tail=FALSE,log.p=TRUE)) #This is a log probability, so if result was  -868.796 could correspond to p ≈ 10−377.)
# Here (with a hacked version of lme4 that allows per-observation random effects, i.e. a Poisson-lognormal distribution):
DATA$obs <- 1:nrow(DATA) ## add observation number to data
# global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + nest.area + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
# global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")

# ALternative for checking onversion https://stats.stackexchange.com/questions/110004/how-scared-should-we-be-about-convergence-warnings-in-lme4
relgrad <- with(global.model.2@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

print(summary(global.model.2))

# stdz.model<-standardize(global.model, standardize.y=FALSE)
# model.set<-dredge(stdz.model)
model.set<-dredge(global.model.2)
top.models<-get.models(model.set, subset=delta<2)
summary(top.models)



#random effects 
global.model1 <- glmer(RESPONSE ~ FIXED + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model1)
# cover gradient + random
global.model2 <- glmer(RESPONSE ~ FIXED + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model2)
anova(global.model1,global.model2, test = "Chisq")
# ant-related + random
global.model3<-glmer(RESPONSE ~ FIXED + PCA + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model3)
# BOTH of gradient and ant
global.model4<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model4)
# INTERACTON of gradient and ant
global.model5<-glmer(RESPONSE ~ FIXED + PCA * cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model5)



# Nest identity is a random effect RANDOM<-(1|nest)
AIC(global.model1,global.model2,global.model3,global.model4,global.model5)
anova(global.model1,global.model2,global.model3,global.model4,global.model5, test = "Chisq")

# 1: random effects 
# 2: gradient + random
# 3: ant-related + random
# 4: Interaction of gradient and ant

# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.global <- do.call(rbind, lapply(list(global.model1,global.model2,global.model3,global.model4,global.model5), broom::glance))
summary.table.global[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.global <- summary.table.global[table.cols]
names(reported.table.global) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.global[['dAIC']] <-  with(reported.table.global, AIC - min(AIC))
reported.table.global[['wAIC']] <- with(reported.table.global, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.global$AIC <- NULL
reported.table.global6 <- reported.table.global[order(reported.table.global$dAIC),]
round (reported.table.global6, digits = 4)
reported.table.global6

### TABLE 6A: Seedling abundance  vs PCA NO SOILS
write.csv(reported.table.global6, file="./Output/global_6A.csv", row.names = F) #export it as a csv file
############################################################################################################

############################################################################################################
# SEEDLING SPECIES RICHNESS VS PCA **NO SOIL** TABLE 6B 
############################################################################################################

DATA<-sdlgs.nosoil
# PCA AXIS 1 or 2?
PCA<-DATA$PCA1.nosoil
# PCA<-DATA$PCA2.nosoil
# 
RESPONSE<-DATA$spp.no
# WHAT FIXED (main) EFFECT?
FIXED<-DATA$location #Plot location: on nests, adjacent to nests, or far from nests

# Nest identity is a random effect RANDOM<-(1|nest)
# distribution family: poisson for starters because both are counts
# may have to use quasi-poisson die to overdispersion

#DOES IT HELP TO INCLUDE NEST AREA?
global.cov3<-glmer(RESPONSE ~ nest.area + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3)

global.nest<-glmer(RESPONSE ~  (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.nest)

AIC(global.cov3,global.nest)
anova(global.cov3,global.nest, test = "Chisq")
# no improvement in fit by adding nest area

global.cov3.1<-glmer(RESPONSE ~ PCA + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.1)
anova(global.cov3.1,global.nest, test = "Chisq")

global.cov3.2<-glmer(RESPONSE ~ cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.2)
anova(global.cov3.2,global.nest, test = "Chisq")

AIC(global.cov3.1,global.cov3.2,global.nest)

# including cover or and pca improve fit over random 

global.model<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model)



# testing for overdipsersion: http://glmm.wikidot.com/faq, Section "How can I deal with overdispersion in GLMMs?"
source("overdisp_fun.R")
overdisp_fun(global.model)

# #from http://glmm.wdfiles.com/local--files/examples/Owls.pdf
# Check for overdispersion (Pearson residuals):
rdev <- sum(residuals(global.model)^2)
mdf <- length(fixef(global.model))
rdf <- length(unique(DATA$nest))-mdf ## residual df [NOT accounting for random effects]
rdev/rdf
# #Overdispersion is quite a bit if > 1 . . . significance test:
(prob.disp <- pchisq(rdev,rdf,lower.tail=FALSE,log.p=TRUE)) #This is a log probability, so if result was  -868.796 could correspond to p ≈ 10−377.)
# Here (with a hacked version of lme4 that allows per-observation random effects, i.e. a Poisson-lognormal distribution):
DATA$obs <- 1:nrow(DATA) ## add observation number to data
# global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + nest.area + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")

print(summary(global.model.2))

# stdz.model<-standardize(global.model, standardize.y=FALSE)
# model.set<-dredge(stdz.model)
model.set<-dredge(global.model.2)
top.models<-get.models(model.set, subset=delta<2)
summary(top.models)



#random effects 
global.model1 <- glmer(RESPONSE ~ FIXED + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model1)
# cover gradient + random
global.model2 <- glmer(RESPONSE ~ FIXED + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model2)
anova(global.model1,global.model2, test = "Chisq")
# ant-related + random
global.model3<-glmer(RESPONSE ~ FIXED + PCA + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model3)
# BOTH of gradient and ant
global.model4<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model4)
# INTERACTON of gradient and ant
global.model5<-glmer(RESPONSE ~ FIXED + PCA * cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model5)



# Nest identity is a random effect RANDOM<-(1|nest)
AIC(global.model1,global.model2,global.model3,global.model4,global.model5)
anova(global.model1,global.model2,global.model3,global.model4,global.model5, test = "Chisq")

# 1: random effects 
# 2: gradient + random
# 3: ant-related + random
# 4: Interaction of gradient and ant

# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.global <- do.call(rbind, lapply(list(global.model1,global.model2,global.model3,global.model4,global.model5), broom::glance))
summary.table.global[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.global <- summary.table.global[table.cols]
names(reported.table.global) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.global[['dAIC']] <-  with(reported.table.global, AIC - min(AIC))
reported.table.global[['wAIC']] <- with(reported.table.global, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.global$AIC <- NULL
reported.table.global6 <- reported.table.global[order(reported.table.global$dAIC),]
round (reported.table.global6, digits = 4)
reported.table.global6

# TABLE 6B: Seedling  richness vs PCA NO SOILS
write.csv(reported.table.global6, file="./Output/global_6B.csv", row.names = F) #export it as a csv file
############################################################################################################

############################################################################################################
# SEEDLING NUMBER OR SPECIES RICHNESS VS PCA **WITH SOIL** TABLE 7A+B 
############################################################################################################
DATA<-sdlgswithsoil
# PCA AXIS 1 or 2?
PCA<-DATA$PCA1withsoil
# PCA<-DATA$PCA2withsoil
RESPONSE<-DATA$sdlg.no
# WHAT FIXED (main) EFFECT?
FIXED<-DATA$location #Plot location: on nests, adjacent to nests, or far from nests
# Nest identity is a random effect RANDOM<-(1|nest)
# distribution family: poisson for starters because both are counts
# may have to use quasi-poisson die to overdispersion

#DOES IT HELP TO INCLUDE NEST AREA?
global.cov3<-glmer(RESPONSE ~ nest.area + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3)

global.nest<-glmer(RESPONSE ~  (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.nest)

AIC(global.cov3,global.nest)
anova(global.cov3,global.nest, test = "Chisq")

#No improvement by adding nest area

global.cov3.1<-glmer(RESPONSE ~ PCA + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.1)
anova(global.cov3.1,global.nest, test = "Chisq")

global.cov3.2<-glmer(RESPONSE ~ cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.2)
anova(global.cov3.2,global.nest, test = "Chisq")

AIC(global.cov3.1,global.cov3.2,global.nest)
# Adding PCA helps iomprove fit over random 

global.model<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model)



# testing for overdipsersion: http://glmm.wikidot.com/faq, Section "How can I deal with overdispersion in GLMMs?"
source("overdisp_fun.R")
overdisp_fun(global.model)

# #from http://glmm.wdfiles.com/local--files/examples/Owls.pdf
# Check for overdispersion (Pearson residuals):
rdev <- sum(residuals(global.model)^2)
mdf <- length(fixef(global.model))
rdf <- length(unique(DATA$nest))-mdf ## residual df [NOT accounting for random effects]
rdev/rdf
# #Overdispersion is quite a bit if > 1 . . . significance test:
(prob.disp <- pchisq(rdev,rdf,lower.tail=FALSE,log.p=TRUE)) #This is a log probability, so if result was  -868.796 could correspond to p ≈ 10−377.)
# Here (with a hacked version of lme4 that allows per-observation random effects, i.e. a Poisson-lognormal distribution):
DATA$obs <- 1:nrow(DATA) ## add observation number to data
# global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + nest.area + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")

print(summary(global.model.2))

# stdz.model<-standardize(global.model, standardize.y=FALSE)
# model.set<-dredge(stdz.model)
model.set<-dredge(global.model.2)
top.models<-get.models(model.set, subset=delta<2)
summary(top.models)



#random effects 
global.model1 <- glmer(RESPONSE ~ FIXED + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model1)
# cover gradient + random
global.model2 <- glmer(RESPONSE ~ FIXED + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model2)
anova(global.model1,global.model2, test = "Chisq")
# ant-related + random
global.model3<-glmer(RESPONSE ~ FIXED + PCA + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model3)
# BOTH of gradient and ant
global.model4<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model4)
# INTERACTON of gradient and ant
global.model5<-glmer(RESPONSE ~ FIXED + PCA * cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model5)



# Nest identity is a random effect RANDOM<-(1|nest)
AIC(global.model1,global.model2,global.model3,global.model4,global.model5)
anova(global.model1,global.model2,global.model3,global.model4,global.model5, test = "Chisq")

# 1: random effects 
# 2: gradient + random
# 3: ant-related + random
# 4: Interaction of gradient and ant

# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.global <- do.call(rbind, lapply(list(global.model1,global.model2,global.model3,global.model4,global.model5), broom::glance))
summary.table.global[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.global <- summary.table.global[table.cols]
names(reported.table.global) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.global[['dAIC']] <-  with(reported.table.global, AIC - min(AIC))
reported.table.global[['wAIC']] <- with(reported.table.global, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.global$AIC <- NULL
reported.table.global6 <- reported.table.global[order(reported.table.global$dAIC),]
round (reported.table.global6, digits = 4)

### TABLE 7A: Seedling abundance  vs PCA WITH SOILS
############################################################################################################
write.csv(reported.table.global6, file="./Output/global_7A.csv", row.names = F) #export it as a csv file




############################################################################################################
# SEEDLING SPECIES RICHNESS VS PCA **WITH SOIL** TABLE 7B 
############################################################################################################
DATA<-sdlgswithsoil
# PCA AXIS 1 or 2?
PCA<-DATA$PCA1withsoil
# PCA<-DATA$PCA2withsoil
RESPONSE<-DATA$spp.no
# WHAT FIXED (main) EFFECT?
FIXED<-DATA$location #Plot location: on nests, adjacent to nests, or far from nests
# Nest identity is a random effect RANDOM<-(1|nest)
# distribution family: poisson for starters because both are counts
# may have to use quasi-poisson die to overdispersion

#DOES IT HELP TO INCLUDE NEST AREA?
global.cov3<-glmer(RESPONSE ~ nest.area + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3)

global.nest<-glmer(RESPONSE ~  (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.nest)

AIC(global.cov3,global.nest)
anova(global.cov3,global.nest, test = "Chisq")

global.cov3.1<-glmer(RESPONSE ~ PCA + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.1)
anova(global.cov3.1,global.nest, test = "Chisq")

global.cov3.2<-glmer(RESPONSE ~ cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3.2)
anova(global.cov3.2,global.nest, test = "Chisq")

AIC(global.cov3,global.cov3.1,global.cov3.2,global.nest)

# including nest area, cover, and pca (indiv) improves fit over random 

global.model<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model)



# testing for overdipsersion: http://glmm.wikidot.com/faq, Section "How can I deal with overdispersion in GLMMs?"
source("overdisp_fun.R")
overdisp_fun(global.model)

# #from http://glmm.wdfiles.com/local--files/examples/Owls.pdf
# Check for overdispersion (Pearson residuals):
rdev <- sum(residuals(global.model)^2)
mdf <- length(fixef(global.model))
rdf <- length(unique(DATA$nest))-mdf ## residual df [NOT accounting for random effects]
rdev/rdf
# #Overdispersion is quite a bit if > 1 . . . significance test:
(prob.disp <- pchisq(rdev,rdf,lower.tail=FALSE,log.p=TRUE)) #This is a log probability, so if result was  -868.796 could correspond to p ≈ 10−377.)
# Here (with a hacked version of lme4 that allows per-observation random effects, i.e. a Poisson-lognormal distribution):
DATA$obs <- 1:nrow(DATA) ## add observation number to data
# global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + nest.area + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
global.model.2 <- glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")

print(summary(global.model.2))

# stdz.model<-standardize(global.model, standardize.y=FALSE)
# model.set<-dredge(stdz.model)
model.set<-dredge(global.model.2)
top.models<-get.models(model.set, subset=delta<2)
summary(top.models)



#random effects 
global.model1 <- glmer(RESPONSE ~ FIXED + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model1)
# cover gradient + random
global.model2 <- glmer(RESPONSE ~ FIXED + cover + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model2)
anova(global.model1,global.model2, test = "Chisq")
# ant-related + random
global.model3<-glmer(RESPONSE ~ FIXED + PCA + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model3)
# BOTH of gradient and ant
global.model4<-glmer(RESPONSE ~ FIXED + PCA + cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model4)
# INTERACTON of gradient and ant
global.model5<-glmer(RESPONSE ~ FIXED + PCA * cover + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model5)



# Nest identity is a random effect RANDOM<-(1|nest)
AIC(global.model1,global.model2,global.model3,global.model4,global.model5)
anova(global.model1,global.model2,global.model3,global.model4,global.model5, test = "Chisq")

# 1: random effects 
# 2: gradient + random
# 3: ant-related + random
# 4: Interaction of gradient and ant

# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.global <- do.call(rbind, lapply(list(global.model1,global.model2,global.model3,global.model4,global.model5), broom::glance))
summary.table.global[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.global <- summary.table.global[table.cols]
names(reported.table.global) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.global[['dAIC']] <-  with(reported.table.global, AIC - min(AIC))
reported.table.global[['wAIC']] <- with(reported.table.global, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.global$AIC <- NULL
reported.table.global6 <- reported.table.global[order(reported.table.global$dAIC),]
round (reported.table.global6, digits = 4)



### TABLE 7B: Seedling richness vs PCA WITH SOILS
############################################################################################################
write.csv(reported.table.global6, file="./Output/global_7B.csv", row.names = F) #export it as a csv file
























############################################################################################################
### FIG 1A: Histogram of Canopy Cover Gradient
############################################################################################################

cover.fig.gradient<-ggplot(nest_data, aes(x=perc.cover, fill=habitat)) +
  geom_histogram(binwidth=10, alpha=.7, position="identity", colour="black")+
  scale_fill_grey(start=0.3, end=1, labels = c("Cerrado denso","Cerrado ralo"))+
  ylab("No. of plots") + 
  xlab("Canopy cover (%)")+
  annotate ("text", x=1.5, y=12, label="A", fontface="bold", size=8, color="black")+
  guides(fill = guide_legend(nrow=2,byrow=TRUE, override.aes = list(colour = NULL))) #remove slash from legend

cover.fig.gradient<-cover.fig.gradient + scale_y_continuous(breaks = seq(0, 16, 2))
cover.fig.gradient<- cover.fig.gradient + theme_classic()+theme(plot.title = element_text(face="bold", size=20),                            #Sets title size, style, location
                                                                # legend.position=c(0.5,0.95),
                                                                axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
                                                                axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                                                axis.title.x=element_text(colour="black", size = 20, vjust=-0.5),           #Sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                                axis.title.y=element_text(colour="black", size = 20, vjust=1.5),            #Sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                                axis.text=element_text(colour="black", size = 16),                          #Sets size and style of labels on axes
                                                                legend.title = element_blank(),                                             #Removes the Legend title
                                                                #legend.key = element_blank(),                                              #Removes the boxes around legend colors
                                                                legend.text = element_text(face="italic", color="black", size=16),
                                                                # legend.position = "top",
                                                                # legend.direction = 'horizontal', 
                                                                legend.key = element_rect(colour = "black"))                                #puts black line around legend box

cover.fig.gradient                                                       



############################################################################################################
### Figure 1B: Canopy cover over plots at different proximities to ant nests
############################################################################################################

# Fig 1B Canopy cover for each plot split by nest 
cover.fig.location<-ggplot(data=DATA, aes(x=location, y=cover, group=nest)) +
  geom_line(size=0.5) + geom_point(size=4, aes(colour=location, shape=location))+
  ylab("Canopy cover (%)")+
  xlab("Plot location")+ 
  scale_y_continuous(limit=c(0, 100))+
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  annotate ("text", x=0.7, y=95, label="B", fontface="bold", size=8, color="black")

cover.fig.location<-cover.fig.location + theme_classic() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                                 axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
                                                                 axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                                 plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=22),        #Sets title size, style, location
                                                                 axis.title.x=element_text(colour="black", size = 20, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                                 axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                                 legend.position = "none",
                                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                                 plot.margin = unit(c(1,3,2,1), "cm"))
cover.fig.location


######################################################
# BINDING THESE UP TO MAKE FIGURE 
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")

Fig1<-multiplot(cover.fig.gradient,cover.fig.location, cols=1)

ggsave("./Output/Fig1.eps", plot = multiplot(cover.fig.gradient,cover.fig.location, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)


#or as 2 sep figs 
ggsave("./Output/Fig1A.tiff", cover.fig.gradient, device = "tiff", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)
ggsave("./Output/Fig1B.eps", cover.fig.location, device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)


######################################################




############################################################################################################
###   FIG 2A - PCA NO SOILS (ALL PLOTS)
############################################################################################################

# The list used to make the figure is uses the column names (i.e., grass.biomass). The next three lines
# Will replace these names with those you actually want to appear in the figure.  
# Note this doesn't change the names in the original dataframe
# ?ggbiplot for more info on arguments you can change and % used to draw ellipses aroudnd points

nest.env.pca.nosoil$rotation
dimnames(nest.env.pca.nosoil$rotation)
dimnames(nest.env.pca.nosoil$rotation)[[1]]<-c("litter", "soil penet.", "grass", "soil moisture")

point.size<-cover.nosoils*6

g_NOsoils <- ggbiplot(nest.env.pca.nosoil, obs.scale = 1, var.scale = 1, 
                      group = location.nosoil, ellipse = TRUE, alpha=0,
                      circle = FALSE, varname.size=6, varname.adjust=1.2)+
  geom_point(aes(color=location.nosoil, size = point.size, shape=location.nosoil), alpha=0.7) + 
  scale_shape_manual(values=c(16,17,15), guide=FALSE)+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"), guide=FALSE)+
  annotate ("text", x=-3, y=3.5, label="A", fontface="bold", size=8, color="black")
#geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06


g_NOsoils<-g_NOsoils + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4,4)) # I adjusted Y axis so that I could read the larger labels on arrows
g_NOsoils<-g_NOsoils + scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4,4)) # I adjusted Y axis so that I could read the larger labels on arrows

g_NOsoils <-g_NOsoils + theme_classic()+theme(legend.direction = 'horizontal', 
                                              legend.position = 'top',
                                              axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
                                              axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                              axis.title.x=element_text(colour="black", size = 20, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                              axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                              axis.text=element_text(colour="black", size = 18),                             #sets size and style of labels on axes
                                              legend.title = element_blank(), #remove title of legend
                                              legend.text = element_text(color="black", size=16, vjust =2),
                                              plot.margin =unit(c(1,1,1,1.5), "lines"))   #plot margin - top, right, bottom, left

g_NOsoils <-g_NOsoils +guides(shape=guide_legend(override.aes=list(size=6, linetype=0, color=c("#000066","#0072B2","#666666")))) #size of legen bars    
g_NOsoils <-g_NOsoils + guides(size="none", colour="none")
print(g_NOsoils)





############################################################################################################
###   FIG 2B - With Soils (SOILS, ONLY "ON" and "FAR" PLOTS
############################################################################################################


# The list used to make the figure is uses the column names (i.e., grass.biomass). The next three lines
# Will replace these names with those you actually want to appear in the figure.  
# Note this doesn't change the names in the original dataframe
# ?ggbiplot for more info on arguments you can change and % used to draw ellipses aroudnd points

nest.env.with.soilchem$rotation
dimnames(nest.env.with.soilchem$rotation)
dimnames(nest.env.with.soilchem$rotation)[[1]]<-c("litter", "soil penet.", "grass", 
                                            "pH", "P", "K", "Ca","Mg","Al","org mat","soil moisture")

point.size<-coverwithsoil*6
# Note on making the figure. In the original attempt the legend of the arrows was always under the points, making it tough to read. 
# This is because the geom_point layer with the adjusted point sizes was placed on top of the original plot.
# to fix this, make the points completely transparent in the original plot by setting alpha to 0, then makle the pooints on the 
# geom_plot layer semin transparent.
g_soils <- ggbiplot(nest.env.with.soilchem, obs.scale = 1, var.scale = 1, alpha=0,
                    group = locationwithsoil, ellipse = TRUE, 
                    circle = FALSE, varname.size=6, varname.adjust=1.2)+
  geom_point(aes(color=locationwithsoil, size = point.size, shape=locationwithsoil), alpha=0.7) + 
  scale_shape_manual(values=c(16,15), guide=FALSE)+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#666666"), guide=FALSE)+
  annotate ("text", x=-3, y=3.5, label="B", fontface=
              "bold", size=8, color="black")
# geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06
# geom_point(aes(color=locationwithsoil, size = point.size)) + scale_size_identity()
# I chose my own colors for the lines
g_soils<-g_soils + scale_x_continuous(breaks = seq(-4, 4, 2), limits = c(-4, 4)) # I adjusted X axis so that I could read the larger labels on arrows
g_soils<-g_soils + scale_y_continuous(breaks = seq(-4, 4, 2), limits = c(-4,4)) # I adjusted Y axis so that I could read the larger labels on arrows

g_soils <-g_soils + theme_classic()+theme(legend.direction = 'horizontal', 
                                          legend.position = 'top',
                                          axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
                                          axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                          axis.title.x=element_text(colour="black", size = 20, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.text=element_text(colour="black", size = 18),                             #sets size and style of labels on axes
                                          legend.title = element_blank(), #remove title of legend
                                          legend.text = element_text(color="black", size=16, vjust =2),
                                          plot.margin =unit(c(1,1,1,1.5), "lines"))   #plot margin - top, right, bottom, left

g_soils <-g_soils +guides(shape=guide_legend(override.aes=list(size=6, linetype=0, color=c("#000066","#666666")))) #size of legen bars    
g_soils <-g_soils + guides(size="none", colour="none")
print(g_soils)



######################################################
# BINDING THESE UP TO MAKE FIGURE 
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")

Fig2<-multiplot(g_NOsoils,g_soils, cols=1)
ggsave("./Output/Fig2.eps", plot = multiplot(g_NOsoils,g_soils, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)
#Sep
ggsave("./Output/Fig2A.tif", g_NOsoils, device = "tiff", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)
ggsave("./Output/Fig2B.tif", g_soils, device = "tiff", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)




######################################################



############################################################################################################
# Figure 3A: PCA-NO-soils DATA vs. canopy cover 
############################################################################################################

CoverEnvAll<-ggplot(DATA2, aes(x = cover, y = PCA1.nosoil, col=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Axis 1 score") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE) # Add linear regression lines
CoverEnvAll<-CoverEnvAll + annotate ("text", x=5, y=2, label="A", fontface="bold", size=8, color="black")
CoverEnvAll<-CoverEnvAll + scale_colour_manual(values=c("#000066","#0072B2","#666666"))  #I chose my own colors for the lines
#plot.sdlg.no<-plot.sdlg.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
CoverEnvAll<-CoverEnvAll + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
CoverEnvAll<- CoverEnvAll + theme_classic()+
  theme(#plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
    axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
    axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
    axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
    axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
    legend.position = 'top',
    legend.title = element_blank(),   #Removes the Legend title
    legend.text = element_text(color="black", size=16),  
    # legend.position = c(0.15,0.75),
    legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
    plot.margin =unit(c(0,1,1,1.5), "cm")) #+  #plot margin - top, right, bottom, left
CoverEnvAll


############################################################################################################
# Fig 3B: PCA-with-soils vs. canopy cover 
############################################################################################################
CoverEnv<-ggplot(sdlgswithsoil, aes(x = cover, y = PCA1withsoil, col=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,15))+
  ylab("Axis 1 score") +
  xlab("Canopy cover (%)")+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines
CoverEnv<-CoverEnv + scale_colour_manual(values=c("#000066", "#666666"))  #I chose my own colors for the lines
CoverEnv<-CoverEnv +annotate ("text", x=5, y=4, label="B", fontface="bold", size=8, color="black")
#plot.sdlg.no<-plot.sdlg.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
CoverEnv<-CoverEnv + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
CoverEnv<-CoverEnv+scale_y_continuous(breaks = seq(-4, 4, 1), limits = c(-4, 4))
CoverEnv<- CoverEnv + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=25, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        # legend.position = c(0.1,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,1,1.8), "cm")) #+  #plot margin - top, right, bottom, left
CoverEnv




######################################################
# BINDING THESE UP TO MAKE FIGURE APPENDIX
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")

Fig3<-multiplot(CoverEnvAll,CoverEnv, cols=1)
ggsave("./Output/Fig3.eps", plot = multiplot(CoverEnvAll,CoverEnv, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

######################################################



############################################################################################################
# Fig 4A: PLOT OF SEEDLING ABUNDNACE VS canopy cover 
############################################################################################################

canopy.sdlgs.fig1<-ggplot(sdlgs.nosoil, aes(x = PCA1.nosoil, y = sdlg.no, colour=location, shape=location, fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("seedling abundance") +
  xlab("PCA-1 Axis 1 Score")+
  geom_smooth(method=lm,se=FALSE)+ 
  annotate ("text", x=2.8, y=85, label="A", fontface="bold", size=8, color="black")
#scale_colour_hue(l=50) + # Use a slightly darker palette than normal
# Add linear regression lines
canopy.sdlgs.fig1<-canopy.sdlgs.fig1 + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(-1, 90))
canopy.sdlgs.fig1<-canopy.sdlgs.fig1 + scale_x_continuous(breaks = seq(-3.5, 3.5, 1), limits = c(-3.5, 3.5))
# canopy.sdlgs.fig1<-canopy.sdlgs.fig1 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
canopy.sdlgs.fig1<- canopy.sdlgs.fig1 + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-15, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        # legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,1,1.5), "cm")) #+  #plot margin - top, right, bottom, left
canopy.sdlgs.fig1


############################################################################################################
# Fig 4B: PLOT OF SEEDLING RICHNESS VS canopy cover
############################################################################################################

canopy.sdlgs.fig2<-ggplot(sdlgs.nosoil, aes(x = PCA1.nosoil , y = spp.no, colour=location, shape=location, fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Species richness") +
  xlab("PCA-1 Axis 1 Score")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=2.8, y=30, label="C", fontface="bold", size=8, color="black")
# canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_y_continuous(breaks = seq(0, 30, 5), limits = c(-5, 30))
# canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_y_continuous(breaks = seq(0, 30, 5), limits = c(-1, 31))
canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_x_continuous(breaks = seq(-3.5, 3.5, 1), limits = c(-3.5, 3.5))
canopy.sdlgs.fig2<- canopy.sdlgs.fig2 + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        legend.position='top',
        # legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,1,1.5), "cm")) #+  #plot margin - top, right, bottom, left
canopy.sdlgs.fig2



######################################################
# BINDING THESE UP TO MAKE FIGURE APPENDIX
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")

Fig4<-multiplot(canopy.sdlgs.fig1,canopy.sdlgs.fig2, cols=1)
ggsave("./Output/Fig4_18aug.eps", plot = multiplot(canopy.sdlgs.fig1,canopy.sdlgs.fig2, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

######################################################






############################################################################################################
# Fig 5A: PLOT OF SEEDLING ABUNDNACE VS canopy cover 
############################################################################################################

canopy.sdlgs.fig5<-ggplot(sdlgs.nosoil, aes(x = cover, y = sdlg.no, colour=location, shape=location, fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("seedling abundance") +
  xlab("Canopy Cover (%)")+
  geom_smooth(method=lm,se=FALSE)+ 
  annotate ("text", x=90, y=90, label="B", fontface="bold", size=8, color="black")
#scale_colour_hue(l=50) + # Use a slightly darker palette than normal
# Add linear regression lines
canopy.sdlgs.fig5<-canopy.sdlgs.fig5 + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(-1, 90))
canopy.sdlgs.fig5<-canopy.sdlgs.fig5 + scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100))
# canopy.sdlgs.fig1<-canopy.sdlgs.fig1 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
canopy.sdlgs.fig5<- canopy.sdlgs.fig5 + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-15, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        # legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,1,1.5), "cm")) #+  #plot margin - top, right, bottom, left
canopy.sdlgs.fig5


############################################################################################################
# Fig 4B: PLOT OF SEEDLING RICHNESS VS canopy cover
############################################################################################################

canopy.sdlgs.fig6<-ggplot(sdlgs.nosoil, aes(x = cover , y = spp.no, colour=location, shape=location, fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Species richness") +
  xlab("Canopy Cover (%)")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=90, y=30, label="D", fontface="bold", size=8, color="black")
# canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_y_continuous(breaks = seq(0, 30, 5), limits = c(-5, 30))
# canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
canopy.sdlgs.fig6<-canopy.sdlgs.fig6 + scale_y_continuous(breaks = seq(0, 30, 5), limits = c(-1, 31))
canopy.sdlgs.fig6<-canopy.sdlgs.fig6 + scale_x_continuous(breaks = seq(0, 100, 20), limits = c(0, 100))
canopy.sdlgs.fig6<- canopy.sdlgs.fig6 + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        legend.position='top',
        # legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,1,1.5), "cm")) #+  #plot margin - top, right, bottom, left
canopy.sdlgs.fig6



######################################################
# BINDING THESE UP TO MAKE FIGURE APPENDIX
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")

Fig5<-multiplot(canopy.sdlgs.fig5,canopy.sdlgs.fig6, cols=1)
ggsave("./Output/Fig5_18aug.eps", plot = multiplot(canopy.sdlgs.fig1,canopy.sdlgs.fig2, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)


Fig6<-multiplot(canopy.sdlgs.fig1,canopy.sdlgs.fig2,canopy.sdlgs.fig5,canopy.sdlgs.fig6, cols=2)
ggsave("./Output/Fig6_18aug.eps", plot = multiplot(canopy.sdlgs.fig1,canopy.sdlgs.fig2,canopy.sdlgs.fig5,canopy.sdlgs.fig6, cols=2), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)



######################################################











############################################################################################################
### APPENDIX B: Correlations between environmental variables
############################################################################################################

lb<-nest_data$litter.bmass
sp<-nest_data$soil.pen
gb<-nest_data$grass.bmass
sh<-nest_data$soil.moisture.surface
pc<-nest_data$cover

# If you want to see the histograms...
# hist(lb)
# hist(sp)
# hist(gb)
# hist(sh)
# hist(pc)

####CORRELATIONS BTWEN ENVT VARIABLES (NON SOIL) 
cor.test(lb, sp, method="spearman")
cor.test(lb,gb, method="spearman")
cor.test(lb,sh, method="spearman") #SIGNIFICANT (litter biomass & soil moisture)
cor.test(gb,sh, method="spearman")
# Not strongly correlated with each other with one exception, but see below. Most are correlated with percent cover

####CORRELATIONS btwn ENVT VARIABLES (NON SOIL) and PERCENT COVER
cor.test(pc,lb, method="spearman") #SIGNIFICANT (litter biomass & percent cover)
cor.test(pc,sp, method="spearman")
cor.test(pc,gb, method="spearman") #SIGNIFICANT (grass biomass & percent cover)
cor.test(pc,sh, method="spearman") #SIGNIFICANT (soil moisture & percent cover)

####CORRELATIONS AMONG SOIL VARIABLES
ph<-nest_data$ph
P<-nest_data$P
K<-nest_data$K
Ca<-nest_data$Ca
Mg<-nest_data$Mg
Al<-nest_data$Al
OM<-nest_data$org.mat

# If you want to see the histograms...
# hist(ph)
# hist(P)
# hist(K)
# hist(Ca)
# hist(Mg)
# hist(Al)
# hist(OM)

cor.test(ph,P)  #SIGNIFICANT
cor.test(ph,K)
cor.test(ph,Ca)
cor.test(ph,Mg)
cor.test(ph,Al) #SIGNIFICANT
cor.test(ph,OM) #SIGNIFICANT
cor.test(ph,P)  #SIGNIFICANT
cor.test(P,K)
cor.test(P,Ca)
cor.test(P,Mg)
cor.test(P,Al)  #SIGNIFICANT
cor.test(P,OM)  #SIGNIFICANT
cor.test(K,Ca)  #SIGNIFICANT
cor.test(K,Mg)  #SIGNIFICANT
cor.test(K,Al)
cor.test(K,OM)
cor.test(Ca,Mg) #SIGNIFICANT
cor.test(Ca,Al)
cor.test(Ca,OM)
cor.test(Mg,Al)
cor.test(Mg,OM)
cor.test(Al,OM) #SIGNIFICANT
cor.test(pc,ph)
cor.test(pc,P)
cor.test(pc,K)
cor.test(pc,Ca) #SIGNIFICANT
cor.test(pc,Mg)
cor.test(pc,Al)
cor.test(pc,OM)
cor.test(pc,P)

# Envt'l with Soil Chem
cor.test(lb,ph) #SIGNIFICANT
cor.test(lb,P)  #SIGNIFICANT
cor.test(lb,K)
cor.test(lb,Ca) #SIGNIFICANT
cor.test(lb,Mg)
cor.test(lb,Al) #SIGNIFICANT
cor.test(lb,OM)
cor.test(sp,ph)
cor.test(sp,P)  #SIGNIFICANT
cor.test(sp,K)
cor.test(sp,Ca)
cor.test(sp,Mg)
cor.test(sp,Al) #SIGNIFICANT
cor.test(sp,OM) #SIGNIFICANT
cor.test(sp,sh) #SIGNIFICANT
cor.test(gb,ph)
cor.test(gb,P)
cor.test(gb,K)
cor.test(gb,Ca)
cor.test(gb,Mg)
cor.test(gb,Al)
cor.test(gb,OM) #SIGNIFICANT
cor.test(gb,sh)
cor.test(sh,ph) #SIGNIFICANT
cor.test(sh,P)  #SIGNIFICANT
cor.test(sh,K)
cor.test(sh,Ca)
cor.test(sh,Mg)
cor.test(sh,Al) #SIGNIFICANT
cor.test(sh,OM) #SIGNIFICANT





############################################################################################################
### APPENDIX C FIGURES: Canopy Cover vs. Envt'l Variables
############################################################################################################

nest_data<-nest_data
# A) grass.bmass

# jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-A.jpeg",
#      width = 6, height = 4, units = "in", bg = "white", res = 300)
grass.canopy<-ggplot(nest_data, aes(x = cover, y = grass.bmass, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Grass biomass (g)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=9, y=1500, label="A", fontface="bold", size=8, color="black")
grass.canopy<-grass.canopy + scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(-1, 1500))
grass.canopy<-grass.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
grass.canopy<- grass.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=16, vjust=-3, hjust=0.05),        #Sets title size, style, location
        #making the axis legend fot size = 0 so that it doesn't appear in pic where all together
        axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        # legend.position = 'top',
        legend.position = c(0.9,0.8),
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=14),  
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1.5,1.5,1.5,1.5), "cm")) #+  #plot margin - top, right, bottom, left
grass.canopy
# dev.off()




# B) litter.bmass
# jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-B.jpeg",
#      width = 6, height = 4, units = "in", bg = "white", res = 300)
litter.canopy<-ggplot(nest_data, aes(x = cover, y = litter.bmass, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Litter biomass (g)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=9, y=1250, label="B", fontface="bold", size=8, color="black")
litter.canopy<-litter.canopy + scale_y_continuous(breaks = seq(0, 1250, 250), limits = c(-1, 1250))
litter.canopy<-litter.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
litter.canopy<- litter.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=16, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'none',
        # legend.position = c(0.9,0.9),
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=14),  
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1.5,1.5,1.5,1.5), "cm")) #+  #plot margin - top, right, bottom, left
litter.canopy
# dev.off()



# C) soil.pen
# jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-C.jpeg",
#      width = 6, height = 4, units = "in", bg = "white", res = 300)
soil.canopy<-ggplot(nest_data, aes(x = cover, y = soil.pen, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Soil penetrability (mm)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=9, y=12.5, label="C", fontface="bold", size=8, color="black")
soil.canopy<-soil.canopy + scale_y_continuous(breaks = seq(2.5, 12.5, 2.5), limits = c(2.5, 12.5))
soil.canopy<-soil.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
soil.canopy<- soil.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=16, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 14),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'none',
        # legend.position = c(0.9,0.9),
        # legend.title = element_blank(),   #Removes the Legend title
        # legend.text = element_text(color="black", size=12),  
        # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1.5,1.5,1.5,1.5), "cm")) #+  #plot margin - top, right, bottom, left
soil.canopy
# dev.off()

# D) soil.moisture.surface
# jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-D.jpeg",
#      width = 6, height = 4, units = "in", bg = "white", res = 300)
moisture.canopy<-ggplot(nest_data, aes(x = cover, y = soil.moisture.surface, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Soil moisture (%)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=9, y=7.5, label="D", fontface="bold", size=8, color="black")
moisture.canopy<-moisture.canopy + scale_y_continuous(breaks = seq(1.5, 7.5, 1), limits = c(1.5, 7.5))
moisture.canopy<-moisture.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
moisture.canopy<- moisture.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=16, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'none',
        # legend.position = c(0.9,0.9),
        # legend.title = element_blank(),   #Removes the Legend title
        # legend.text = element_text(color="black", size=14),  
        # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(1.5,1.5,1.5,1.5), "cm")) #+  #plot margin - top, right, bottom, left
moisture.canopy
# dev.off()


######################################################
# BINDING THESE UP TO MAKE FIGURE APPENDIX C
######################################################
# uses source(muliplot.R) loaded at start of code
source("multiplot.R")

FigAppC<-multiplot(grass.canopy,soil.canopy,litter.canopy, moisture.canopy, cols=2)
ggsave("./Output/FigAppendixC.eps", plot = multiplot(grass.canopy, soil.canopy,litter.canopy, moisture.canopy, cols=2), device = "eps", scale = 1, width = 10, height = 10, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

# Sep panels
ggsave("./Output/AppC-A.eps", grass.canopy, device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

ggsave("./Output/AppC-B.eps", soil.canopy,  device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

ggsave("./Output/AppC-C.eps", litter.canopy,  device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

ggsave("./Output/AppC-D.eps", moisture.canopy,device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

# ggsave("/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan//Paper 2 thesis Ch2 (PeerJ)/PeerJ v3/Fig4.eps", plot = multiplot(canopy.sdlgs.fig1,canopy.sdlgs.fig2, cols=1), device = "eps", scale = 1, width = NA, height = NA, units = c("in", "cm", "mm"), dpi = 300, limitsize = TRUE)

######################################################



######################################################
#  HISTOGRAM OF PLANT HEIGHT: APPENDIX D
######################################################
  str(veg_data)
  ht_histogram<-ggplot(veg_data, aes(x=ht_cm, fill=location)) + 
    geom_histogram(binwidth=15, position="dodge")
    ht_histogram<-ht_histogram+scale_fill_manual(values=c("#000066","#0072B2","#666666"))+
    ylab("Frequency") +
    xlab("Plant height (cm)")
    ht_histogram<-ht_histogram+scale_y_continuous(breaks = seq(0, 200, 20), limits = c(0, 200))
    ht_histogram<-ht_histogram+scale_x_continuous(breaks = seq(0, 130, 10), limits = c(0, 130))
    ht_histogram<-ht_histogram +theme_classic()+
      theme(plot.title = element_text(face="bold", size=16, vjust=-3, hjust=0.05),        #Sets title size, style, location
            axis.title.x=element_text(colour="black", size = 14, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
            axis.title.y=element_text(colour="black", size = 14, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
            axis.text=element_text(colour="black", size = 12),                              #sets size and style of labels on axes
            axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
            axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
            legend.position = 'right',
            # legend.position = c(0.9,0.9),
            # legend.title = element_blank(),   #Removes the Legend title
            # legend.text = element_text(color="black", size=14),  
            # legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
            plot.margin =unit(c(1.5,1.5,1.5,1.5), "cm")) 
    ht_histogram

############################################################################################################
### SUMMARY DATA
############################################################################################################

# This will calculate means of each column ignoring the NAs in each.
# SUMM <- nest_data %>%
#   group_by(location) %>%
#   summarise_each(funs(mean(., na.rm = TRUE)))
# 
# SUMM <- nest_data %>%
#   group_by(location) %>%
#   summarise_each(funs(sd(., na.rm = TRUE)))

# This will calculate means of each column ignoring the NAs in each.
SUMM <- nest_data %>%
  group_by(location) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

SUMM2 <- sdlgs.nosoil %>%
  group_by(location) %>%
  summarise_each(funs(mean(., na.rm = TRUE)))

SUMM3 <- sdlgs.nosoil %>%
  group_by(location) %>%
  summarise_each(funs(sd(., na.rm = TRUE)))

sum(sdlgs.nosoil$sdlg.no)

# This tells you the common species in our survey
common.spp<-as.data.frame(count(veg_data, species))
common.spp<-common.spp[order(-common.spp$n),] #- to make it descending order



 
# IN RESPINSE TO REVIEWS OF V1: BOOTSTRAP TO SEE IF RATIO OF SAMPLES TO VARIABLES A PROBLEM for our PCA

source("PCAxisScoreBoot.R") #Function to do the Bootstrapping
PCA<-env.varswithsoil # Which PCA?asa
Reps<-100 #How many reps? 10K is good
AxisNumber=1 #Whihc Axis?

PCAxisScoreBoot(PCA, AxisNumber,Reps)






# NOTES
#
# for quesiton on dot colors posted on stack overflow
# http://stackoverflow.com/questions/30968563/ggbiplot-how-to-maintain-group-colors-after-changing-point-size
# 
# env.vars<-data.frame(replicate(5,sample(0:10,20,rep=TRUE)))
# cover<-c(89, 92, 72, 53, 88, 89, 71, 83, 71, 66, 23, 30,  5, 15, 57, 54,0, 23, 9, 16)
# location<-c("location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2", "location1", "location2")
# point.size<-cover*0.1
#
