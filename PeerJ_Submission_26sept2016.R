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
install_github("embruna/ggbiplot") 
library(ggbiplot)
library(tidyverse) #Data Manipulations+ggplo1
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
rm(list=ls())

############################################################################################################
### DATA ENTRY AND CLEANUP
############################################################################################################

# NEST DATA
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
NEST.DATA.both<-filter(NEST.DATA, habitat =="CD" | habitat == "CR") # Select only Cerrado Denso and Cerrado Ralo
NEST.DATA.both<-droplevels(NEST.DATA.both)
str(NEST.DATA.both)

# Which of the habitats are you going to analyze: CD, CR, or both?
# NEST.DATA.PCA.ALL<-NEST.DATA.CD
# NEST.DATA.PCA.ALL<-NEST.DATA.CR
NEST.DATA.PCA.ALL<-NEST.DATA.both

# SEEDLINGS
VEG<-read.csv("./Data/ActiveNests_CensoVeg_1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
VEG$location=factor(VEG$location, levels=c("nest","adjacent", "far"), ordered=TRUE)
VEG_both<-VEG[VEG$habitat=="CR"|VEG$habitat=="CD",] #both habitats
VEG_both <- droplevels(VEG_both)

















############################################################################################################
### APPENDIX C FIGURES: Canopy Cover vs. Envt'l Variables
############################################################################################################
# A) grass.bmass

jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-A.jpeg",
     width = 6, height = 4, units = "in", bg = "white", res = 300)
grass.canopy<-ggplot(NEST.DATA.PCA.ALL, aes(x = perc.cover, y = grass.bmass, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Grass biomass (g)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=5, y=1500, label="A", fontface="bold", size=8, color="black")
grass.canopy<-grass.canopy + scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(-1, 1500))
grass.canopy<-grass.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
grass.canopy<- grass.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        #legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
grass.canopy
dev.off()




# B) litter.bmass
jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-B.jpeg",
     width = 6, height = 4, units = "in", bg = "white", res = 300)
litter.canopy<-ggplot(NEST.DATA.PCA.ALL, aes(x = perc.cover, y = litter.bmass, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Litter biomass (g)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=5, y=1250, label="B", fontface="bold", size=8, color="black")
litter.canopy<-litter.canopy + scale_y_continuous(breaks = seq(0, 1250, 250), limits = c(-1, 1250))
litter.canopy<-litter.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
litter.canopy<- litter.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        #legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
litter.canopy
dev.off()



# C) soil.pen
jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-C.jpeg",
     width = 6, height = 4, units = "in", bg = "white", res = 300)
soil.canopy<-ggplot(NEST.DATA.PCA.ALL, aes(x = perc.cover, y = soil.pen, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Soil penetrability (mm)") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=5, y=12.5, label="C", fontface="bold", size=8, color="black")
soil.canopy<-soil.canopy + scale_y_continuous(breaks = seq(2.5, 12.5, 2.5), limits = c(2.5, 12.5))
soil.canopy<-soil.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
soil.canopy<- soil.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        #legend.position = c(0.9,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
soil.canopy
dev.off()

# D) soil.moisture.surface
jpeg(file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/AppendixC-D.jpeg",
     width = 6, height = 4, units = "in", bg = "white", res = 300)
moisture.canopy<-ggplot(NEST.DATA.PCA.ALL, aes(x = perc.cover, y = soil.moisture.surface, colour=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(15,16,17))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Soil moisture") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
  annotate ("text", x=5, y=7.5, label="D", fontface="bold", size=8, color="black")
moisture.canopy<-moisture.canopy + scale_y_continuous(breaks = seq(1.5, 7.5, 1), limits = c(1.5, 7.5))
moisture.canopy<-moisture.canopy + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-1, 100))
moisture.canopy<- moisture.canopy + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 20, vjust=0),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 20, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
        legend.position = 'top',
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        #legend.position = c(0.1,0.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
moisture.canopy
dev.off()






############################################################################################################
### FIG 1A: Histogram of Canopy Cover Gradient
############################################################################################################

cover.fig.gradient<-ggplot(NEST.DATA.PCA.ALL, aes(x=perc.cover, fill=habitat)) +
  geom_histogram(binwidth=5, alpha=.7, position="identity", colour="black")+
  scale_fill_grey(start=0.3, end=1, labels = c("Cerrado denso","Cerrado ralo"))+
  ylab("No. of plots") + 
  xlab("Canopy cover (%)")+
  annotate ("text", x=1.5, y=12, label="A", fontface="bold", size=8, color="black")+
  guides(fill = guide_legend(nrow=2,byrow=TRUE, override.aes = list(colour = NULL))) #remove slash from legend

cover.fig.gradient<-cover.fig.gradient + scale_y_continuous(breaks = seq(0, 16, 2))
cover.fig.gradient<- cover.fig.gradient + theme_classic()+theme(plot.title = element_text(face="bold", size=20),                            #Sets title size, style, location
                                                        legend.position=c(0.5,0.95),
                                                        axis.line.y = element_line(color="black", size = 0.5, lineend="square"),
                                                        axis.line.x = element_line(color="black", size = 0.5, lineend="square"),
                                                        axis.title.x=element_text(colour="black", size = 20, vjust=-0.5),           #Sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                        axis.title.y=element_text(colour="black", size = 20, vjust=1.5),            #Sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                        axis.text=element_text(colour="black", size = 16),                          #Sets size and style of labels on axes
                                                        legend.title = element_blank(),                                             #Removes the Legend title
                                                        #legend.key = element_blank(),                                              #Removes the boxes around legend colors
                                                        legend.text = element_text(face="italic", color="black", size=16),
                                                        # legend.position = "none",                                            
                                                        legend.key = element_rect(colour = "black"))                                #puts black line around legend box
                                                      
cover.fig.gradient                                                       
  


############################################################################################################
### Analyses: Does Canopy Cover vary with Proimity to ant nests? 
############################################################################################################

# analyses
str(NEST.DATA.both)
coverxhab<-dplyr::select(NEST.DATA.both, habitat, nest, location, perc.cover, nest.area)
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

summary.table.cover <- do.call(rbind, lapply(list(cover1, cover4, cover5), broom::glance))
summary.table.cover[["model"]] <- 1:3
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table <- summary.table.cover[table.cols]
names(reported.table) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table[['dAIC']] <-  with(reported.table, AIC - min(AIC))
reported.table[['wAIC']] <- with(reported.table, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table$AIC <- NULL
write.csv(reported.table, file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/CoverxLocTable.csv", row.names = F) #export it as a csv file

############################################################################################################
### Figure 1B 
############################################################################################################

# Fig 1B Canopy cover for each plot splitr by nest 
cover.fig.location<-ggplot(data=coverxhab, aes(x=location, y=perc.cover, group=nest)) +
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
                                                         axis.text=element_text(colour="black", size = 18),                              #sets size and style of labels on axes
                                                         plot.margin = unit(c(1,3,2,1), "cm"))
cover.fig.location


############################################################################################################
### Correlations between environmental variables
############################################################################################################

lb<-NEST.DATA.PCA.ALL$litter.bmass
sp<-NEST.DATA.PCA.ALL$soil.pen
gb<-NEST.DATA.PCA.ALL$grass.bmass
sh<-NEST.DATA.PCA.ALL$soil.moisture.surface
pc<-NEST.DATA.PCA.ALL$perc.cover

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
ph<-NEST.DATA.PCA.ALL$ph
P<-NEST.DATA.PCA.ALL$P
K<-NEST.DATA.PCA.ALL$K
Ca<-NEST.DATA.PCA.ALL$Ca
Mg<-NEST.DATA.PCA.ALL$Mg
Al<-NEST.DATA.PCA.ALL$Al
OM<-NEST.DATA.PCA.ALL$org.mat

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
### PCA: Data Prep
############################################################################################################

#This is missing in a bunch of them, so will reduce sample size too much
NEST.DATA.PCA.ALL$peak.soil.temp<-NULL
NEST.DATA.PCA.ALL$time.peak.soil.temp<-NULL
NEST.DATA.PCA.ALL$date.peak.soil.tem<-NULL
NEST.DATA.PCA.ALL$soil.moisture.deep<-NULL

NEST.DATA.PCA.NOSOILS<-NEST.DATA.PCA.ALL
NEST.DATA.PCA.NOSOILS$ph<-NULL
NEST.DATA.PCA.NOSOILS$P<-NULL
NEST.DATA.PCA.NOSOILS$K<-NULL
NEST.DATA.PCA.NOSOILS$Ca<-NULL
NEST.DATA.PCA.NOSOILS$Mg<-NULL
NEST.DATA.PCA.NOSOILS$Al<-NULL
NEST.DATA.PCA.NOSOILS$org.mat<-NULL    

#Clear all those with missing values to do PCA
NEST.DATA.PCA.ALL<-na.omit(NEST.DATA.PCA.ALL) 
NEST.DATA.PCA.NOSOILS<-na.omit(NEST.DATA.PCA.NOSOILS) 

droplevels(NEST.DATA.PCA.ALL)
droplevels(NEST.DATA.PCA.NOSOILS)

# str(NEST.DATA.PCA.ALL)
# summary(NEST.DATA.PCA.ALL)
# str(NEST.DATA.PCA.NOSOILS)
# summary(NEST.DATA.PCA.NOSOILS)

# REMOVE CANOPY COVER FROM THE PCA 
# (recall we are testing if canopy cover is independent in effects on env't and seedlings). 
# BUT FIRST CREATE A VECTOR OF CANOPY COVER AND NEST AREA IN CASE YOU NEED IT LATER
cover.nosoils<-NEST.DATA.PCA.NOSOILS$perc.cover
cover.ALL<-NEST.DATA.PCA.ALL$perc.cover
nest.area.nosoils<-NEST.DATA.PCA.NOSOILS$nest.area
nest.area.ALL<-NEST.DATA.PCA.ALL$nest.area

NEST.DATA.PCA.ALL$perc.cover<-NULL
NEST.DATA.PCA.NOSOILS$perc.cover<-NULL

NEST.DATA.PCA.ALL$nest.area<-NULL
NEST.DATA.PCA.NOSOILS$nest.area<-NULL

# if you wanted to log transform your variables you would do so here. 
# However, we aren't transforming as per convo with HLV
# env.vars <- log(NEST.DATA.PCA[, 5:18]+1)
 
# FOR PCA ALL (WITH SOILS)
env.vars.all <- NEST.DATA.PCA.ALL[,5:dim(NEST.DATA.PCA.ALL)[2]] #did it with dim because so that you don't have to adjust this if you decide to include canopy cover in the PCA
site.cats.all <- NEST.DATA.PCA.ALL[, 1:4]

# FOR PCA ALL (NO SOILS)
env.vars.nosoil <- NEST.DATA.PCA.NOSOILS[,5:dim(NEST.DATA.PCA.NOSOILS)[2]] #did it with dim because so that you don't have to adjust this if you decide to include canopy cover in the PCA
site.cats.nosoil <- NEST.DATA.PCA.NOSOILS[, 1:4]


############################################################################################################
### PCA-2 (no soils chem, all plots and nests)
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
### PCA-1 (with soils chem, only plots "on" and "far")
############################################################################################################

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
nest.env.pca.all <- prcomp(env.vars.all,
                           center = TRUE,
                           scale. = TRUE) 

# Visualizing the results.
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

############################################################################################################
###   FIG 2A - PCA-1 (NO SOILS, ALL PLOTS
############################################################################################################

location.nosoil<-site.cats.nosoil$location
cover.nosoil<-cover.nosoils
nest.nosoil<-site.cats.nosoil$nest

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
  annotate ("text", x=-3, y=3.5, label="A) PCA-1", fontface="bold", size=8, color="black")
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
###   FIG 2B - PCA-2 (SOILS, ONLY "ON" and "FAR" PLOTS
############################################################################################################

location.all<-site.cats.all$location
cover.all<-cover.ALL
nest.all<-site.cats.all$nest

# The list used to make the figure is uses the column names (i.e., grass.biomass). The next three lines
# Will replace these names with those you actually want to appear in the figure.  
# Note this doesn't change the names in the original dataframe
# ?ggbiplot for more info on arguments you can change and % used to draw ellipses aroudnd points

nest.env.pca.all$rotation
dimnames(nest.env.pca.all$rotation)
dimnames(nest.env.pca.all$rotation)[[1]]<-c("litter", "soil penet.", "grass", 
                                            "pH", "P", "K", "Ca","Mg","Al","org mat","soil moisture")

point.size<-cover.ALL*6
# Note on making the figure. In the original attempt the legend of the arrows was always under the points, making it tough to read. 
# This is because the geom_point layer with the adjusted point sizes was placed on top of the original plot.
# to fix this, make the points completely transparent in the original plot by setting alpha to 0, then makle the pooints on the 
# geom_plot layer semin transparent.
g_soils <- ggbiplot(nest.env.pca.all, obs.scale = 1, var.scale = 1, alpha=0,
                    group = location.all, ellipse = TRUE, 
                    circle = FALSE, varname.size=6, varname.adjust=1.2)+
  geom_point(aes(color=location.all, size = point.size, shape=location.all), alpha=0.7) + 
  scale_shape_manual(values=c(16,15), guide=FALSE)+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#666666"), guide=FALSE)+
  annotate ("text", x=-3, y=3.5, label="B) PCA-2", fontface=
              "bold", size=8, color="black")
# geom_point(size=point.size)  #Scaling the size of the point by canopy cover. 100% canopy cover=point size = 6.  That is why each % is multiplied by 0.06
# geom_point(aes(color=location.all, size = point.size)) + scale_size_identity()
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


############################################################################################################
############################################################################################################
### ANALYSES OF SEEDLING # AND DIV VS PCA SCORES
############################################################################################################
############################################################################################################

############################################################################################################
## START BY EXTRACTING COMPONENT SCORES - PCA NO SOIL CHEM
############################################################################################################
#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scores.nosoil<-(nest.env.pca.nosoil$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1.nosoil<-as.data.frame(pca.plot.scores.nosoil[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2.nosoil<-as.data.frame(pca.plot.scores.nosoil[,2])#this saves the 2nd column - PCA Axis 2 - as a dataframe
GLM.DATA.nosoil<-as.data.frame(cbind(nest.nosoil,location.nosoil, cover.nosoils, PCA.1.nosoil, PCA.2.nosoil))
names(GLM.DATA.nosoil)[4]<-"PCA1.nosoil" #reaname the column
names(GLM.DATA.nosoil)[5]<-"PCA2.nosoil" #rename the column


############################################################################################################
## START BY EXTRACTING COMPONENT SCORES - PCA WITH SOIL CHEM
############################################################################################################

#nest.env.pca$x =  scores for each of the plots for each PCA
pca.plot.scores.all<-(nest.env.pca.all$x) #this saves the matrix of PCA scores (all axes) for all plots 
PCA.1.all<-as.data.frame(pca.plot.scores.all[,1]) #this saves the 1st column - PCA Axis 1 - as a dataframe
PCA.2.all<-pca.plot.scores.all[,2]#this saves the 2nd column - PCA Axis 2 - as a dataframe
GLM.DATA.all<-as.data.frame(cbind(nest.all,location.all, cover.ALL, PCA.1.all, PCA.2.all))
names(GLM.DATA.all)[4]<-"PCA1.all" #reaname the column
names(GLM.DATA.all)[5]<-"PCA2.all" #rename the column



############################################################################################################
#### COUNT OF SEEDLINGS AND SPP RICH IN PLOTS 
############################################################################################################

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
select <- dplyr::select
VEG_both_hack<-select(VEG_both,nest,location,species,plot.id)
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
sdlgs<-cbind(NEST.DATA.both$plot.id, as.data.frame(count.df)) #NOTE: added the as.data.frame on 9/22, not sure why wouldn't do without conversion as before.
names(sdlgs)[1]<-"plot.id" #rename the column

# THIS CREATES A DATAFRAME YOU CAN BIND TO THE % COVER
sdlgs<-cbind(sdlgs, spp.df)
names(sdlgs)[5]<-"spp.no" #rename the column
str(sdlgs)

# BIND THEM UP....add % cover and nest area!!!
sdlgs.perc.cover<-select(NEST.DATA.both, plot.id, perc.cover, nest.area)
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


############################################################################################################
## TEST: CORRELATION BETWEEN CANOPYY COVER & PCA SCORES: EDGE/NEST ONLY, WITH SOILS CHEM
##
## FIGURE: PCA SCORE vs. CANOPY COVER
##
## ANALYSIS: GLMM
############################################################################################################

# Testing for correlation between canopy cover and PCA axis scores
DATA<-droplevels(na.omit(sdlgs.all))
cor.test(DATA$cover,DATA$PCA1.all)
cor.test(DATA$cover,DATA$PCA2.all)


############################################################################################################
# Fig 3B: PCA-with-soils vs. canopy cover 
############################################################################################################
CoverEnv<-ggplot(DATA, aes(x = cover, y = PCA1.all, col=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,15))+
  ylab("Axis 1 score") +
  xlab("Canopy cover (%)")+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines
CoverEnv<-CoverEnv + scale_colour_manual(values=c("#000066", "#666666"))  #I chose my own colors for the lines
CoverEnv<-CoverEnv +annotate ("text", x=5, y=4, label="B) PCA-2", fontface="bold", size=8, color="black")
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


# analyses
DATA<-droplevels(na.omit(sdlgs.all))

COVARIATE<-DATA$cover
COVARIATE2<-DATA$nest.area
RESPONSE<-DATA$PCA1.all
FIXED<-DATA$location
#DOES INCLUDING NEST AREA IMPROVE THE FIT OVER JUST random effect of NEST?
pca1.covariate1<-lmer(RESPONSE ~ COVARIATE2 + (1|nest), data = DATA)
summary(pca1.covariate1)
pca1.nest<-lmer(RESPONSE ~  (1|nest), data = DATA)
summary(pca1.nest)
AIC(pca1.covariate1, pca1.nest)
anova(pca1.covariate1,pca1.nest, test = "Chisq")
#NO, SO DON't INCLUDE

# Nest identity is a random effect RANDOM<-(1|nest)
pca1.1<-lmer(RESPONSE ~   (1|nest), data = DATA)
summary(pca1.1)
pca1.2<-lmer(RESPONSE ~ FIXED + (1|nest), data = DATA)
summary(pca1.2)
pca1.3<-lmer(RESPONSE ~ COVARIATE + (1|nest), data = DATA)
summary(pca1.3)
pca1.4<-lmer(RESPONSE ~ FIXED + COVARIATE + (1|nest), data = DATA)
summary(pca1.4)
pca1.5<-lmer(RESPONSE ~ FIXED * COVARIATE + (1|nest), data = DATA)
summary(pca1.5)

AIC(pca1.5,pca1.4,pca1.3,pca1.2, pca1.1)
anova(pca1.5,pca1.4,pca1.3,pca1.2, pca1.1, test = "Chisq")


#Need table for models with
#random effect of nest ID only (1)
# effect of plot location + radnom
# effect of canopy cover + radnom
# effect of plot, canopy, random, but no interaction
# but no interaction with canopy cover covariate 
# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.pca1 <- do.call(rbind, lapply(list(pca1.1, pca1.2,pca1.3, pca1.4, pca1.5), broom::glance))
summary.table.pca1[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.pca1 <- summary.table.pca1[table.cols]
names(reported.table.pca1) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.pca1[['dAIC']] <-  with(reported.table.pca1, AIC - min(AIC))
reported.table.pca1[['wAIC']] <- with(reported.table.pca1, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.pca1$AIC <- NULL
reported.table.pca1 <- reported.table.pca1[order(reported.table.pca1$dAIC),]
write.csv(reported.table.pca1, file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/PCA2vLoc.csv", row.names = F) #export it as a csv file

##################################################################################
##################################################################################
## GLMM with Nest  AND Canopy cover treated as random effects
##################################################################################
##################################################################################
COVARIATE<-DATA$cover
COVARIATE2<-DATA$nest.area
RESPONSE<-DATA$PCA1.all
FIXED<-DATA$location
#DOES INCLUDING NEST AREA IMPROVE THE FIT OVER JUST random effect of NEST?
pca1.covariate1<-lmer(RESPONSE ~ COVARIATE2 + (1|nest), data = DATA)
summary(pca1.covariate1)
pca1.nest<-lmer(RESPONSE ~  (1|nest), data = DATA)
summary(pca1.nest)
AIC(pca1.covariate1, pca1.nest)
anova(pca1.covariate1,pca1.nest, test = "Chisq")
#NO, SO DON't INCLUDE

# Nest identity is a random effect RANDOM<-(1|nest)
pca1.1<-lmer(RESPONSE ~ (1|nest), data = DATA)
summary(pca1.1)
pca1.2<-lmer(RESPONSE ~ FIXED + (1|nest), data = DATA)
summary(pca1.2)
pca1.3<-lmer(RESPONSE ~ (1|COVARIATE) + (1|nest), data = DATA)
summary(pca1.3)
pca1.4<-lmer(RESPONSE ~ FIXED + (1|COVARIATE) + (1|nest), data = DATA)
summary(pca1.4)
pca1.5<-lmer(RESPONSE ~ FIXED * (1|COVARIATE) + (1|nest), data = DATA)
summary(pca1.5)

AIC(pca1.5,pca1.4,pca1.3,pca1.2, pca1.1)
anova(pca1.5,pca1.4,pca1.3,pca1.2, pca1.1, test = "Chisq")


#Need table for models with
#random effect of nest ID only (1)
# effect of plot location + radnom
# effect of canopy cover + radnom
# effect of plot, canopy, random, but no interaction
# but no interaction with canopy cover covariate 
# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.pca1 <- do.call(rbind, lapply(list(pca1.1, pca1.2,pca1.3, pca1.4, pca1.5), broom::glance))
summary.table.pca1[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.pca1 <- summary.table.pca1[table.cols]
names(reported.table.pca1) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.pca1[['dAIC']] <-  with(reported.table.pca1, AIC - min(AIC))
reported.table.pca1[['wAIC']] <- with(reported.table.pca1, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.pca1$AIC <- NULL
reported.table.pca1 <- reported.table.pca1[order(reported.table.pca1$dAIC),]
write.csv(reported.table.pca1, file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al PeerJ (Ch2)/PeerJ v2/PCA2vLoc.csv", row.names = F) #export it as a csv file


##################################################################################
##################################################################################





############################################################################################################
## TEST: CORRELATION BETWEEN CANOPYY COVER & PCA SCORES : ALL PLOTS, NO SOILS CHEM
##
## FIGURE: PCA SCORE vs. CANOPY COVER
##
## ANALYSIS: GLMM
############################################################################################################

#############
#CoverEnvAll 
# Testing for correlation between canopy cover and PCA axis scores
DATA2<-droplevels(na.omit(sdlgs.nosoil))
#DATA3<-DATA2[!DATA2$location %in% c("adjacent"),] 
cor.test(DATA2$perc.cover,DATA2$PCA1.nosoil)
cor.test(DATA2$perc.cover,DATA2$PCA2.nosoil)


############################################################################################################
# Figure 3A: PCA-NO-soils DATA vs. canopy cover 
############################################################################################################

CoverEnvAll<-ggplot(DATA2, aes(x = perc.cover, y = PCA1.nosoil, col=location, shape=location,fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Axis 1 score") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE) # Add linear regression lines
  CoverEnvAll<-CoverEnvAll + annotate ("text", x=5, y=2, label="A) PCA-1", fontface="bold", size=8, color="black")
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
# ANALYSES
############################################################################################################

RESPONSE<-DATA2$PCA2.nosoil
COVARIATE<-DATA2$perc.cover
COVARIATE2<-DATA2$nest.area
FIXED<-DATA2$location
# Nest identity is a random effect RANDOM<-(1|nest)


#DOES INCLUDING COVARIATE HELP?
pca2.covatiate2<-lmer(RESPONSE ~ COVARIATE2 + (1|nest), data = DATA2)
summary(pca2.covatiate2)
pca2.nest<-lmer(RESPONSE ~  (1|nest), data = DATA2)
summary(pca2.covatiate2)
AIC(pca2.covatiate2, pca2.nest)
anova(pca2.covatiate2,pca2.nest, test = "Chisq")
#NO!!!

# Nest identity is a random effect RANDOM<-(1|nest)
# random effect only
pca2.1<-lmer(RESPONSE ~ (1|nest), data = DATA2)
summary(pca2.1)
# effect of location + random
pca2.2<-lmer(RESPONSE ~ FIXED + (1|nest), data = DATA2)
summary(pca2.2)
# effect of cover + random
pca2.3<-lmer(RESPONSE ~ COVARIATE + (1|nest), data = DATA2)
summary(pca2.3)
#effect of both + random (no interaction)
pca2.4<-lmer(RESPONSE ~ FIXED + COVARIATE + (1|nest), data = DATA2)
summary(pca2.4)
#effect of both, their interaction,and random
pca2.5<-lmer(RESPONSE ~ FIXED * COVARIATE + (1|nest), data = DATA2)
summary(pca2.5)

AIC(pca2.1,pca2.2,pca2.3,pca2.4,pca2.5)
anova(pca2.1,pca2.2, pca2.3,pca2.4,pca2.5,test = "Chisq")

#Need table for models with
#random effect of nest ID only (1)
# effect of plot location + radnom
# effect of canopy cover + radnom
# effect of plot, canopy, random, but no interaction
# but no interaction with canopy cover covariate 
# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.pca2 <- do.call(rbind, lapply(list(pca2.1, pca2.2,pca2.3, pca2.4, pca2.5), broom::glance))
summary.table.pca2[["model"]] <- 1:5
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.pca2 <- summary.table.pca2[table.cols]
names(reported.table.pca2) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.pca2[['dAIC']] <-  with(reported.table.pca2, AIC - min(AIC))
reported.table.pca2[['wAIC']] <- with(reported.table.pca2, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.pca2$AIC <- NULL
reported.table.pca2 <- reported.table.pca2[order(reported.table.pca2$dAIC),]
write.csv(reported.table.pca2, file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al MS 1 (Ch2)/PCA2vLoc.csv", row.names = F) #export it as a csv file



############################################################################################################
### WHAT FACTORS INFLUENCE SEEDLING COUNT?  
############################################################################################################
# Nice overview of GLMs here: http://plantecology.syr.edu/fridley/bio793/glm.html
#  Need to consider nest a random effect
# below is based on Grueber et al. 2011. J Evol Biol. 24:699-711.  
# Another good source is http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html
# for more on lmer/glmer error message:
# http://stats.stackexchange.com/questions/35841/using-glmer-to-replicate-result-from-lmer-for-mulitlevel-modelling-in-r

############################################################################################################
# WHICH DATASET and COVARIATE? 
############################################################################################################
# If you are using all the biotic and abiotic data collected for the PCA then you are using sdlgs.all BUT
# # this dataset only includes plot ON or AWAY from nests
DATA<-droplevels(na.omit(sdlgs.all))
COVARIATE<-DATA$PCA1.all
COVARIATE2<-DATA$cover
COVARIATE3<-DATA$nest.area

#  OR
#  If you want to include all the plots - on, adjacent, and far from nests - then you are using sdlgs.nosoil because
#  this dataset does NOT have soils chem data

# COVARIATE<-DATA$PCA2.all
# DATA<-droplevels(na.omit(sdlgs.nosoil))
# COVARIATE<-DATA$PCA1.nosoil
# COVARIATE2<-DATA$perc.cover
# COVARIATE3<-DATA$nest.area

############################################################################################################
### WHAT RESPONSE VARIABLE? Seedling number per plot or seedling species richness per plot?
############################################################################################################
RESPONSE<-DATA$sdlg.no  
# OR 
RESPONSE<-DATA$spp.no

###################################
# WHAT FIXED (main) EFFECT? 
###################################
#Plot location: on nests, adjacent to nests, or far from nests
FIXED<-DATA$location

###################################
# OTHER
###################################
# Nest identity is a random effect RANDOM<-(1|nest)
# distribution family: poisson for starters because both are counts
# may have to use quasi-poisson die to overdispersion

#DOES IT HELP TO INCLUDE NEST AREA?
global.cov3<-glmer(RESPONSE ~ COVARIATE3 + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.cov3)

global.nest<-glmer(RESPONSE ~  (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.nest)

AIC(global.cov3,global.nest)
anova(global.cov3,global.nest, test = "Chisq")
# DELTA AIC IS SO SMALL, LETS INCLUDE

global.model<-glmer(RESPONSE ~ FIXED + COVARIATE + COVARIATE2 + COVARIATE3 + (1|nest), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
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
global.model.2 <- glmer(RESPONSE ~ FIXED + COVARIATE + COVARIATE2 + COVARIATE3 + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
print(summary(global.model.2))

# stdz.model<-standardize(global.model, standardize.y=FALSE)
# model.set<-dredge(stdz.model)
model.set<-dredge(global.model.2)
top.models<-get.models(model.set, subset=delta<2)
summary(top.models)


#random effects 
global.model1 <- glmer(RESPONSE ~  (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model1)
# gradient + random
global.model2 <- glmer(RESPONSE ~ COVARIATE2 + (1|nest) + (1|obs), data = DATA,family=poisson, na.action = "na.fail")
summary(global.model2)
# ant-related + random FOR PCA2
# global.model3<-glmer(RESPONSE ~ FIXED + COVARIATE + COVARIATE3 + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
# summary(global.model3)
# BOTH of gradient and ant FOR PCA2
# global.model4<-glmer(RESPONSE ~ FIXED + COVARIATE + COVARIATE2 + COVARIATE3 + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
# summary(global.model4)

# ant-related + random FOR PCA1
global.model3<-glmer(RESPONSE ~ FIXED +  COVARIATE3 + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model3)
# BOTH of gradient and ant for PCA!
global.model4<-glmer(RESPONSE ~ FIXED + COVARIATE + COVARIATE2 + COVARIATE3 + (1|nest)+ (1|obs), data = DATA ,family=poisson, na.action = "na.fail", REML=FALSE)
summary(global.model4)

# Nest identity is a random effect RANDOM<-(1|nest)
AIC(global.model1,global.model2,global.model3,global.model4)
anova(global.model1,global.model2,global.model3,global.model4, test = "Chisq")

# 1: random effects 
# 2: gradient + random
# 3: ant-related + random
# 4: Interaction of gradient and ant

# see http://www.ashander.info/posts/2015/10/model-selection-glms-aic-what-to-report/ for what to report
summary.table.global <- do.call(rbind, lapply(list(global.model1,global.model2,global.model3,global.model4), broom::glance))
summary.table.global[["model"]] <- 1:4
table.cols <- c("model", "df.residual", "deviance", "AIC")
reported.table.global <- summary.table.global[table.cols]
names(reported.table.global) <- c("Model", "Resid. Df", "Resid. Dev", "AIC")
reported.table.global[['dAIC']] <-  with(reported.table.global, AIC - min(AIC))
reported.table.global[['wAIC']] <- with(reported.table.global, exp(- 0.5 * dAIC) / sum(exp(- 0.5 * dAIC)))
reported.table.global$AIC <- NULL
reported.table.global <- reported.table.global[order(reported.table.global$dAIC),]
round (reported.table.global, digits = 4)
write.csv(reported.table.global, file="/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Costa et al MS 1 (Ch2)/global.csv", row.names = F) #export it as a csv file



############################################################################################################
# Fig 4A: PLOT OF SEEDLING ABUNDNACE VS canopy cover 
############################################################################################################

canopy.sdlgs.fig1<-ggplot(sdlgs.nosoil, aes(x = perc.cover, y = sdlg.no, colour=location, shape=location, fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("seedling abundance") +
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)+ 
  annotate ("text", x=5, y=85, label="A", fontface="bold", size=8, color="black")
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
    # Add linear regression lines
#plot.sdlg.no<-plot.sdlg.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
canopy.sdlgs.fig1<-canopy.sdlgs.fig1 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
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

canopy.sdlgs.fig2<-ggplot(sdlgs.nosoil, aes(x = perc.cover, y = spp.no, colour=location, shape=location, fill=location)) + 
  geom_point(size = 3) +
  scale_shape_manual(values=c(16,17,15))+  # Use a square circle and triangle
  scale_colour_manual(values=c("#000066","#0072B2","#666666"))+
  guides(fill = guide_legend(override.aes = list(linetype = 0)))+
  ylab("Species richness") +
  xlab("Canopy cover (%)")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,se=FALSE)+   # Add linear regression lines
annotate ("text", x=5, y=30, label="B", fontface="bold", size=8, color="black")
canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_y_continuous(breaks = seq(0, 30, 5), limits = c(-5, 30))
canopy.sdlgs.fig2<-canopy.sdlgs.fig2 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(-5, 100))
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



############################################################################################################
### SUMMARY DATA
############################################################################################################

# This will calculate means of each column ignoring the NAs in each.
# SUMM <- NEST.DATA.PCA.ALL %>%
#   group_by(location) %>%
#   summarise_each(funs(mean(., na.rm = TRUE)))
# 
# SUMM <- NEST.DATA.PCA.ALL %>%
#   group_by(location) %>%
#   summarise_each(funs(sd(., na.rm = TRUE)))

# This will calculate means of each column ignoring the NAs in each.
SUMM <- NEST.DATA.PCA.ALL %>%
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
common.spp<-as.data.frame(count(VEG_both, species))
common.spp<-common.spp[order(-common.spp$n),] #- to make it descending order



 
# IN RESPINSE TO REVIEWS OF V1: BOOTSTRAP TO SEE IF RATIO OF SAMPLES TO VARIABLES A PROBLEM for our PCA

source("PCAxisScoreBoot.R") #Function to do the Bootstrapping
PCA<-env.vars.all # Which PCA?asa
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
