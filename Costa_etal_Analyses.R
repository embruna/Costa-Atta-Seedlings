
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN: Costa el al MS


# library(gdata)
# library(lme4)
# library(reshape)
# library(maps)
# library(WDI)
# library(RColorBrewer)
# library(lattice)
# library(coefplot2) ## for coefplot2
# library(emdbook)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(reshape)


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
#make plot locations an ordered factor nest<adjacent<far
NEST.DATA$location=factor(NEST.DATA$location, levels=c("nest","adjacent", "far"), ordered=TRUE)

summary(NEST.DATA)
summary(VEG)
summary(AIRTEMPHUMID)

str(NEST.DATA)
str(VEG)
str(AIRTEMPHUMID)

# Use Cerrado Ralo (CR) and Cerrado Denso (CD) in analyses
#nest dataset
NEST.DATA_both<-NEST.DATA[NEST.DATA$habitat=="CR"|NEST.DATA$habitat=="CD",] #both habitats
NEST.DATA_both <- droplevels(NEST.DATA_both)

NEST.DATA_CR<-NEST.DATA[NEST.DATA$habitat=="CR",] #only CR
NEST.DATA_CR <- droplevels(NEST.DATA_CR)

NEST.DATA_CD<-NEST.DATA[NEST.DATA$habitat=="CD",] #only CD
NEST.DATA_CD <- droplevels(NEST.DATA_CD)

### VEG dataset
VEG_both<-VEG[VEG$habitat=="CR"|VEG$habitat=="CD",] #both habitats
VEG_both <- droplevels(VEG_both)

VEG_CR<-VEG[VEG$habitat=="CR",] #only CR
VEG_CR <- droplevels(VEG_CR)

VEG_CD<-VEG[VEG$habitat=="CD",] #only CD
VEG_CD <- droplevels(VEG_CD)


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
dim(NEST.DATA_both) 

# add the column "plot.id" from NEST.DATA_both to create new summary df seedlings.  NB: THERE IS HIGH POTENTIAL FOR FUNCKING THINGS 
# UP HERE, SO BE CAREFUL
sdlgs<-cbind(NEST.DATA_both$plot.id, count.df)
names(sdlgs)[1]<-"plot.id" #rename the column

# THIS CREATES A DATAFRAME YOU CAN BIND TO THE % COVER
sdlgs<-cbind(sdlgs, spp.df)
names(sdlgs)[5]<-"spp.no" #rename the column
str(sdlgs)

# BIND THEM UP....add % cover!!!
sdlgs.perc.cover<-select(NEST.DATA_both, plot.id, perc.cover)
dim(sdlgs.perc.cover) #make sure same size as sdlgs dataframe
sdlgs<-left_join(sdlgs, sdlgs.perc.cover, by = "plot.id")


# SDLG COUNTS GLMS
sdlg.glm.data<-na.omit(sdlgs)
glm.sdlg1 = glm(sdlg.no ~ perc.cover*location+nest ,data=sdlg.glm.data,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg1)

glm.sdlg2 = glm(sdlg.no ~ location+nest ,data=sdlg.glm.data,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg2)

AIC(glm.sdlg1,glm.sdlg2)
anova(glm.sdlg1,glm.sdlg2,test="Chisq")

# SPP COUNTS GLMS
sdlg.glm.data<-na.omit(sdlgs)
glm.sdlg1 = glm(spp.no ~ perc.cover*location+nest ,data=sdlg.glm.data,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg1)

glm.sdlg2 = glm(spp.no ~ location+nest ,data=sdlg.glm.data,family=poisson) #Recall * is syntax syntax shortcue of both main effects + interaction
summary(glm.sdlg2)

AIC(glm.sdlg1,glm.sdlg2)
anova(glm.sdlg1,glm.sdlg2,test="Chisq")

###############
### VISUALIZATIONS
###############
plot.sdlg.no<-ggplot(sdlg.glm.data, aes(x = perc.cover, y = sdlg.no, col=location, fill=location)) + 
geom_point(shape=16, size = 3) +
ylab("no of seedlings") +
xlab("canopy cover (%)")+
ggtitle("A")+
#scale_colour_hue(l=50) + # Use a slightly darker palette than normal
geom_smooth(method=lm,se=FALSE)   # Add linear regression lines
plot.sdlg.no<-plot.sdlg.no + scale_colour_manual(values=c("darkblue", "darkred", "black"))  #I chose my own colors for the lines
#plot.sdlg.no<-plot.sdlg.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
plot.sdlg.no<-plot.sdlg.no + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
plot.sdlg.no<- plot.sdlg.no + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        #legend.position = 'none',
                                           legend.title = element_blank(),   #Removes the Legend title
                                            legend.text = element_text(color="black", size=16),  
                                            legend.position = c(0.23,0.7),
                                            legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
#guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

plot.sdlg.no



##########
## SPP NUMBER
##########

plot.spp.no<-ggplot(sdlg.glm.data, aes(x = perc.cover, y = spp.no, col=location, fill=location)) + 
geom_point(shape=16, size = 3) +
ylab("no of species") +
xlab("canopy cover (%)")+
ggtitle("B")+
#scale_colour_hue(l=50) + # Use a slightly darker palette than normal
geom_smooth(method=lm,se=FALSE)    # Add linear regression lines  


plot.spp.no<-plot.spp.no + scale_colour_manual(values=c("darkblue", "darkred", "black"))  #I chose my own colors for the lines
#plot.spp.no<-plot.spp.no + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
plot.spp.no<-plot.spp.no + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
plot.spp.no<- plot.spp.no + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        #legend.position = 'none',
                                           legend.title = element_blank(),   #Removes the Legend title
                                            legend.text = element_text(color="black", size=16),  
                                            legend.position = c(0.23,0.7),
                                            legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
#guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

plot.spp.no








###############
### SANDBOX
###############


#need to spread data into wide form to take the diff between the nest and far plots
wide1<-select(sdlgs, nest, location, count)
wide1<-spread(wide1,location,count, fill=0)
wide2<-select(sdlgs, nest, location, spp.df)
wide2<-spread(sdlgs,location,spp.dff, fill=0)







foo3<-select(VEG_both_summary, nest, location, species)
foo3<-group_by(foo3, nest, location)

foo3<-foo2 %>% 
  group_by(species) %>%
  summarise(no_rows = length(species))





foo2<-VEG_both_summary %>%
  gather(nest, location, "species", 8) %>% 
  group_by(owner,observation, Val) %>% 
  summarise(n= n()) %>%
  ungroup() %>%
  spread(Val, n, fill=0)


VEG_both_summary %>% 
  summarise(VEG_both_summary, group_by(location, nest), ht_cm=sum(ht_cm))
  
  group_by(location) %>% 
  summarise(ht_cm=sum(ht_cm))


foo<-group_by(VEG_both_summary, location)
foo<-VEG_both_summary %>%  
  group_by(location) %>% 
  summarise(mean(ht_cm))
foo

foo<-VEG_both_summary %>% group_by(location) %>% summarise(ht_cm=sum(ht_cm))

summarise(iris, avg = mean(Sepal.Length))

  
  group_by(nest) & group_by(location) %>% summarise(maxht = max(ht_cm)) %>% arrange(maxht)


VEG_both_summary 

summarise(VEG_both, avg = mean(ht_cm))


summarise(VEG_both, avg = mean(ht_cm))









##########################
####### FIGURES ##########
##########################



###########
## grass plot Cerrado Ralo
###########

grass.plotCR<-ggplot(NEST.DATA_CR, aes(x=location, y=grass.bmass)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  geom_boxplot() + stat_boxplot(geom ='errorbar')+
  ylab("grass biomass (g)") +  
  xlab("Plot Location")
grass.plotCR<-grass.plotCR+ggtitle("A) Cerrado Ralo")
#grass.plotCR<-grass.plotCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#grass.plotCR<-grass.plotCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
grass.plotCR<-grass.plotCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,2,1), "cm"))

grass.plotCR

###########
## grass plot Cerrado Denso
###########
grass.plotCD<-ggplot(NEST.DATA_CD, aes(x=location, y=grass.bmass)) + geom_boxplot()+
  ylab("grass biomass (g)") +  
  xlab("Plot Location")
grass.plotCD<-grass.plotCD+ggtitle("B) Cerrado Denso")
#grass.plotCD<-grass.plotCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#grass.plotCD<-grass.plotCD + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
grass.plotCD<-grass.plotCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,2,1), "cm"))

grass.plotCD

main = textGrob("Grass Biomass", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
FigGrBmass<-grid.arrange(grass.plotCR,grass.plotCD, sub=main, ncol=2, nrow=1) 






## Air Temp and Humidity near and far from nests in CR and CD over the course of 24 hours

# count.ATH<-as.data.frame(table(AIRTEMPHUMID$nest, AIRTEMPHUMID$habitat)) #This is just to see how many nests in each habitat type
ATH.FIG<-na.omit(AIRTEMPHUMID)  #create a dataframe to make this figure

# #Time needs to be a factor so you can plot it as the x axis
# ATH.FIG$time<-as.factor(ATH.FIG$time)

#change code to complete names so figure legends look nicer
levels(ATH.FIG$habitat)[levels(ATH.FIG$habitat)=="CR"]<-"Cerrado Ralo"  
levels(ATH.FIG$habitat)[levels(ATH.FIG$habitat)=="CD"]<-"Cerrado Denso"  
levels(ATH.FIG$location)[levels(ATH.FIG$location)=="away"]<-"10m from nest"  
levels(ATH.FIG$location)[levels(ATH.FIG$location)=="nest"]<-"Center of nest"  

# Calculate the average of air temp for nests in each habitat in each location at each time point
ATH.FIG<-ATH.FIG %>% group_by(habitat, location, time) %>% summarise(mean(temp), sd(temp), mean(air.humid), sd(air.humid))
ATH.FIG<-as.data.frame(ATH.FIG)
#rename the columns
colnames(ATH.FIG)[4]<-c("Temperature")
colnames(ATH.FIG)[5]<-c("SD.temperature")
colnames(ATH.FIG)[6]<-c("Humidity")
colnames(ATH.FIG)[7]<-c("SD.humidity")
colnames(ATH.FIG)[1]<-c("Habitat")
colnames(ATH.FIG)[2]<-c("Location")


# Use ggplot to make the figures.  this one is a more basic qplot. The following line greates a line plot of publication 
#this will shoft the error bars a bit so they don't overlap with "position dodge"














#Number of plants in each plot  group by nest and location of plot and count 
count.df<-VEG_both %>%
  group_by(nest, location) %>%
  summarise(count = n())
count.df


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


##########################################
## SANDBOX
##########################################

#Facet plot

pd<-position_dodge(0.4)

AirTempFig<- ggplot(ATH.FIG, aes(x = time, y = Temperature, color = Location)) +
  geom_errorbar(aes(ymin=Temperature-SD.temperature, ymax=Temperature+SD.temperature), width=.75, position=pd)+
  ylab("Air temperature (mean ± SD)") +  
  xlab("Hour") +
  geom_line()+geom_point(size=3) +
  facet_wrap( ~ Habitat, nrow=1, scales="fixed")     #scales="free_x" keeps the same y axis for all facets but allows you to modify x axis

AirTempFig<-AirTempFig + scale_colour_manual(values=c("red4", "darkblue"))  #I chose my own colors for the lines
# AirTempFig<-AirTempFig + geom_line(aes(group=factor(location)),size=.75)  #Changes the thickness of the lines
AirTempFig<-AirTempFig + scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23))
AirTempFig<-AirTempFig + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                          axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                          axis.text=element_text(colour="black", size = 16),
                                          strip.text.x = element_text(size=16, colour="black", vjust=3, face="bold"),
                                          plot.margin = unit(c(1,3,3,1), "cm"),   
                                          panel.margin = unit(1.5, "lines"),    #Adds more space between the facets (=panels)
                                          strip.background = element_blank(),
                                          legend.title=element_blank(),
                                          legend.text = element_text(color="black", size=16),
                                          legend.key = element_blank(),  #removes box around legend subunits
                                          legend.position = c(0.97,0.9),   #plot margin
                                          panel.border = element_rect(fill=NA, colour = "black", size=1, linetype="solid"))+
  guides(colour=guide_legend(override.aes=list(size=3)))+                        #Removes the boxes around legend colors       
  guides(fill = guide_legend(reverse=TRUE))
AirTempFig



