
#R CODE FOR IMPORTING, MANIPULATING, AND ANALYZING THE DATASETS USED IN: Costa el al MS


# library(gdata)
# library(lme4)
# library(reshape)
# library(maps)
# library(WDI)
# library(RColorBrewer)
# library(lattice)
# library(coefplot2) ## for coefplot2
# library(reshape)
# library(gridExtra)
# library(emdbook)
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
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



##########################
####### FIGURES ##########
##########################


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
pd<-position_dodge(0.4)

AirTempFig<- ggplot(ATH.FIG, aes(x = time, y = Temperature, color = location)) +
  geom_errorbar(aes(ymin=Temperature-SD.temperature, ymax=Temperature+SD.temperature), width=.75, position=pd)+
  ylab("Air temperature (nest plots, mean SD)") +xlab("Hour")+geom_line()+geom_point(size=3) +
  facet_wrap( ~ Habitat, nrow=1, scales="free_x")+ggtitle("A")      #scales="free_x" keeps the same y axis for all facets but allows you to modify x axis

AirTempFig<-AirTempFig + scale_colour_manual(values=c("red4", "darkblue"))  #I chose my own colors for the lines
AirTempFig<-AirTempFig + geom_line(aes(group=factor(location)),size=.75)  #Changes the thickness of the lines
AirTempFig<-AirTempFig + scale_x_continuous(breaks = seq(0, 23, 2), limits = c(0, 23))
AirTempFig<-AirTempFig + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                       plot.title = element_text(hjust=0.01, vjust=2, face="bold", size=20),        #Sets title size, style, location
                                       axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                       axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                       axis.text=element_text(colour="black", size = 16),
                             strip.text.x = element_text(size=16, colour="black", vjust=-1, face="bold"),
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


#Line plot
#note the filter means only doing with nest plots
MyATHfig<-ggplot(filter(ATH.FIG, Habitat=="Cerrado Ralo"), aes(x = time, y = Temperature, col=location)) + 
  geom_errorbar(aes(ymin=Temperature-SD.temperature, ymax=Temperature+SD.temperature), width=.75, position=pd)+
  ylab("Air temperature (nest plots, mean SD)") +xlab("Hour")+geom_line()+geom_point(size=3) 

MyATHfig<-MyATHfig+ggtitle("A) Cerrado ralo")
MyATHfig<-MyATHfig + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
MyATHfig<-MyATHfig + geom_line(aes(group=factor(location)),size=.75)  #Changes the thickness of the lines
MyATHfig<-MyATHfig + scale_x_continuous(breaks = seq(0, 23, 1), limits = c(0, 23))
MyATHfig<- MyATHfig + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                       plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=20),        #Sets title size, style, location
                                       axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                       axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                       axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                       legend.title = element_blank(),                                  #Removes the Legend title
                                       legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                       legend.text = element_text(color="black", size=16),
                                       plot.margin =unit(c(2,2,2,2), "cm"),
                                       legend.position = c(0.9,0.5))+   #plot margin
  guides(colour=guide_legend(override.aes=list(size=3)))                        #size of legen bars                                                      
MyATHfig

