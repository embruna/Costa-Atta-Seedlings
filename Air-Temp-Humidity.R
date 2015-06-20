library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)

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
#####
## Cerrado Ralo Temp
#####
#this will shoft the error bars a bit so they don't overlap with "position dodge"
pd<-position_dodge(0.4)
#Line plot
#note the filter means only doing with nest plots
my_grob_CR = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=15, fontface="bold")))


TempCR<-ggplot(filter(ATH.FIG, Habitat=="Cerrado Ralo"), aes(x = time, y = Temperature, col=Location)) + 
  geom_errorbar(aes(ymin=Temperature-SD.temperature, ymax=Temperature+SD.temperature), width=.75, position=pd)+
  ylab("Air temperature (mean °C ± SD)") + 
  xlab("Hour")+geom_line()+geom_point(size=3)+
  annotation_custom(my_grob_CR)
TempCR<-TempCR+ggtitle("Cerrado ralo")
TempCR<-TempCR + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
TempCR<-TempCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
TempCR<-TempCR + scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50))
TempCR<- TempCR + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                   plot.title = element_text(face="bold", size=20),        #Sets title size, style, location
                                   axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                   legend.title = element_blank(),                                  #Removes the Legend title
                                   legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                   legend.text = element_text(color="black", size=16),
                                   plot.margin =unit(c(1,1,1,1), "cm"),
                                   legend.position = "none",
                                   legend.position = c(0.9,0.5))+   #plot margin
  guides(colour=guide_legend(override.aes=list(size=3)))                        #size of legen bars    

TempCR
#####
## Cerrado Denso Temp
#####
pd<-position_dodge(0.4)
#Line plot
my_grob_CD = grobTree(textGrob("B", x=0.05,  y=.95, gp=gpar(col="black", fontsize=15, fontface="bold")))

#note the filter means only doing with nest plots
TempCD<-ggplot(filter(ATH.FIG, Habitat=="Cerrado Denso"), aes(x = time, y = Temperature, col=Location)) + 
  geom_errorbar(aes(ymin=Temperature-SD.temperature, ymax=Temperature+SD.temperature), width=.75, position=pd)+
  ylab("Air temperature (mean °C ± SD)") +  
  xlab("Hour")+geom_line()+geom_point(size=3)+
  annotation_custom(my_grob_CD) 

TempCD<-TempCD+ggtitle("Cerrado denso")
TempCD<-TempCD + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
TempCD<-TempCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
TempCD<-TempCD + scale_y_continuous(breaks = seq(0, 50, 10), limits = c(0, 50))
TempCD<- TempCD + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                   plot.title = element_text(face="bold", size=20),        #Sets title size, style, location
                                   axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                   legend.title = element_blank(),                                  #Removes the Legend title
                                   legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                   legend.text = element_text(color="black", size=16),
                                   plot.margin =unit(c(1,1,1,1), "cm"),
                                   legend.position = "none",
                                   legend.position = c(0.9,0.5))+   #plot margin
  guides(colour=guide_legend(override.aes=list(size=3)))                        #size of legen bars    

TempCD

#####
## Cerrado Ralo Humid
#####
#this will shoft the error bars a bit so they don't overlap with "position dodge"
pd<-position_dodge(0.4)
#Line plot
#note the filter means only doing with nest plots
HumidCR<-ggplot(filter(ATH.FIG, Habitat=="Cerrado Ralo"), aes(x = time, y = Humidity, col=Location)) + 
  geom_errorbar(aes(ymin=Humidity-SD.humidity, ymax=Humidity+SD.humidity), width=.75, position=pd)+
  ylab("Humidity (mean % ± SD)") +  
  xlab("Hour")+geom_line()+geom_point(size=3) 

HumidCR<-HumidCR+ggtitle("C")
HumidCR<-HumidCR + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
HumidCR<-HumidCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
HumidCR<-HumidCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
HumidCR<- HumidCR + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                     plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=15),        #Sets title size, style, location
                                     axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                     axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                     axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                     legend.title = element_blank(),                                  #Removes the Legend title
                                     legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                     legend.text = element_text(color="black", size=16),
                                     plot.margin =unit(c(1,1,2,1), "cm"),
                                     legend.position = "none",
                                     legend.position = c(0.9,0.5))+   #plot margin
  guides(colour=guide_legend(override.aes=list(size=3)))                        #size of legen bars    

HumidCR
#####
## Cerrado Denso Humid
#####
pd<-position_dodge(0.4)
#Line plot
#note the filter means only doing with nest plots
HumidCD<-ggplot(filter(ATH.FIG, Habitat=="Cerrado Denso"), aes(x = time, y = Humidity, col=Location)) + 
  geom_errorbar(aes(ymin=Humidity-SD.humidity, ymax=Humidity+SD.humidity), width=.75, position=pd)+
  ylab("Humidity (mean % ± SD)") +  
  xlab("Hour")+geom_line()+geom_point(size=3) 

HumidCD<-HumidCD+ggtitle("D")
HumidCD<-HumidCD + scale_colour_manual(values=c("darkred", "darkblue"))  #I chose my own colors for the lines
HumidCD<-HumidCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
HumidCD<-HumidCD + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
HumidCD<- HumidCD + theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                     plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=15),        #Sets title size, style, location
                                     axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                     axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                     axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                     legend.title = element_blank(),                                  #Removes the Legend title
                                     legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                     legend.text = element_text(color="black", size=16),
                                     plot.margin =unit(c(1,1,2,1), "cm"),
                                     legend.position = c(0.7,0.9))+   #plot margin
  guides(colour=guide_legend(override.aes=list(size=3)))                        #size of legen bars    

HumidCD

main = textGrob("Air Temperature and Humidity Near and Far from Nests", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
FigATH<-grid.arrange(TempCR,TempCD, HumidCR,HumidCD, sub=main, ncol=2, nrow=2)