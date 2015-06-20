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
#make plot locations an ordered factor nest<adjacent<far
NEST.DATA$location=factor(NEST.DATA$location, levels=c("nest","adjacent", "far"), ordered=TRUE)


# Use Cerrado Ralo (CR) and Cerrado Denso (CD) in analyse
NEST.DATA_both<-NEST.DATA[NEST.DATA$habitat=="CR"|NEST.DATA$habitat=="CD",] #both habitats
NEST.DATA_both <- droplevels(NEST.DATA_both)

NEST.DATA_CR<-NEST.DATA[NEST.DATA$habitat=="CR",] #only CR
NEST.DATA_CR <- droplevels(NEST.DATA_CR)

NEST.DATA_CD<-NEST.DATA[NEST.DATA$habitat=="CD",] #only CD
NEST.DATA_CD <- droplevels(NEST.DATA_CD)


##########################
####### FIGURES ##########
##########################



###########
## grass plot Cerrado Ralo
###########
my_grob_CR = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))

grass.plotCR<-ggplot(NEST.DATA_CR, aes(x=location, y=grass.bmass)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("grass biomass (g)") +  
#   xlab("Plot Location")+
annotation_custom(my_grob_CR)
grass.plotCR<-grass.plotCR+ggtitle("Cerrado Ralo")
#grass.plotCR<-grass.plotCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#grass.plotCR<-grass.plotCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
grass.plotCR<-grass.plotCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x = element_blank(),
                                                 #axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,1,1), "cm"))

grass.plotCR

###########
## grass plot Cerrado Denso
###########
my_grob_CR = grobTree(textGrob("B", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))


grass.plotCD<-ggplot(NEST.DATA_CD, aes(x=location, y=grass.bmass)) +
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("grass biomass (g)") +  
#   xlab("Plot Location")+
annotation_custom(my_grob_CR)
grass.plotCD<-grass.plotCD+ggtitle("Cerrado Denso")
#grass.plotCD<-grass.plotCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#grass.plotCD<-grass.plotCD + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
grass.plotCD<-grass.plotCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x = element_blank(),  #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,1,1), "cm"))

grass.plotCD


###########
## litter plot Cerrado Ralo
###########

litter.plotCR<-ggplot(NEST.DATA_CR, aes(x=location, y=litter.bmass)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  geom_boxplot()+
  ylab("litter biomass (g)") +  
  xlab("Plot Location")
litter.plotCR<-litter.plotCR+ggtitle("C")
#litter.plotCR<-litter.plotCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#litter.plotCR<-litter.plotCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
litter.plotCR<-litter.plotCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,2,1), "cm"))

litter.plotCR

###########
## litter plot Cerrado Denso
###########
litter.plotCD<-ggplot(NEST.DATA_CD, aes(x=location, y=litter.bmass)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("litter biomass (g)") +  
  xlab("Plot Location")
litter.plotCD<-litter.plotCD+ggtitle("D")
#litter.plotCD<-litter.plotCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#litter.plotCD<-litter.plotCD + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
litter.plotCD<-litter.plotCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,2,1), "cm"))

litter.plotCD


########
## Arranging all in one plot
########

main = textGrob("Grass and Litter Biomass", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
FigGrBmass<-grid.arrange(grass.plotCR,grass.plotCD, litter.plotCR,litter.plotCD, sub=main, ncol=2, nrow=2) 




