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
#canopy cover Cerrado Ralo
###########
my_grob_CR = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))

CoverCR<-ggplot(NEST.DATA_CR, aes(x=location, y=perc.cover)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("Camopy cover (mean % ± SD)") +  
  #   xlab("Plot Location")+
  annotation_custom(my_grob_CR)
CoverCR<-CoverCR+ggtitle("Cerrado Ralo")
#CoverCR<-CoverCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#CoverCR<-CoverCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
CoverCR<-CoverCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
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

CoverCR

###########
## canopy cover Cerrado Denso
###########
my_grob_CR = grobTree(textGrob("B", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))


CoverCD<-ggplot(NEST.DATA_CD, aes(x=location, y=perc.cover)) +
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("grass biomass (g)") +  
  #   xlab("Plot Location")+
  annotation_custom(my_grob_CR)
CoverCD<-CoverCD+ggtitle("Cerrado Denso")
#CoverCD<-CoverCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
#CoverCD<-CoverCD + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
CoverCD<-CoverCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(face="bold", size=18),        #Sets title size, style, location
                                                 axis.title.x = element_blank(),  #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                                 legend.text = element_text(color="black", size=16),
                                                 plot.margin =unit(c(1,1,1,1), "cm"))

CoverCD

########
## Arranging all in one plot
########

main = textGrob("Canopy Cover", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
FigCanopyCOver<-grid.arrange(CoverCR,CoverCD,sub=main, ncol=2, nrow=1) 




