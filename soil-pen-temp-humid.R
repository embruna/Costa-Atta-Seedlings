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
## soil pen Cerrado Ralo
###########
my_grob_CR = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))

soil.penCR<-ggplot(NEST.DATA_CR, aes(x=location, y=soil.pen)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("soil penetration (mm)") +  
  #   xlab("Plot Location")+
  annotation_custom(my_grob_CR)
soil.penCR<-soil.penCR+ggtitle("Cerrado Ralo")
#soil.penCR<-soil.penCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
soil.penCR<-soil.penCR + scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0, 14))
soil.penCR<-soil.penCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(face="bold", size=18, vjust=2),        #Sets title size, style, location
                                                 axis.title.x = element_blank(),
                                                 #axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                             plot.margin =unit(c(1,0,0,2), "cm"),   
                                             legend.text = element_text(color="black", size=16))

soil.penCR

###########
## soil pen Cerrado Denso
###########
my_grob_CD = grobTree(textGrob("B", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))


soil.penCD<-ggplot(NEST.DATA_CD, aes(x=location, y=soil.pen)) +
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("soil penetration (mm)") +
  #   xlab("Plot Location")+
  annotation_custom(my_grob_CD)
soil.penCD<-soil.penCD+ggtitle("Cerrado Denso")
#soil.penCD<-soil.penCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
soil.penCD<-soil.penCD + scale_y_continuous(breaks = seq(0, 14, 2), limits = c(0, 14))
soil.penCD<-soil.penCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                                 panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                                 plot.title = element_text(face="bold", size=18, vjust=2),        #Sets title size, style, location
                                                 axis.title.x = element_blank(),  #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                                 axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                                 legend.title = element_blank(),                                  #Removes the Legend title
                                                 legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                             plot.margin =unit(c(1,0,0,2), "cm"),
                                             legend.text = element_text(color="black", size=16))
                                              
soil.penCD


###########
##peak soil temp Cerrado Ralo
###########


peak.tempCR<-ggplot(NEST.DATA_CR, aes(x=location, y=peak.soil.temp)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("peak soil temp (°C)") +    
  xlab("Plot Location")
peak.tempCR<-peak.tempCR+ggtitle("C")
#peak.tempCR<-peak.tempCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
peak.tempCR<-peak.tempCR + scale_y_continuous(breaks = seq(25, 65, 10), limits = c(25, 65))
peak.tempCR<-peak.tempCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                               plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                               axis.title.x=element_blank(), #(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                               legend.title = element_blank(),                                  #Removes the Legend title
                                               legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                               plot.margin =unit(c(0,0,0,2), "cm"),                                              
                                               legend.text = element_text(color="black", size=16))
                                               

peak.tempCR


###########
## peak.temp Cerrado Denso
###########
peak.tempCD<-ggplot(NEST.DATA_CD, aes(x=location, y=peak.soil.temp)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("peak soil temp (°C)") +    
  xlab("Plot Location")
peak.tempCD<-peak.tempCD+ggtitle("D")
#peak,tempCD<-peak,tempCD + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
peak.tempCD<-peak.tempCD + scale_y_continuous(breaks = seq(25, 65, 10), limits = c(25, 65))
peak.tempCD<-peak.tempCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                               plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                               axis.title.x=element_blank(), #text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                               legend.title = element_blank(),                                  #Removes the Legend title
                                               legend.key = element_blank(),
                                               plot.margin =unit(c(0,0,0,2), "cm"))
                                               #legend.text = element_text(color="black", size=16), #Removes the boxes around legend colors
                                               
peak.tempCD




###########
##surf soil humid Cerrado Ralo
###########


humCR<-ggplot(NEST.DATA_CR, aes(x=location, y=soil.humid.surface)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("Surface soil humidity") +    
  xlab("Plot Location")
humCR<-humCR+ggtitle("E")
#peak,tempCR<-peak,tempCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
peak.tempCR<-peak.tempCR + scale_y_continuous(breaks = seq(2, 6, 2), limits = c(2, 6))
humCR<-humCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                               plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                              axis.title.x=element_blank(), #text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                               axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                               legend.title = element_blank(),                                  #Removes the Legend title
                                               legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                   plot.margin =unit(c(0,0,2,2), "cm"),
                                               legend.text = element_text(color="black", size=16))
                                                                                              

humCR


###########
## soil surf humid Cerrado Denso
###########


humCD<-ggplot(NEST.DATA_CR, aes(x=location, y=soil.humid.surface)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("Surface soil humidity") +    
  xlab("Plot Location")
humCD<-humCD+ggtitle("F")
#peak,tempCR<-peak,tempCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
peak.tempCR<-peak.tempCR + scale_y_continuous(breaks = seq(2, 6, 2), limits = c(2, 6))
humCD<-humCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                   plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
                                    axis.title.x=element_blank(), #(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                   legend.title = element_blank(),                                  #Removes the Legend title
                                   legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                   plot.margin =unit(c(0,0,2,2), "cm"),
                                  legend.text = element_text(color="black", size=16))
                                  

humCD




###########
## deep soil humid Cerrado Ralo
###########

# 
# deep.humCR<-ggplot(NEST.DATA_CR, aes(x=location, y=humid.soil.deep)) + 
#   geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
#   ylab("Deep soil humidity") +    
#   xlab("Plot Location")
# deep.humCR<-deep.humCR+ggtitle("G")
# #peak,tempCR<-peak,tempCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
# #peak,tempCR<-peak,tempCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
# deep.humCR<-deep.humCR+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
#                                    panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
#                                    plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
#                                    axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
#                                    axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
#                                    axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
#                                    legend.title = element_blank(),                                  #Removes the Legend title
#                                    legend.key = element_blank(),                                  #Removes the boxes around legend colors
#                                     plot.margin =unit(c(0,0,2,2), "cm"),
#                                    legend.text = element_text(color="black", size=16))
# 
# deep.humCR
# 
# 
# 
# ###########
# ## deep soil humid Cerrado Denso
# ###########
# 
# 
# deep.humCD<-ggplot(NEST.DATA_CR, aes(x=location, y=humid.soil.deep)) + 
#   geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
#   ylab("Deep soil humidity") +    
#   xlab("Plot Location")
# deep.humCD<-deep.humCD+ggtitle("H")
# #peak,tempCR<-peak,tempCR + scale_x_continuous(breaks = seq(0, 24, 2), limits = c(0, 24))
# #peak,tempCR<-peak,tempCR + scale_y_continuous(breaks = seq(0, 90, 15), limits = c(0, 90))
# deep.humCD<-deep.humCD+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
#                                              panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
#                                              plot.title = element_text(hjust=0.05, vjust=-1.8, face="bold", size=18),        #Sets title size, style, location
#                                              axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
#                                              axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
#                                              axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
#                                              legend.title = element_blank(),                                  #Removes the Legend title
#                                              legend.key = element_blank(),                                  #Removes the boxes around legend colors
#                                              plot.margin =unit(c(0,0,2,2), "cm"),
#                                              legend.text = element_text(color="black", size=16))
# 
# deep.humCD
# ########
# ## Arranging all in one plot
# ########

main = textGrob("Soil Penetration, Temp, and Humidty", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
FigSoil<-grid.arrange(soil.penCR, soil.penCD, peak.tempCR, peak.tempCD, humCR, humCD, sub=main, #deep.humCR, deep.humCD, 
                      ncol=2, nrow=3) 




