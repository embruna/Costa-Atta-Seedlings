library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)

#CLear out everything from the environment
rm(list=ls())

################################################################################################
################################################################################################
### DATA ENTRY AND CLEANUP
################################################################################################
################################################################################################
#Step 1: load the individual CSV files and save them as dataframes
setwd("/Users/emiliobruna/Dropbox/Alan/Data/Capitulo2")
NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
VEG<-read.csv("ActiveNests_CensoVeg_1.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
AIRTEMPHUMID<-read.csv("ActiveNests_TempAirHumid_7.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )

#make plot locations an ordered factor nest<adjacent<far
NEST.DATA$location=factor(NEST.DATA$location, levels=c("nest","adjacent", "far"), ordered=TRUE)
#Select on Cerrado Denso and Cerrado Ralo
NEST.DATA.FIG<-filter(NEST.DATA, habitat == "CD" | habitat == "CR")
#change codes to complete names so figure legends look nicer
levels(NEST.DATA.FIG)[levels(NEST.DATA.FIG)=="CR"]<-"Cerrado Ralo"  
levels(NEST.DATA.FIG)[levels(NEST.DATA.FIG)=="CD"]<-"Cerrado Denso"
levels(NEST.DATA.FIG)[levels(NEST.DATA.FIG)=="far"]<-"10m from nest"  
levels(NEST.DATA.FIG)[levels(NEST.DATA.FIG)=="nest"]<-"Center of nest"  
levels(NEST.DATA.FIG)[levels(NEST.DATA.FIG)=="adjacent"]<-"Adjacent to nest" 

colnames(NEST.DATA.FIG)[colnames(NEST.DATA.FIG)=="grass.bmass"] <- "Grass"
colnames(NEST.DATA.FIG)[colnames(NEST.DATA.FIG)=="litter.bmass"] <- "Litter"
colnames(NEST.DATA.FIG)[colnames(NEST.DATA.FIG)=="soil.pen"] <- "Hardness"
colnames(NEST.DATA.FIG)[colnames(NEST.DATA.FIG)=="soil.humid.surface"] <- "Humidity"
colnames(NEST.DATA.FIG)[colnames(NEST.DATA.FIG)=="peak.soil.temp"] <- "Temperature"

################################################################################################
################################################################################################
### FACET PLOT: BIOMASS
################################################################################################
################################################################################################

Env1<-select(NEST.DATA.FIG,location, habitat,  perc.cover, Grass, Litter)
Env1<-gather(Env1, "variable", "value", 4:5)

# my_grob_ENV1 = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))


EnvFig1<-ggplot(Env1, aes(x=perc.cover, y=value, col=location, fill=location)) + 
  geom_point(shape=16, size=3)+
#   facet_grid(variable ~ .)+
  facet_wrap(~variable,nrow = 2,scales = "free")+
  ylab("Biomass (g)") +  
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines, Don't add shaded confidence region
# +annotation_custom(my_grob_ENV1)
EnvFig1<-EnvFig1 + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
EnvFig1<-EnvFig1 + scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(-10, 1500))
EnvFig1<-EnvFig1 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))



EnvFig1<- EnvFig1 + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        panel.margin = unit(2, "lines"), #space between facets
        axis.line = element_line(colour = "black"), #sets colors of axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),
        legend.position = c(0.8,0.85),
        strip.text.x = element_text(size=18, colour="black", face="bold", vjust=-1, hjust=.1),
        strip.background = element_blank(),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'), #box around legend
        plot.margin =unit(c(0,1,2,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

EnvFig1


################################################################################################
################################################################################################
### FACET PLOT: SOIL TEMP & HUMIDITY & HARDNESS
################################################################################################
################################################################################################



Env2<-select(NEST.DATA.FIG,location, habitat, perc.cover,  Hardness,  Humidity,  Temperature)
Env2<-gather(Env2, "variable", "value", 4:6)

# my_grob_ENV1 = grobTree(textGrob("A", x=0.05,  y=.95, gp=gpar(col="black", fontsize=18, fontface="bold")))


EnvFig2<-ggplot(Env2, aes(x=perc.cover, y=value, col=location, fill=location)) + 
  geom_point(shape=16, size=3)+
  #   facet_grid(variable ~ .)+
  facet_wrap(~variable,nrow = 3,scales = "free")+
  ylab("Value") +  
  xlab("Canopy cover (%)")+
  geom_smooth(method=lm,se=FALSE)   # Add linear regression lines, Don't add shaded confidence region
# +annotation_custom(my_grob_ENV1)
EnvFig2<-EnvFig2 + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
#EnvFig2<-EnvFig2 + scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(-10, 1500))
EnvFig2<-EnvFig2 + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))



EnvFig2<- EnvFig2 + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        panel.margin = unit(2, "lines"), #space between facets
        axis.line = element_line(colour = "black"), #sets colors of axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),
        legend.position = c(0.8,0.85),
        strip.text.x = element_text(size=18, colour="black", face="bold", vjust=-1, hjust=.1),
        strip.background = element_blank(),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'), #box around legend
        plot.margin =unit(c(0,1,2,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

EnvFig2







################################################################################################
################################################################################################
### INDIVIDUAL PLOTS 
################################################################################################
################################################################################################



##########################
#####PLOT: LITTER BIOMASS
##########################
litter.fig<-ggplot(NEST.DATA.FIG, aes(x = perc.cover, y = Litter, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("litter biomass (mean g ± SD)") +
  xlab("canopy cover (%)")+
  ggtitle("A")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region
litter.fig<-litter.fig + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
litter.fig<-litter.fig + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
litter.fig<-litter.fig + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
litter.fig<- litter.fig + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
        legend.position = 'none',
        #                                   legend.title = element_blank(),   #Removes the Legend title
        #                                    legend.text = element_text(color="black", size=16),  
        #                                    legend.position = c(0.23,0.7),
        #                                    legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) #+  #plot margin - top, right, bottom, left
#guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

litter.fig


##########################
#####PLOT: GRASS BIOMASS
##########################
grass.fig<-ggplot(NEST.DATA.FIG, aes(x = perc.cover, y = Grass, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("grass biomass (g)") +
  xlab("canopy cover (%)")+
  ggtitle("B")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region
grass.fig<-grass.fig + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
# grass.fig<-grass.fig + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
grass.fig<-grass.fig + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
grass.fig<- grass.fig + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        legend.position = c(.9,.8),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        # legend.position = 'none',
        plot.margin =unit(c(0,1,2,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

grass.fig



##########################
#####PLOT: SOIL HARDNESS
##########################
soil.pen<-ggplot(NEST.DATA.FIG, aes(x = perc.cover, y = Hardness, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("soil penetration (mm)") +
  xlab("canopy cover (%)")+
  ggtitle("C")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region
soil.pen<-soil.pen + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
# soil.pen<-soil.pen + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
soil.pen<-soil.pen + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
soil.pen<- soil.pen + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        #         legend.title = element_blank(),   #Removes the Legend title
        #         legend.text = element_text(color="black", size=16),  
        #         legend.position = c(0.23,0.7),
        #         legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        legend.position = 'none',
        plot.margin =unit(c(0,1,0,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

soil.pen



##########################
#####PLOT: SURFACE SOIL HUMIDITY
##########################
soil.humid<-ggplot(NEST.DATA.FIG, aes(x = perc.cover, y = Humidity, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("surface soil humidity (%)") +
  xlab("canopy cover (%)")+
  ggtitle("D")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region
soil.humid<-soil.humid + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
# soil.humid<-soil.humid + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
soil.humid<-soil.humid + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
soil.humid<- soil.humid + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        legend.title = element_blank(),   #Removes the Legend title
        legend.text = element_text(color="black", size=16),  
        legend.position = c(0.3,0.87),
        legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        # legend.position = 'none'),
        plot.margin =unit(c(0,1,0,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

soil.humid



##########################
#####PLOT: SURFACE TEMP
##########################
peak.soil.temp<-ggplot(NEST.DATA.FIG, aes(x = perc.cover, y = Temperature, col=location, fill=location)) + 
  geom_point(shape=16, size = 3) +
  ylab("soil temperature (°C))") +
  xlab("canopy cover (%)")+
  ggtitle("E")+
  #scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)    # Don't add shaded confidence region
peak.soil.temp<-peak.soil.temp + scale_colour_manual(values=c("darkred", "black", "darkblue"))  #I chose my own colors for the lines
# peak.soil.temp<-peak.soil.temp + scale_y_continuous(breaks = seq(0, 1400, 200), limits = c(-10, 1400))
peak.soil.temp<-peak.soil.temp + scale_x_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))
peak.soil.temp<- peak.soil.temp + theme_classic()+
  theme(plot.title = element_text(face="bold", size=18, vjust=-3.5, hjust=0.05),        #Sets title size, style, location
        axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
        axis.text=element_text(colour="black", size = 16),                             #sets size and style of labels on axes
        #         legend.title = element_blank(),   #Removes the Legend title
        #         legend.text = element_text(color="black", size=16),  
        #         legend.position = c(0.23,0.7),
        #         legend.background = element_rect(colour = 'black', size = 0.5, linetype='solid'),
        legend.position = 'none',
        plot.margin =unit(c(0,1,2,1.5), "cm")) +  #plot margin - top, right, bottom, left
  guides(colour=guide_legend(override.aes=list(size=4, linetype=0)))  #size of legen bars    

peak.soil.temp


##########################
#####PALL TOGETHER
##########################
main = textGrob("ENVT CHAR as FUNCTION OF % CANOPY COVER", vjust = 0, gp = gpar(fontface = "bold", fontsize = 20))
Soil.Fig<-grid.arrange(soil.pen, soil.humid, peak.soil.temp, sub=main, ncol=2, nrow=2)

main = textGrob("ENVT CHAR as FUNCTION OF % CANOPY COVER", vjust = 1.5, gp = gpar(fontface = "bold", fontsize = 20))
Bmass.Fig<-grid.arrange(litter.fig, grass.fig, sub=main, ncol=1, nrow=3)


