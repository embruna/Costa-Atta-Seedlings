library(ggplot2)
library(dplyr)
library(tidyr)

#CLear out everything from the environment
rm(list=ls())



NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#SElect data for each histogram
HIST.DATA<-select(NEST.DATA, habitat, perc.cover)
HIST.DATA<-subset(HIST.DATA, habitat=="CD" | habitat =="CR")
#change codes to complete names so figure legends look nicer
levels(HIST.DATA$habitat)[levels(HIST.DATA$habitat)=="CR"]<-"Cerrado Ralo"  
levels(HIST.DATA$habitat)[levels(HIST.DATA$habitat)=="CD"]<-"Cerrado Denso"

perc.cover.fig<-ggplot(HIST.DATA, aes(x=perc.cover, fill=habitat)) +
  geom_histogram(binwidth=5, alpha=.7, position="identity", colour="black")+
  scale_fill_manual("",values = c('darkblue', 'black'))+
  ylab("Count") + 
  xlab("Canopy Cover (%)")

perc.cover.fig<- perc.cover.fig + theme_classic()+theme(plot.title = element_text(face="bold", size=20),        #Sets title size, style, location
                                   legend.position="top",
                                   axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                   legend.title = element_blank(),                                  #Removes the Legend title
                                   legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                   legend.text = element_text(color="black", size=16),
                                   plot.margin =unit(c(1,1,1,1), "cm"),
                                   legend.position = "none",
                                   legend.key = element_rect(colour = "black"), #puts black line around legend box
                                   legend.position = c(0.9,0.5))+   #plot margin
  guides(fill = guide_legend(override.aes = list(colour = NULL))) #remove slash from legend


perc.cover.fig

#pos="dodge"
