library(ggplot2)
library(dplyr)
library(tidyr)

#CLear out everything from the environment
rm(list=ls())

setwd("/Users/emiliobruna/Dropbox/SHARED FOLDERS/Alan/Data/Capitulo2")

NEST.DATA<-read.csv("ActiveNests_data_2-3-4-5-6.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
#SElect data for each histogram
HIST.DATA<-select(NEST.DATA, nest, location, habitat, perc.cover)
HIST.DATA<-subset(HIST.DATA, habitat=="CD" | habitat =="CR")
#change codes to complete names so figure legends look nicer
levels(HIST.DATA$habitat)[levels(HIST.DATA$habitat)=="CR"]<-"Cerrado Ralo"  
levels(HIST.DATA$habitat)[levels(HIST.DATA$habitat)=="CD"]<-"Cerrado Denso"

perc.cover.fig<-ggplot(HIST.DATA, aes(x=perc.cover, fill=habitat)) +
  geom_histogram(binwidth=5, alpha=.7, position="identity", colour="black")+
  scale_fill_manual("",values = c('black', 'lightgray'))+
  ylab("Count") + 
  xlab("Canopy Cover (%)")

perc.cover.fig<- perc.cover.fig + theme_classic()+theme(plot.title = element_text(face="bold", size=20),        #Sets title size, style, location
                                   legend.position="top",
                                   axis.title.x=element_text(colour="black", size = 20, vjust=-0.5),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.title.y=element_text(colour="black", size = 20, vjust=0.5),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                   axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                   legend.title = element_blank(),                                  #Removes the Legend title
                                   legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                   legend.text = element_text(color="black", size=16),
                                   legend.position = "none",
                                   legend.key = element_rect(colour = "black"), #puts black line around legend box
                                   legend.position = c(0.9,0.5))+   #plot margin
  guides(fill = guide_legend(override.aes = list(colour = NULL))) #remove slash from legend


perc.cover.fig

##############################
# BOX PLOT BY LOCATION OF PLOT
##############################
#Rename the levels so they look nice on the graph
levels(HIST.DATA$location)[levels(HIST.DATA$location)=="far"]<-"10m from nest"  
levels(HIST.DATA$location)[levels(HIST.DATA$location)=="nest"]<-"Center of nest"  
levels(HIST.DATA$location)[levels(HIST.DATA$location)=="adjacent"]<-"Adjacent to nest" 
#reorder them so the appear on graph from nest to furthest away
levels(HIST.DATA$location) = c("Center of nest", "Adjacent to nest","10m from nest" )


#Set up the boxplot
perc.cover.loc<-ggplot(HIST.DATA, aes(x=location, y=perc.cover, fill =location)) + 
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  geom_boxplot(outlier.colour = "darkblue", outlier.size = 3)+
  ylab("Percent Canopy Cover ")

# modofy the box plot
perc.cover.loc<-perc.cover.loc + scale_y_continuous(breaks = seq(0, 100, 20), limits = c(-10, 100))+
  scale_fill_manual(values = c("darkred", "gray", "blue2"))

perc.cover.loc<-perc.cover.loc+theme_classic()+theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
                                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), #sets colors of axes
                                             plot.title = element_text(face="bold", size=18, vjust=2),        #Sets title size, style, location
                                             axis.title.x = element_blank(),
                                             #axis.title.x=element_text(colour="black", size = 18, vjust=-2),            #sets x axis title size, style, distance from axis #add , face = "bold" if you want bold
                                             axis.title.y=element_text(colour="black", size = 18, vjust=2),            #sets y axis title size, style, distance from axis #add , face = "bold" if you want bold
                                             axis.text=element_text(colour="black", size = 16),                              #sets size and style of labels on axes
                                             legend.title = element_blank(),                                  #Removes the Legend title
                                             legend.key = element_blank(),                                  #Removes the boxes around legend colors
                                             legend.text = element_text(color="black", size=16))
perc.cover.loc


# ANalysis - is there an effect of nest proxity (main or interaction) on % cover?


HIST.DATA<-na.omit(HIST.DATA)

HIST.DATA$prop.cover<-(HIST.DATA$perc.cover/100)+0.01

# use bionomial or quasibinomial?
# http://stats.stackexchange.com/questions/134783/glm-for-proportional-data-and-underdispersion

# Just intercept
glm.cover0<-glm(prop.cover~1, HIST.DATA,family=binomial)  
summary(glm.cover0)

# Main effect of location
glm.cover1<-glm(prop.cover~location, HIST.DATA,family=binomial)  
summary(glm.cover1)

# Just the Nest Effect
glm.cover2<-glm(prop.cover~nest, HIST.DATA,family=binomial)
summary(glm.cover2)                    

# Just the habitat Effect
# glm.cover3<-glm(prop.cover~habitat, HIST.DATA,family=binomial)
# summary(glm.cover3)        


# effect of location and nest
glm.cover4<-glm(prop.cover~location+nest, HIST.DATA,family=binomial)
summary(glm.cover4)
# 

# effect of location, habitat, their interaction, and nest [block]
# glm.cover5<-glm(prop.cover~location*habitat+nest, HIST.DATA,family=binomial)
# summary(glm.cover5)

AIC(glm.cover0,glm.cover1, glm.cover2, glm.cover4)

anova(glm.cover4,glm.cover5,test="Chisq")


aov1<-aov(log(prop.cover/(1-prop.cover))~location*habitat+nest, HIST.DATA)
summary(aov1)

perc.cover.loc




summary(filter(HIST.DATA, habitat=="Cerrado Denso"))
summary(filter(HIST.DATA, habitat=="Cerrado Ralo"))
#pos="dodge"

