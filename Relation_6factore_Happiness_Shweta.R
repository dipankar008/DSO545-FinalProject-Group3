# Shweta Desai: analyzing relatioship with happiness and factores like
  ## 1. Corrpution
  ## 2. GDP per capita
  ## 3. Health life expectancy

library(ggThemeAssist)
library(rvest)
library(ggplot2)
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)

library(lubridate)
library(ggThemeAssist)
library(ggplot2)

library(anytime)


library(maps)
library(ggplot2)
library(dplyr)
library(mapproj)
library(ggmap)

library(readr)

library(ggrepel)


############################################### Funcation  ###################################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


######################################################################################################

whr = read.csv("WHR.csv")


whr_geo = whr
whr_geo$Country = as.character(whr_geo$Country)
whr_geo$Region = as.character(whr_geo$Region)


colnames(whr_geo)[colnames(whr_geo) == "Trust..Government.Corruption."] = "Government_Corruption"
colnames(whr_geo)[colnames(whr_geo) == "Economy..GDP.per.Capita."] = "GDP_per_Capita"

colnames(whr_geo)


####################################################################################################
####################################################################################################


## Corruption and Happiness relation : 2015
corrup_2015 = whr_geo %>%
  filter(Happiness.Rank <= 30) %>%
  arrange(Happiness.Rank) %>%
  filter(Year == 2015) %>% 
  ggplot() +
  ## happiness
  geom_point(aes(x=Happiness.Score, 
                 y=factor(desc(Happiness.Rank)), 
                 group=1),
             size=2, 
             color="red") +
  ## corruption
  geom_point(aes(x=Government_Corruption*25, 
                 y=factor(desc(Happiness.Rank)), group=1),
             size=2, 
             color="blue") +
  scale_x_continuous(breaks = seq(0.0,16.0,by = 0.5) , 
                     labels = seq(0.0,16.0,by=0.5),sec.axis = sec_axis(~.*5, 
                                                                       name = "Corruption",
                                                                       labels = seq(0,70,by=10),
                                                                       breaks = seq(0,70,by = 10)))  +
  ggtitle("Happiness Score and Corruption Comparition with Rank-2015") +
  xlab("Happiness Score")+
  ylab("Happiness Rank")+
  ## Country name
  geom_label_repel(
    aes(Government_Corruption*25, factor(desc(Happiness.Rank)), label = Country),
    box.padding = 0.30, 
    point.padding = 0.1,
    segment.color = 'grey50') 


## corruption index for 2016
corrup_2016 = whr_geo %>%
  filter(Happiness.Rank <= 30) %>%
  arrange(Happiness.Rank) %>%
  filter(Year == 2016) %>% 
  ggplot() +
  ## happiness
  geom_point(aes(x=Happiness.Score, 
                 y=factor(desc(Happiness.Rank)), 
                 group=1),
             size=2, 
             color="red") +
  ## corruption
  geom_point(aes(x=Government_Corruption*25, 
                 y=factor(desc(Happiness.Rank)), group=1),
             size=2, 
             color="blue") +
  scale_x_continuous(breaks = seq(0.0,16.0,by = 0.5) , 
                     labels = seq(0.0,16.0,by=0.5),sec.axis = sec_axis(~.*5, 
                                                                       name = "Corruption",
                                                                       labels = seq(0,70,by=10),
                                                                       breaks = seq(0,70,by = 10)))  +
  ggtitle("Happiness Score and Corruption Comparition with Rank-2016") +
  xlab("Happiness Score")+
  ylab("Happiness Rank")+
  ## Country name
  geom_label_repel(
    aes(Government_Corruption*25, factor(desc(Happiness.Rank)), label = Country),
    box.padding = 0.30, 
    point.padding = 0.1,
    segment.color = 'grey50') 




## 2017 : Corruption index for 2017 
corrup_2017 = whr_geo %>%
  filter(Happiness.Rank <= 30) %>%
  arrange(Happiness.Rank) %>%
  filter(Year == 2017) %>% 
  ggplot() +
  ## happiness
  geom_point(aes(x=Happiness.Score, 
                 y=factor(desc(Happiness.Rank)), 
                 group=1),
             size=2, 
             color="red") +
  ## corruption
  geom_point(aes(x=Government_Corruption*25, 
                 y=factor(desc(Happiness.Rank)), group=1),
             size=2, 
             color="blue") +
  scale_x_continuous(breaks = seq(0.0,16.0,by = 0.5) , 
                     labels = seq(0.0,16.0,by=0.5),sec.axis = sec_axis(~.*5, 
                                                                       name = "Corruption",
                                                                       labels = seq(0,70,by=10),
                                                                       breaks = seq(0,70,by = 10)))  +
  ggtitle("Happiness Score and Corruption Comparition with Rank- 2017") +
  xlab("Happiness Score")+
  ylab("Happiness Rank")+
  ## Country name
  geom_label_repel(
    aes(Government_Corruption*25, factor(desc(Happiness.Rank)), label = Country),
    box.padding = 0.10, 
    point.padding = 0.5,
    segment.color = 'gray50',
    segment.colour="black") 

## display all 3 years on same 

multiplot(corrup_2015,corrup_2016,corrup_2017,cols=2)



############################################################################################################
#########################  GDP per Capita ##################################################################
###########################################################################################################


gdp_2015 = whr_geo %>%
  filter(Happiness.Rank <= 30) %>%
  arrange(Happiness.Rank) %>%
  filter(Year == 2015) %>% 
  ggplot() +
  ## happiness
  geom_point(aes(x=Happiness.Score, 
                 y=factor(desc(Happiness.Rank)), 
                 group=1),
             size=2, 
             color="red") +
  ## corruption
  geom_point(aes(x=GDP_per_Capita*7, 
                 y=factor(desc(Happiness.Rank)), group=1),
             size=2, 
             color="blue") +
  scale_x_continuous(breaks = seq(0,17,by = 0.5) , 
                     labels = seq(0,17,by=0.5),sec.axis = sec_axis(~.*1, 
                                                                    name = "GDP per Capita",
                                                                    labels = round(seq(0,17,by=0.5)/7,digit=2),
                                                                    breaks = seq(0,17,by =0.5))) +
  ggtitle("Happiness Score and 'GDP per Capita' Comparition with Rank-2015") +
  xlab("Happiness Score")+
  ylab("Happiness Rank") +
  ## Country name
  geom_label_repel(
    aes(GDP_per_Capita*7, factor(desc(Happiness.Rank)), label = Country),
    box.padding = 0.30, 
    point.padding = 0.3,
    segment.color = 'grey50') 


gdp_2016 = whr_geo %>%
  filter(Happiness.Rank <= 30) %>%
  arrange(Happiness.Rank) %>%
  filter(Year == 2016) %>% 
  ggplot() +
  ## happiness
  geom_point(aes(x=Happiness.Score, 
                 y=factor(desc(Happiness.Rank)), 
                 group=1),
             size=2, 
             color="red") +
  ## GDP per Capita
  geom_point(aes(x=GDP_per_Capita*7, 
                 y=factor(desc(Happiness.Rank)), group=1),
             size=2, 
             color="blue") +
  scale_x_continuous(breaks = seq(0,17,by = 0.5) , 
                     labels = seq(0,17,by=0.5),sec.axis = sec_axis(~.*1, 
                                                                   name = "GDP per Capita",
                                                                   labels = round(seq(0,17,by=0.5)/7,digit=2),
                                                                   breaks = seq(0,17,by =0.5))) +
  ggtitle("Happiness Score and 'GDP per Capita' Comparition with Rank-2016") +
  xlab("Happiness Score")+
  ylab("Happiness Rank") +
  ## Country name
  geom_label_repel(
    aes(GDP_per_Capita*7, factor(desc(Happiness.Rank)), label = Country),
    box.padding = 0.30, 
    point.padding = 0.3,
    segment.color = 'grey50') 

gdp_2017 = whr_geo %>%
  filter(Happiness.Rank <= 30) %>%
  arrange(Happiness.Rank) %>%
  filter(Year == 2017) %>% 
  ggplot() +
  ## happiness
  geom_point(aes(x=Happiness.Score, 
                 y=factor(desc(Happiness.Rank)), 
                 group=1),
             size=2, 
             color="red") +
  ## GDP per Capita
  geom_point(aes(x=GDP_per_Capita*7, 
                 y=factor(desc(Happiness.Rank)), group=1),
             size=2, 
             color="blue") +
  scale_x_continuous(breaks = seq(0,17,by = 0.5) , 
                     labels = seq(0,17,by=0.5),sec.axis = sec_axis(~.*1, 
                                                                   name = "GDP per Capita",
                                                                   labels = round(seq(0,17,by=0.5)/7,digit=2),
                                                                   breaks = seq(0,17,by =0.5))) +
  ggtitle("Happiness Score and 'GDP per Capita' Comparition with Rank-2017") +
  xlab("Happiness Score")+
  ylab("Happiness Rank") +
  ## Country name
  geom_label_repel(
    aes(GDP_per_Capita*7, factor(desc(Happiness.Rank)), label = Country),
    box.padding = 0.30, 
    point.padding = 0.3,
    segment.color = 'grey50') 

## display all 3 years on same 

multiplot(gdp_2015,gdp_2016,gdp_2017,cols=2)
