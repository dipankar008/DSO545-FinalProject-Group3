library(dplyr)
library(ggplot2)
library(shiny)
library(maptools)
library(gdata)
library(readxl)
library(tidyr)

WHR2015 <- read.csv("World Happiness Report 2015.csv")
WHR2016 <- read.csv("World Happiness Report 2016.csv")
WHR2017 <- read.csv("World Happiness Report 2017.csv")

WHR2015$Year <- 2015
WHR2016$Year <- 2016
WHR2017$Year <- 2017

WHR2016$Standard.Error <- WHR2016$Upper.Confidence.Interval - WHR2016$Happiness.Score

WHR2016 <- WHR2016 %>%
  select(-Lower.Confidence.Interval,-Upper.Confidence.Interval)

WHR2016 <- WHR2016[,c(1:4,13,5:12)]

Regions <- WHR2015 %>%
  select(Country,Region)

WHR2017$Standard.Error <- WHR2017$Whisker.high - WHR2017$Happiness.Score
WHR2017 <- WHR2017 %>%
  select(-Whisker.high,-Whisker.low) 
WHR2017$Region <- NA

vlookup <- function(df1,df2,df1_compare_colindex,df2__compare_colIndex, df1_target_colindex, df2_source_colindex){
  x=1
  y=1
  for (x in 1:nrow(df1)) {
    for(y in 1:nrow(df1)){
      if(as.character(df2[x,df1_compare_colindex])==as.character(df1[y,df2__compare_colIndex])){
        df1[y,df1_target_colindex] = as.character(df2[x,df2_source_colindex])
      }
      y <- y+1
    }
    x<- x+1
  }
}

vlookup(WHR2017,Regions,1,1,13,2)



for (x in 1:nrow(WHR2017)) {
  for(y in 1:nrow(WHR2017)){
    if(as.character(Regions[x,1])==as.character(WHR2017[y,1])){
      WHR2017[y,13] = as.character(Regions[x,2])
    }
    y <- y+1
  }
  x<- x+1
}
WHR2017$Region <- factor(WHR2017$Region)
WHR2017 <- WHR2017[,c(1,13,2,3,12,4:7,9,8,10,11)]



WHR <- rbind(WHR2015,WHR2016,WHR2017)
WHR$Year <- factor(WHR$Year)
WHR <- WHR[,c(1,2,13,3:12)]
colnames(WHR2016) == colnames(WHR2017)

write.csv(WHR,"WHR.csv")

WHR %>%
  filter(Region == "Australia and New Zealand") %>%
ggplot(aes(x= Country, y = Happiness.Rank, group = Year, fill = Year)) +
  geom_col(position = position_dodge()) 

##################################################################################################

# countrydata <- read.xls(xls = "C:/Users/dipan/OneDrive/Engineering Management/DSO 545/Project/DSO545-FinalProject/World Bank’s Ease of Doing Business Index-Dipankar.xlsx", sheet = 1, header = T)
# 
# read_excel("C:/Users/dipan/OneDrive/Engineering Management/DSO 545/Project/DSO545-FinalProject/World Bank’s Ease of Doing Business Index-Dipankar.xlsx")
# 

easeofbiz <- read.csv("EaseOfDoingBusiness.csv")
easeofbiz$Region <- NA

for (x in 1:nrow(easeofbiz)) {
  for(y in 1:nrow(easeofbiz)){
    if(as.character(Regions[x,1])==as.character(easeofbiz[y,1])){
      easeofbiz[y,13] = as.character(Regions[x,2])
    }
    y <- y+1
  }
  x<- x+1
}

easeofbiz$Region <- factor(easeofbiz$Region)
easeofbiz <- easeofbiz[,c(1,13,2:12)]


easeofbiz <- easeofbiz %>%
  gather(Ease.of.Doing.Business.Rank:Resolving.Insolvency, key = "Parameter", value = "Rank")

write.csv(easeofbiz,"easeofbiz_update.csv")





