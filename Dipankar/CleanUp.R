library(dplyr)
library(ggplot2)
library(shiny)
library(maptools)
library(gdata)
library(readxl)
library(tidyr)
library(rvest)

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


for (x in 1:nrow(Regions)) {
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

WHR <- WHR %>%
  filter(!(Country %in% c("Oman","Suriname","Laos", "South Sudan", "Somalia",
                          "Somaliland Region", "Mozambique", "Lesotho", "Somaliland region",
                          "Swaziland", "Djibouti", "Comoros", "Central African Republic", 
                          "Belize", "Namibia", "Puerto Rico")))


WHR$Country <- as.character(WHR$Country)
write.csv(WHR,"WHR.csv")

WHR %>%
  filter(Region == "Australia and New Zealand") %>%
ggplot(aes(x= Country, y = Happiness.Rank, group = Year, fill = Year)) +
  geom_col(position = position_dodge()) 

happy_score <- WHR %>%
  select(1,2,3,5) %>%
  mutate(Year = gsub("20","X20",Year)) %>%
  spread(key = Year, value = Happiness.Score, convert = F )
  

happy_score <- happy_score[complete.cases(happy_score),]

happy_score<- happy_score %>% 
  mutate(diff = happy_score$X2017  - happy_score$X2015) %>%
  arrange(-diff) 

happy_score_top <- happy_score %>% 
  top_n(6)
happy_score_bot <- happy_score %>% 
    top_n(-6)

happy_TopBot<-rbind(happy_score_bot,happy_score_top)

write.csv(happy_score,"happy_score_TopBot.csv")

WHR_2 <- WHR %>%
  mutate(Comment = NA)

happy_score_bot$Region <- as.character(happy_score_bot$Region)
happy_score_top$Region <- as.character(happy_score_top$Region)


for (a in 1:nrow(happy_score_bot)) {
  for(b in 1:nrow(WHR_2)){
    if( happy_score_bot[a,1] == WHR_2[b,1]){
      WHR_2[b,14] = "Bottom 6"
    }
    b <- b+1
  }
  b<- b+1
}

for (x in 1:nrow(happy_score_top)) {
  for(y in 1:nrow(WHR_2)){
    if(happy_score_top[x,1] == WHR_2[y,1]){
      WHR_2[y,14] = "Top 6"
    }
    y <- y+1
  }
  x<- x+1
}

WHR_2 <- WHR_2 %>%
  filter(Comment %in% c("Bottom 6", "Top 6"))

write.csv(WHR_2,"WHR_TopBot.csv")

WHR_2 %>%
  gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
  filter(Comment == "Top 6") %>%
  ggplot(aes(x=Type, y= Score, group = Year, fill = Year)) +
  geom_col(position = position_dodge()) +
  coord_flip() + facet_wrap(~Country) +
  geom_text(aes(label = Happiness.Rank), position = position_dodge(width = 1))
  
  


##################################################################################################

# countrydata <- read.xls(xls = "C:/Users/dipan/OneDrive/Engineering Management/DSO 545/Project/DSO545-FinalProject/World Bank’s Ease of Doing Business Index-Dipankar.xlsx", sheet = 1, header = T)
# 
# read_excel("C:/Users/dipan/OneDrive/Engineering Management/DSO 545/Project/DSO545-FinalProject/World Bank’s Ease of Doing Business Index-Dipankar.xlsx")
# 

easeofbiz <- read.csv("EaseOfDoingBusiness.csv")
easeofbiz$Region <- 1

for (a in 1:nrow(Regions)) {
  for(b in 1:nrow(easeofbiz)){
    if(as.character(Regions[a,1]) == as.character(easeofbiz[b,1])){
      easeofbiz[b,13] = as.character(Regions[b,2])
    }
    b <- b+1
  }
  a<- a+1
}

easeofbiz$Region <- factor(easeofbiz$Region)
easeofbiz <- easeofbiz[,c(1,13,2:12)]


easeofbiz <- easeofbiz %>%
  gather(Ease.of.Doing.Business.Rank:Resolving.Insolvency, key = "Parameter", value = "Rank")

write.csv(easeofbiz,"easeofbiz_update.csv")


#################################################################################################

URL <- "https://developers.google.com/public-data/docs/canonical/countries_csv"


Geocodes <- URL %>%
  read_html()
  
Geocodes <- html_nodes(Geocodes, "table") %>%
  html_table(fill = T)

Geocodes <- Geocodes[[1]]

write.csv(Geocodes,"Geo.csv")



