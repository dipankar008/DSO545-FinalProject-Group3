library(dplyr)
library(ggplot2)
library(shiny)
library(maptools)
library(gdata)
library(readxl)
library(tidyr)
library(rvest)
library(stringr)

### 1
### Creating a master file, which will include Happiness parameters of different countries with respect to their year
### Name of the file will be WHR, short for World Happiness Report

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


WHR$Country <- str_replace(string = WHR$Country, 
                           pattern = "Congo (Brazzaville)", 
                           replacement = "Congo (Republic)")

WHR$Country <- str_replace(string = as.character(WHR$Country), 
                               pattern = "Congo (Kinshasa)",
                               replacement = "Congo (Democratic Republic)")


write.csv(WHR,"WHR.csv")


### The WHR has been saved in the project directory to be called again in Shiny App and other related fuctions

### 2
### We want to show Top 6 and Bottom 6 countries who has chnaged the most in terms of there happiness score
### For above, we will make the subset of 12 countries (Top 6 and Bottom 6) with respect to their year


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


### CSV file WHR_TopBot.csv has been saved to be further called in Shiny App
  


##################################################################################################

# countrydata <- read.xls(xls = "C:/Users/dipan/OneDrive/Engineering Management/DSO 545/Project/DSO545-FinalProject/World Bank’s Ease of Doing Business Index-Dipankar.xlsx", sheet = 1, header = T)
# 
# read_excel("C:/Users/dipan/OneDrive/Engineering Management/DSO 545/Project/DSO545-FinalProject/World Bank’s Ease of Doing Business Index-Dipankar.xlsx")
# 

easeofbiz <- read.csv("EaseOfDoingBusiness.csv")
easeofbiz$Region <- 1

easeofbiz$Economy <- str_replace(string = easeofbiz$Economy,
                                 pattern = "Congo, Rep.",
                                 replacement = "Congo (Brazzaville)")

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

write.csv(easeofbiz,"easeofbiz.csv")


#################################################################################################

URL <- "https://developers.google.com/public-data/docs/canonical/countries_csv"


Geocodes <- URL %>%
  read_html()
  
Geocodes <- html_nodes(Geocodes, "table") %>%
  html_table(fill = T)

Geocodes <- Geocodes[[1]]

write.csv(Geocodes,"Geo.csv")

Geocodes <- read.csv("Geo.csv")

Geocodes <- Geocodes %>%
  filter(name %in% WHR$Country)

colnames(Geocodes)[which(names(Geocodes) == "name")] <- "Country"

WHR_Geo <- merge(WHR, Geocodes, by = "Country")

write.csv(WHR_Geo,"WHR_Geo.csv")



###################################################################################################

map.world <- map_data(map="world")


map.world$region <- str_replace(string = map.world$region, 
                                pattern = "^UK", 
                                replacement = "United Kingdom")
map.world$region <- str_replace(string = map.world$region, 
                                pattern = "Democratic Republic of the Congo",
                                replacement = "Congo (Kinshasa)")
map.world$region <- str_replace(string = map.world$region, 
                                pattern = "Republic of Congo" , 
                                replacement = "Congo (Brazzaville)")
map.world$region <- str_replace(string = map.world$region, 
                                pattern = "^USA", 
                                replacement = "United States")
map.world$region <- str_replace(string = map.world$region, 
                                pattern = c("Trinidad","Tobago") , 
                                replacement = "Trinidad and Tobago")
map.world$region <- str_replace(string = map.world$region, 
                                pattern = "North Cyprus", 
                                replacement = "Cyprus")
map.world$region <- str_replace(string = map.world$region, 
                                pattern = "Palestine" , 
                                replacement = "Palestinian Territories")


for(x in 1:nrow(map.world)){
  if(is.na(map.world[x,6])){
    next
  }else if(map.world[x,6] == "Hong Kong"){
    map.world[x,5] = "Hong Kong"
  } 
  x = x+1
}

for(x in 1:nrow(map.world)){
  if(is.na(map.world[x,6])){
    next
  }else if(map.world[x,6] == "Northern Cyprus"){
    map.world[x,5] = "North Cyprus"
  } 
  x = x+1
}


write.csv(map.world,"WorldMap.csv")
Worldmap <- read.csv("Worldmap.csv",stringsAsFactors = F)

WHR$Country <- as.character(WHR$Country)

WorldMap_res <- Worldmap %>%
  filter(region %in% unique(WHR$Country))

colnames(WorldMap_res)[which(names(WorldMap_res) == "region")] <- "Country"


WHR_map <- merge(WHR, WorldMap_res, by="Country")

write.csv(WHR_map,"WHR_map.csv")
