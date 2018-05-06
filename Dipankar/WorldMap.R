library(maps)
library(ggplot2)
library(ggmap)
library(maptools)
# 
# WorldMap <- qmap(location = "world")
# 
# mapdata <- map_data("world")
# 
# mapdata
# 
# 
# data(wrld_simpl)
# myCountries = wrld_simpl@data$NAME %in% c("India")
# plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
# 
# # sidebarPanel(
# #   helpText("Shows different parameters of related with happiness for the selected country"),
# # 
# #   selectInput(
# #     inputId = "list",
# #     label = "Choose a country from dropdown menu",
# #     choices = data$Country
# #   ),
# #   textInput(inputId = "name",
# #             label = "Enter the name of Country",
# #             placeholder = "First letter to be capital"
# #   )

library(rworldmap)
library(mapdata)
library(ggplot2)
library(dplyr)
library(stringr)

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
WorldMap <- read.csv("Worldmap.csv",stringsAsFactors = F)



WHR <- read.csv("WHR.csv")

WHR$Country <- as.character(WHR$Country)


WorldMap_res <- WorldMap %>%
  filter(region %in% unique(WHR$Country))



unique(WorldMap_res$region)

WorldMap_res$Country <- WorldMap_res$region


WHR_map <- merge(WHR, WorldMap_res, by="Country")

write.csv(WHR_map,"WHR_map_2015.csv")
write.csv(WorldMap_res, "WorldMap_res.csv")


WHR_map %>%
  filter(Year == 2015) %>%
  group_by(Country) %>%
  arrange(order) %>%
  ungroup() %>%
  ggplot() +
  geom_polygon(data = WorldMap,aes(x = long, y = lat, group = group), fill = "pink", alpha = 0.5 ) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Happiness.Score),color = "black") +
  scale_fill_gradient2(low = "white", high = "darkred")

geo <- read.csv("Geo.csv",stringsAsFactors = F)

ggplot(WorldMap_res, aes(x= long, y = lat)) +
  geom_polygon(color = "black", fill = "white", aes(group = group)) 


  



sidebarLayout(
  sidebarPanel( 
    radioButtons(inputId = "bottom",
                 label = "Select the comparison typr", 
                 choices = c("Rank Comparison", "Parameter Comparision", "Location"),
                 selected = NULL)),
  mainPanel( plotOutput(outputId = "bot"), 
             plotOutput(outputId = "bot1"), 
             leafletOutput(outputId = "bot2"))



#################################

# #Add the data you want to map countries by to map.world
# #In this example, I add lengths of country names plus some offset
# map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))
# 
# # ggplot() 
# #   geom_map(data = map.world, map = map.world, aes(x=long, y=lat, fill= region, map_id=region)) +
# #   theme(legend.position="none") +
# #   scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar") +
# #   coord_equal()
# gg <- ggplot()
# gg <- gg + theme(legend.position="none")
# gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=region))
# gg <- gg + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
# gg <- gg + coord_equal()
# gg
  

