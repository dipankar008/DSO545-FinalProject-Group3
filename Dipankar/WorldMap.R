library(maps)
library(ggplot2)
library(ggmap)
library(maptools)

WorldMap <- qmap(location = "world")

mapdata <- map_data("world")

mapdata


data(wrld_simpl)
myCountries = wrld_simpl@data$NAME %in% c("India")
plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])

# sidebarPanel(
#   helpText("Shows different parameters of related with happiness for the selected country"),
# 
#   selectInput(
#     inputId = "list",
#     label = "Choose a country from dropdown menu",
#     choices = data$Country
#   ),
#   textInput(inputId = "name",
#             label = "Enter the name of Country",
#             placeholder = "First letter to be capital"
#   )

library(rworldmap)
library(mapdata)
library(ggplot2)
map.world <- map_data(map="world")

#Add the data you want to map countries by to map.world
#In this example, I add lengths of country names plus some offset
map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))

# ggplot() 
#   geom_map(data = map.world, map = map.world, aes(x=long, y=lat, fill= region, map_id=region)) +
#   theme(legend.position="none") +
#   scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar") +
#   coord_equal()
gg <- ggplot()
gg <- gg + theme(legend.position="none")
gg <- gg + geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=region))

gg <- gg + scale_fill_gradient(low = "green", high = "brown3", guide = "colourbar")
gg <- gg + coord_equal()
gg
  

