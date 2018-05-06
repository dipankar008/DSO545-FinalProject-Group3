library(sp)
library(maps)
library(maptools)
library(leaflet)

# make sure to use the latest maps package
# it was recently updated at the time of the answer

world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), FALSE)

cnt <- c("China")

target <- subset(world_map, country %in% cnt)

leaflet(target) %>% 
  addTiles() %>% 
  addPolygons(weight=1) %>%
  addEasyButton(easyButton(
    icon="fa-globe", title="Zoom to Level 1",
    onClick=JS("function(btn, map){ map.setZoom(3); }"))) 
