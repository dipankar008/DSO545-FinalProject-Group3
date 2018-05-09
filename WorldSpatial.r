world <- map("world", fill=TRUE, plot=FALSE)
worldSpatial <- map2SpatialPolygons(world, sub(":.*$", "", world$names))


worldSpatial <- SpatialPolygonsDataFrame(worldSpatial,
                                         data.frame(country=names(worldSpatial), 
                                                    stringsAsFactors=FALSE), FALSE)

worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = "^UK", 
                                replacement = "United Kingdom")
worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = "Democratic Republic of the Congo",
                                replacement = "Congo (Kinshasa)")
worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = "Republic of Congo" , 
                                replacement = "Congo (Brazzaville)")
worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = "^USA", 
                                replacement = "United States")
worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = c("Trinidad","Tobago") , 
                                replacement = "Trinidad and Tobago")
worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = "North Cyprus", 
                                replacement = "Cyprus")
worldSpatial$country <- str_replace(string = worldSpatial$country, 
                                pattern = "Palestine" , 
                                replacement = "Palestinian Territories")


