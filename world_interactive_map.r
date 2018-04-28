library(leaflet)
library(maps)
library(ggmap)
library(viridis)
library(dplyr)


whr = read.csv("WHR.csv")

china = geocode("China")
lon = china[1][1]
lat = china[2][1]

whr_geo = whr
whr_geo$Country = as.character(whr_geo$Country)
whr_geo$Region = as.character(whr_geo$Region)
whr_geo = whr_geo %>%
          mutate_geocode(Country)

colnames(whr_geo)[colnames(whr_geo) == "Trust..Government.Corruption."] = "Government_Corruption"

factpal = colorFactor(viridis(5), whr_geo$Government_Corruption)
#qpal    =  colorNumeric(viridis(10, option = "plasma"), data2$age)
#factpal2 = colorFactor(c("darkorange", "steelblue"), data2$gender)



#############Only East asia 2015######################


whr_east_asia_2015 = whr_geo %>%
                     filter(Region == "Eastern Asia") %>%
                     filter(Year == 2015)

whr_east_asia_2015$Government_Corruption = round(whr_east_asia_2015$Government_Corruption,
                                                  digits=3)

whr_east_asia_2015 = whr_east_asia_2015 %>%
            mutate(Govt_influence = ifelse(Government_Corruption <=0.03, 
                   "Low", 
                   ifelse(Government_Corruption > .03 & Government_Corruption < .2, 
                          "Average", 
                          "High")))
factpal_ea = colorFactor(viridis(5), whr_east_asia_2015$Govt_influence)


whr_east_asia_2015 %>%
       leaflet() %>% 
       setView(lon, lat , zoom = 4) %>%
       #addTiles() #%>%
       addProviderTiles("Stamen.TonerLite") %>%
       addCircleMarkers(stroke = FALSE, group = "Government/Corruption Influence", 
                        fillOpacity = 0.5, 
                        radius=2.5,
                 popup = ~whr_east_asia_2015$Govt_influence,
                 color = ~factpal_ea(whr_east_asia_2015$Govt_influence)) %>%
       addLegend("topleft", pal = factpal, values = whr_east_asia_2015$Govt_influence, 
                  title = "Government", 
                  opacity = .8)


save(whr_geo,file="WHR_Geo.Rda")
