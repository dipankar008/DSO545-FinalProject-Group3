library(leaflet)
library(maps)
library(ggmap)
library(viridis)
library(dplyr)
library(stringr)



####Read Data 
whr = read.csv("WHR.csv")

china = geocode("China")
lon = china[1][1]
lat = china[2][1]

#####Modify attributes
whr_geo = whr
whr_geo$Country = as.character(whr_geo$Country)
whr_geo$Region = as.character(whr_geo$Region)

colnames(whr_geo)[colnames(whr_geo) == "Trust..Government.Corruption."] = 
  "Government_Corruption"

whr_geo$Government_Corruption = 
  round(whr_geo$Government_Corruption, digits=3)

whr_geo$Freedom = round(whr_geo$Freedom, digits=3)

######Add geo codes column
whr_geo = whr_geo %>%
          mutate_geocode(Country)


##Build colr factor
factpal = colorFactor(viridis(5), whr_geo$Government_Corruption)


#qpal    =  colorNumeric(viridis(10, option = "plasma"), data2$age)
#factpal2 = colorFactor(c("darkorange", "steelblue"), data2$gender)


#####################East Asia Region - 2015######################

whr_east_asia_2015 = whr_geo %>%
                     filter(Region == "Eastern Asia") %>%
                     filter(Year == 2015) %>%
                     arrange(Government_Corruption)

whr_east_asia_2015 = whr_east_asia_2015 %>%
  mutate(Govt_influence = ifelse(Government_Corruption <=0.03, 
                                 "Low", 
                          ifelse(Government_Corruption > .03 & 
                                 Government_Corruption < .2, 
                                 "Average", 
                                 "High")))

factpal_ea = colorFactor(viridis(5), whr_east_asia_2015$Govt_influence)

#####################Asia Region - 2015######################

whr_asia_2015 = whr_geo %>%
                filter(str_detect(Region, 'Asia')) %>%
                filter(Year == 2015) %>%
                arrange(Government_Corruption)


whr_asia_2015 = whr_asia_2015 %>%
                mutate(Govt_influence = ifelse(Government_Corruption <=0.05, 
                                              "Low", 
                                        ifelse(Government_Corruption > .05 & 
                                               Government_Corruption < .2, 
                                               "Average", 
                                               "High")))

whr_asia_2015 = whr_asia_2015 %>%
  mutate(Freedom_influence = ifelse(Freedom <=0.3, 
                                 "Low", 
                                 ifelse(Freedom > .3 & 
                                        Freedom <= .5, 
                                        "Average", 
                                        "High")))

factpal_a = colorFactor(viridis(5), 
                        whr_asia_2015$Govt_influence)

factpal_f =  colorFactor(c("darkorange","red", "steelblue"), 
                         whr_asia_2015$Freedom_influence)

factpal_g = colorNumeric(viridis(10, option = "plasma"), 
                         whr_asia_2015$Generosity)

#pal = colorNumeric(c("red", "green", "blue"), 1:10)


?colorFactor

#whr_east_asia_2015 %>%
whr_asia_2015 %>%
       leaflet() %>% 
       setView(lon, lat , zoom = 4) %>%
       addProviderTiles("Stamen.TonerLite") %>%
       
       ####Govt Influence
       addCircleMarkers(stroke = FALSE, 
                        group = "Government Influence", 
                        fillOpacity = 4, radius=8,
                        label =  ~paste(Country,":",
                                       "Govt Influence:",Govt_influence),
                        labelOptions = labelOptions(noHide = F, 
                                                    textsize = "8px",
                                                    style = list(
                                                      "color" = "red",
                                                      "font-family" = "serif",
                                                      "font-style" = "italic",
                                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "12px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                    )),
                        color = ~factpal_a(Govt_influence)) %>%
  
       ###Freedom Influence
       addCircleMarkers(stroke = FALSE, 
                        group = "Freedom Influence", 
                        fillOpacity = 4, radius=8,
                        label =  ~paste(Country,":",
                                   "Freedom Influence:",Freedom_influence),
                        labelOptions = labelOptions(noHide = F, 
                                               textsize = "8px",
                                               style = list(
                                                 "color" = "black",
                                                 "font-family" = "serif",
                                                 "font-style" = "bold",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                        color = ~factpal_f(Freedom_influence))  %>%
  
       ###Generosity Influence
       addCircleMarkers(stroke = FALSE, 
                   group = "Generosity Influence", 
                   fillOpacity = 4, radius=8,
                   label =  ~paste(Country,":",
                                   "Generosity Influence:",Generosity),
                   labelOptions = labelOptions(noHide = F, 
                                               textsize = "8px",
                                               style = list(
                                                 "color" = "black",
                                                 "font-family" = "serif",
                                                 "font-style" = "bold",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                   color = ~factpal_g(Generosity))  %>%
  
      addLegend("topleft", pal = factpal_a, 
                values = whr_asia_2015$Govt_influence, 
                title = "Government Influence", 
                opacity = .8) %>%
  
      addLegend("topleft", pal = factpal_f, 
                values = whr_asia_2015$Freedom_influence, 
                title = "Freedom Influence", 
                opacity = .8)  %>%
  
      addLegend("topleft", pal = factpal_g, 
                values = whr_asia_2015$Generosity, 
                title = "Generosity Influence", 
                opacity = .8) %>%
      
      addLayersControl(
              baseGroups = c("Government Influence",
                             "Freedom Influence",
                             "Generosity Influence"),
                             #"Life.Expectancy",
                             #"Family",
                             #"GDP per Capita"),
              options = layersControlOptions(collapsed = FALSE))

?legend
save(whr_geo,file="WHR_Geo.Rda")
?addCircleMarkers
