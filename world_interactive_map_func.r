#########################################################################################

#File:world_interactive_map_func.r

#Name: Pavan Gururaj Muddebihal

#Brief : World happiness Analysis - Analyze influence of different attributes as listed 
#        below on happiness score for different world regions
#1. Freedom Influence
#2. Generosity Influence
#3. Life expectancy Influence
#4. Family Influence
#5. Government/Corruption Influence
#6. GDP/per capita

#########################################################################################


library(leaflet)
library(maps)
library(ggmap)
library(viridis)
library(dplyr)
library(stringr)


####Read Data that holds geocodes for all countries
whr = read.csv("WHR_geo.csv")

#####Modify attributes
whr_geo = whr
whr_geo$Country = as.character(whr_geo$Country)
whr_geo$Region = as.character(whr_geo$Region)

##Rename attributes
colnames(whr_geo)[colnames(whr_geo) == "Trust..Government.Corruption."] = 
                                        "Government_Corruption"
colnames(whr_geo)[colnames(whr_geo) == "Health..Life.Expectancy."] = 
                                        "Life_Expectancy"
colnames(whr_geo)[colnames(whr_geo) == "Economy..GDP.per.Capita."] = 
                                        "GDP_per_capita"

##Round up attributes to 3 digits
whr_geo$Life_Expectancy = round(whr_geo$Life_Expectancy, digits = 3)
whr_geo$Government_Corruption = round(whr_geo$Government_Corruption, digits=3)
whr_geo$Generosity = round(whr_geo$Generosity, digits=3)
whr_geo$Freedom = round(whr_geo$Freedom, digits=3)
whr_geo$Family = round(whr_geo$Family, digits=3)
whr_geo$GDP_per_capita = round(whr_geo$GDP_per_capita, digits=3)


###Function of plot the interactive world map for region based attribute analysis
plot_int_world_map = function(region, year)
{

  #Filter data set based on region and Year selected by user
  whr_reg_yr = whr_geo %>%
               filter(Region == region) %>%
               filter(Year == year) %>%
               arrange(Government_Corruption)

  #Assigning influence levels for Government corruption
  whr_reg_yr = whr_reg_yr %>%
               mutate(Govt_influence = ifelse(Government_Corruption <=0.05, 
                                             "Low", 
                                       ifelse(Government_Corruption > .05 & 
                                              Government_Corruption < .2, 
                                             "Moderate", 
                                            "High")))
  
  whr_reg_yr$Govt_influence = factor(whr_reg_yr$Govt_influence,
                                      levels = c("Low", "Moderate", "High"))
  
  #Assigning influence levels for Freedom
  whr_reg_yr = whr_reg_yr %>%
                mutate(Freedom_influence = ifelse(Freedom <=0.3, 
                                            "Low", 
                                           ifelse(Freedom > .3 & 
                                                  Freedom <= .5, 
                                             "Moderate", 
                                             "High")))
  
  whr_reg_yr$Freedom_influence = factor(whr_reg_yr$Freedom_influence,
                                        levels = c("Low", "Moderate", "High"))
  
  #Define color palettes for different happiness attributes
  factpal_a = colorFactor(viridis(5), 
                          whr_reg_yr$Govt_influence)
  factpal_f =  colorFactor(c("darkorange","steelblue","red"), 
                           whr_reg_yr$Freedom_influence)
  
  factpal_g = colorNumeric(viridis(10, option = "magma"), 
                           whr_reg_yr$Generosity)
  factpal_l = colorNumeric(viridis(10, option = "plasma"), 
                           whr_reg_yr$Life_Expectancy)
  factpal_fa = colorNumeric(viridis(10, option = "inferno"), 
                           whr_reg_yr$Family)
  factpal_gd = colorNumeric(viridis(10, option = "plasma"), 
                            whr_reg_yr$GDP_per_capita)
  

  curr_lat = whr_reg_yr %>%
             select(latitude) %>%
             summarise(mean(latitude))
  curr_lat = unlist(unclass(curr_lat))
  print(curr_lat)
  
  curr_long = whr_reg_yr %>%
              select(longitude) %>%
              summarise(mean(longitude))
  curr_long = unlist(unclass(curr_long))
  print(curr_long)
  
  #Plot leaflet adding markers and legends for different attributes
  whr_reg_yr %>%
       leaflet() %>%
       #setView(longitude, latitude, zoom = 4) %>%
       addProviderTiles("Stamen.TonerLite") %>%
       
       ####Govt Influence Markers
       addCircleMarkers(stroke = FALSE, 
                        group = "Government Influence", 
                        fillOpacity = 2, radius=8,
                        label =  ~paste(Country,":",
                                       "Govt Influence:",Govt_influence),
                        labelOptions = labelOptions(noHide = F, 
                                                    textsize = "8px",
                                                    style = list(
                                                      "color" = "black",
                                                      "font-family" = "serif",
                                                      "font-style" = "italic",
                                                      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                      "font-size" = "16px",
                                                      "border-color" = "rgba(0,0,0,0.5)"
                                                    )),
                        color = ~factpal_a(Govt_influence)) %>%
  
       ###Freedom Influence Markers
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
                                                 "font-style" = "italic",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "16px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                        color = ~factpal_f(Freedom_influence))  %>%
  
       ###Generosity Influence Markers
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
                                                 "font-style" = "italic",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "16px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                        color = ~factpal_g(Generosity))  %>%
  
       ###Life Expectancy Influence Markers
       addCircleMarkers(stroke = FALSE, 
                        group = "Life Expectancy Influence", 
                        fillOpacity = 4, radius=8,
                        label =  ~paste(Country,":",
                                   "Life Expectancy Influence",Life_Expectancy),
                        labelOptions = labelOptions(noHide = F, 
                                               textsize = "8px",
                                               style = list(
                                                 "color" = "black",
                                                 "font-family" = "serif",
                                                 "font-style" = "italic",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "16px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                        color = ~factpal_l(Life_Expectancy))  %>%
    
       ###Family Influence Markers
       addCircleMarkers(stroke = FALSE, 
                        group = "Family Influence", 
                        fillOpacity = 4, radius=8,
                        label =  ~paste(Country,":",
                                     "Family Influence",Family),
                        labelOptions = labelOptions(noHide = F, 
                                                 textsize = "8px",
                                                 style = list(
                                                   "color" = "black",
                                                   "font-family" = "serif",
                                                   "font-style" = "italic",
                                                   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                   "font-size" = "16px",
                                                   "border-color" = "rgba(0,0,0,0.5)"
                                                 )),
                        color = ~factpal_fa(Family))  %>%
  
    
    ###Family Influence Markers
    addCircleMarkers(stroke = FALSE, 
                     group = "GDP per capita Influence", 
                     fillOpacity = 4, radius=8,
                     label =  ~paste(Country,":",
                                     "GDP per capita Influence ",GDP_per_capita),
                     labelOptions = labelOptions(noHide = F, 
                                                 textsize = "8px",
                                                 style = list(
                                                   "color" = "black",
                                                   "font-family" = "serif",
                                                   "font-style" = "italic",
                                                   "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                   "font-size" = "16px",
                                                   "border-color" = "rgba(0,0,0,0.5)"
                                                 )),
                     color = ~factpal_gd(GDP_per_capita))  %>%
      
      ###Add legends for all attributes
      addLegend("topleft", pal = factpal_a, 
                values = whr_reg_yr$Govt_influence, 
                title = "Government Influence", 
                opacity = .8) %>%
  
      addLegend("topleft", pal = factpal_f, 
                values = whr_reg_yr$Freedom_influence, 
                title = "Freedom Influence", 
                opacity = .8)  %>%
  
      addLegend("topleft", pal = factpal_g,
                values = whr_reg_yr$Generosity, 
                title = "Generosity Influence", 
                opacity = .8) %>%
      
      addLegend("bottomright", pal = factpal_l, 
                values = whr_reg_yr$Life_Expectancy, 
                title = "Life Expectancy Influence", 
                opacity = .8) %>%
  
      addLegend("bottomright", pal = factpal_fa, 
                values = whr_reg_yr$Family, 
                title = "Family Influence", 
                opacity = .8 ) %>%
    
      addLegend("topleft", pal = factpal_gd, 
                values = whr_reg_yr$GDP_per_capita, 
                title = "GDP per capita Influence", 
                opacity = .8) %>%
      
      #List the atribute groups
      addLayersControl(baseGroups = c("Government Influence",
                                      "Freedom Influence",
                                      "Generosity Influence",
                                      "Life Expectancy Influence",
                                      "Family Influence",
                                      "GDP per capita Influence"),
                      options = layersControlOptions(collapsed = FALSE))
}

