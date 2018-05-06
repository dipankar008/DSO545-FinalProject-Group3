#########################################################################################

#File:world_interactive_map_func.r

#Name: Pavan Gururaj Muddebihal

#Brief : World happiness Analysis - Analyze influence of different attributes as listed 
#        below on happiness score based on different world regions
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



####Read Data 
whr = read.csv("WHR.csv")
#whr_geo = readRDS(WHR_Geo.Rda)
#####Modify attributes
whr_geo = whr
whr_geo$Country = as.character(whr_geo$Country)
whr_geo$Region = as.character(whr_geo$Region)

##Rename attributes
colnames(whr_geo)[colnames(whr_geo) == "Trust..Government.Corruption."] = 
                                        "Government_Corruption"
colnames(whr_geo)[colnames(whr_geo) == "Health..Life.Expectancy."] = 
                                        "Life_Expectancy"

##Round up attributes to 3 digits
whr_geo$Life_Expectancy = round(whr_geo$Life_Expectancy, digits = 3)
whr_geo$Government_Corruption = round(whr_geo$Government_Corruption, digits=3)
whr_geo$Freedom = round(whr_geo$Freedom, digits=3)

######Add geo codes column
# whr_geo = whr_geo %>%
#   mutate_geocode(Country)


plot_int_world_map = function(region, year)
{

  cat("IN-->plot_int_world_map",region,year)
  whr_reg_yr = whr_geo %>%
               filter(Region == region) %>%
               filter(Year == year) %>%
               arrange(Government_Corruption)
  cat("nrows",nrow(whr_reg_yr))
  
  whr_reg_yr = whr_reg_yr %>%
               mutate_geocode(Country)
  cat("ncol",ncol(whr_reg_yr))
  
  whr_reg_yr = whr_reg_yr %>%
               mutate(Govt_influence = ifelse(Government_Corruption <=0.05, 
                                             "Low", 
                                       ifelse(Government_Corruption > .05 & 
                                              Government_Corruption < .2, 
                                             "Average", 
                                            "High")))
  
  whr_reg_yr = whr_reg_yr %>%
                mutate(Freedom_influence = ifelse(Freedom <=0.3, 
                                            "Low", 
                                           ifelse(Freedom > .3 & 
                                                  Freedom <= .5, 
                                             "Average", 
                                             "High")))
  
  
  factpal_a = colorFactor(viridis(5), 
                          whr_reg_yr$Govt_influence)
  factpal_f =  colorFactor(c("darkorange","red", "steelblue"), 
                           whr_reg_yr$Freedom_influence)
  factpal_g = colorNumeric(viridis(10, option = "plasma"), 
                           whr_reg_yr$Generosity)
  factpal_l = colorNumeric(viridis(10, option = "plasma"), 
                           whr_reg_yr$Life_Expectancy)

  cat("\ncol before leaflet",ncol(whr_reg_yr))
  str(whr_reg_yr)
  whr_reg_yr %>%
       leaflet() %>% 
       setView(whr_reg_yr$lon, whr_reg_yr$lat , zoom = 2) %>%
       addProviderTiles("Stamen.TonerLite") %>%     ####Govt Influence
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
                        color = ~factpal_a(Govt_influence)) %>%      ###Freedom Influence
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
  
       ###Generosity Influence
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
                                                 "font-style" = "bold",
                                                 "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                 "font-size" = "12px",
                                                 "border-color" = "rgba(0,0,0,0.5)"
                                               )),
                   color = ~factpal_l(Life_Expectancy))  %>%
  
      ###Legends for above attributes
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
      
      addLegend("topleft", pal = factpal_l, 
                values = whr_reg_yr$Life_Expectancy, 
                title = "Life Expectancy Influence", 
                opacity = .8) %>%
  
      addLayersControl(
              baseGroups = c("Government Influence",
                             "Freedom Influence",
                             "Generosity Influence",
                             "Life Expectancy Influence"),
                             #"Family",
                             #"GDP per Capita"),
              options = layersControlOptions(collapsed = FALSE))

}

plot_int_world_map("Australia and New Zealand",2016)

save(whr_geo,file="WHR_Geo.Rda")

