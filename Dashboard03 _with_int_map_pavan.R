library(shiny)
library(maptools)
library(plotly)
library(dplyr)
library(sp)
library(maps)
library(leaflet)
library(tidyr)


data = read.csv("WHR.csv")
data2 = read.csv("easeofbiz_update.csv")
data3 <- read.csv("happy_score_TopBot.csv")
data4 <- read.csv("WHR_TopBot.csv")

world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), FALSE)


ui <- shinyUI(
  
  #title = ",
  
  navbarPage(
    title = "Pursuit of Happiness",
    
    tabPanel("Our Approach"),
    tabPanel("Country Specific Information",
             fluidRow(
               column(3,
                      
                      helpText("Shows different parameters related with happiness for the selected country"),
                      selectizeInput(inputId = "try", 
                                     label = "Enter the name of Country" , 
                                     choices = data$Country, selected = NULL, multiple = FALSE )),
               leafletOutput(outputId = "map")
             ),
             
             fluidRow(
               splitLayout(cellWidths = c("70%", "30%"), plotOutput(outputId = "graph"), tableOutput(outputId = "table"))
             )         ),
    tabPanel("Top 6 : Countries with improved ranking",
            
              sidebarLayout(
              sidebarPanel( 
                radioButtons(inputId = "select",
                        label = "Select the comparison typr", 
                        choices = c("Rank Comparison", "Parameter Comparision"),
                        selected = NULL)),
            mainPanel( plotOutput(outputId = "Top"))
             )),
    tabPanel("Region based attribute analysis",
             
             sidebarLayout(
               sidebarPanel( 
                 radioButtons(inputId = "region",
                              label = "Select the Region", 
                              choices = c("Australia and New Zealand", 
                                          "Central and Eastern Europe",
                                          "Eastern Asia",
                                          "Latin America and Caribbean",
                                          "Middle East and Northern Africa",
                                          "North America",
                                          "Southeastern Asia",
                                          "Southern Asia",
                                          "Sub-Saharan Africa",
                                          "Western Europe"),
                              selected = NULL),
                 radioButtons(inputId = "year",
                              label = "Select the Year", 
                              choices = c("2015", 
                                          "2016",
                                          "2017"
                                          ),
                              selected = NULL)),
               mainPanel(leafletOutput(outputId = "int_world_map"))
               #leafletOutput("int_world_map")
             )),
    
    tabPanel("Component 4")
  )
)

server <- function(input, output) {
  output$graph <- renderPlot({

    data %>%
      filter(Country ==  input$try) %>%
      ggplot(aes(x= as.factor(Year), y = Happiness.Score, fill = as.factor(Year))) +
      geom_col(position = position_dodge())

  })
  
  output$map <- renderLeaflet({
    cnt <- c(input$try)
      
    
    target <- subset(world_map, country %in% cnt)
    
    leaflet(target) %>% 
      addTiles() %>% 
      addPolygons(weight=1) %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Level 1",
        onClick=JS("function(btn, map){ map.setZoom(3); }"))) 
  })
  
  output$table <- renderTable({
    data2 %>%
      filter(Economy == input$try) %>%
      select(Parameter,Rank)
    
  }) 
  
  output$Top <- renderPlot({
    if(input$select == "Rank Comparison" ) {
      data4 %>%
        filter(Comment == "Top 6") %>%
        ggplot(aes(x=Country, y = Happiness.Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge())
    } else {
      data4 %>%
      gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
        filter(Comment == "Top 6") %>%
        ggplot(aes(x=Type, y= Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
        coord_flip() + facet_wrap(~Country)
    }
  })
  
  #region based analysis plot on an interactive world map
  output$int_world_map <- renderLeaflet({
    #Call function to plot leaflet
    cat("IN-->plot_int_world_map",region,year)
    whr_reg_yr = whr_geo %>%
      filter(Region == input$region) %>%
      filter(Year == input$year) %>%
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
    
  })
}

shinyApp(ui,server)


