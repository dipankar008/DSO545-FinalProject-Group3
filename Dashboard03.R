library(shiny)
library(maptools)
library(dplyr)
library(sp)
library(maps)
library(leaflet)
library(tidyr)


data = read.csv("WHR.csv")
data2 = read.csv("easeofbiz.csv")
data3 <- read.csv("geo.csv",stringsAsFactors = F)
data4 <- read.csv("WHR_TopBot.csv")
data5 <- read.csv("WHR_map.csv")
WorldMap <- read.csv("Worldmap.csv")
world <- map("world", fill=TRUE, plot=FALSE)
world_map <- map2SpatialPolygons(world, sub(":.*$", "", world$names))
world_map <- SpatialPolygonsDataFrame(world_map,
                                      data.frame(country=names(world_map), 
                                                 stringsAsFactors=FALSE), FALSE)


ui <- shinyUI(
  
    navbarPage(
    title = "Pursuit of Happiness",
    
    tabPanel("Our Approach",
             
              tabsetPanel(type = "tabs",
                         tabPanel("Introduction",
                                  textOutput("intro")
                                  ),
                         tabPanel("Heat Map",
                                  sidebarLayout(
                                  sidebarPanel( radioButtons(inputId = "Ye",
                                               label = "Select Year",
                                               choices = c("2015","2016","2017"),
                                               selected = NULL )),
                                  mainPanel(plotOutput(outputId = "XY")))),

                         tabPanel("Parameter Description")
                                  )
                         ),
 
    tabPanel("Country Specific Information",
             fluidRow(
               column(3,
                      
                      helpText("Shows different parameters related with happiness for the selected country"),
                      selectizeInput(inputId = "try", 
                                     label = "Enter the name of Country" , 
                                     choices = data$Country, selected = NULL, multiple = FALSE )),
              
                          leafletOutput(outputId = "map")
                          
             ),
             br(),
             
             fluidRow(
               column(4, 
                           plotOutput(outputId = "graph")),
               column(3, offset = 1,
                           verticalLayout(helpText("Values of different parameters per \nyear",
                                                   " for the selected country"), 
                                          tableOutput(outputId = "table1"))),
               column(2, offset = 1,
                           tableOutput(outputId = "table")))
                        
             ),
    
    tabPanel("Top 6 : Countries with improved ranking",
            
             navlistPanel(
               tabPanel("Rank Comparision",
                        plotOutput("top")),
               tabPanel("Parameter Comparision",
                        plotOutput("top1")),
               tabPanel("location",
                        leafletOutput("top2"))
             )
            ),
    tabPanel("Bottom 6 : Countries with decreased rankings",
             navlistPanel(
               tabPanel("Rank Comparision",
                        plotOutput("bot")),
               tabPanel("Parameter Comparision",
                        plotOutput("bot1")),
               tabPanel("location",
                        leafletOutput("bot2"))
               )
             ),
    tabPanel("Component 4")
  )
)

server <- function(input, output) {
  
  output$intro <- renderText({
    source("intro.R")
  })
  
  
  output$XY <- renderPlot({

    data5 %>%
      filter(Year == as.character(input$Ye)) %>%
      group_by(Country) %>%
      arrange(order) %>%
      ungroup() %>%
      ggplot() +
      geom_polygon(data = WorldMap,aes(x = long, y = lat, group = group), fill = "pink", alpha = 0.5 ) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = Happiness.Score),color = "black") +
      scale_fill_gradient2(low = "white", high = "darkred")
  })
  
  output$graph <- renderPlot({

    data %>%
      filter(Country ==  input$try) %>%
      ggplot(aes(x= Year, y = Happiness.Score)) +
      geom_line(color = "blue", linetype = "dashed") + 
      geom_point(size = 3) + 
      geom_text(aes(label= Happiness.Rank), vjust = -1) +
      scale_x_continuous(breaks = c(2015,2016,2017))

  })
  
  output$map <- renderLeaflet({
    cnt <- c(input$try)
    
    target <- subset(world_map, country %in% cnt)
    
    leaflet(target) %>% 
      addTiles() %>% 
      addPolygons(weight=1) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Standard",
        onClick=JS("function(btn, map){ map.setZoom(3); }"))) 
  })
  
  output$table1 <- renderTable({
    x <- data %>%
      filter(Country == input$try) %>%
      select(Year,Happiness.Rank ,Happiness.Score,Economy..GDP.per.Capita.:Generosity) %>%
      gather(Happiness.Rank: Generosity,key = "Parameter", value = "Score") %>%
      spread(key = Year, value = Score)
      x[c(5,6,1:4,7:8),]
  })
  
  output$table <- renderTable({
    data2 %>%
      filter(Economy == input$try) %>%
      select(Parameter,Rank)
    
  }) 
  
  output$top <- renderPlot({
    
      data4 %>%
        filter(Comment == "Top 6") %>%
        ggplot(aes(x=Country, y = Happiness.Score, group = as.factor(Year), fill = as.factor(Year))) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = Happiness.Rank), vjust = -1 ,position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("red", "orange", "brown"),
                        guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                             direction = "horizontal")) +
      theme(legend.position = "bottom")
      
    })
  
  output$top1 <- renderPlot({
      
    data4 %>%
      gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
        filter(Comment == "Top 6") %>%
        arrange(Happiness.Rank)%>%
        ggplot(aes(x=Type, y= Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
        geom_text(aes(label = Happiness.Rank), hjust = -1 ,position = position_dodge(width = 1)) + 
        scale_fill_manual(values = c("red", "orange", "brown"),
                          guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                               direction = "horizontal")) +
        coord_flip() + facet_wrap(~Country) + theme(legend.position = "bottom")
    
  
  })
  
  output$top2 <- renderLeaflet({
    
    
    data4$Country <- as.character(data4$Country)
    cnt <- data4 %>%
      filter(Comment == "Top 6") %>%
      select(Country) %>%
      unique() 
    cnt <- unlist(unclass(cnt))
    
    target <- subset(world_map, country %in% cnt)
    
    leaflet(target) %>% 
      addTiles() %>% 
      addPolygons(weight=1) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Standard",
        onClick=JS("function(btn, map){ map.setZoom(3); }")))
  })
  
  output$bot <- renderPlot({
    
      data4 %>%
        filter(Comment == "Bottom 6") %>%
        ggplot(aes(x=Country, y = Happiness.Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
      geom_text(aes(label = Happiness.Rank), hjust = -1 ,position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("red", "orange", "brown"),
                        guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                             direction = "horizontal")) + 
      theme(legend.position = "bottom")
    })
  output$bot1 <- renderPlot({
    
      data4 %>%
        gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
        filter(Comment == "Bottom 6") %>%
        ggplot(aes(x=Type, y= Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
      geom_text(aes(label = Happiness.Rank), hjust = -1 ,position = position_dodge(width = 1)) +
        scale_fill_manual(values = c("red", "orange", "brown"),
                          guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                               direction = "horizontal")) +
        coord_flip() + facet_wrap(~Country) + theme(legend.position = "bottom")
    
     
  })
  
  output$bot2 <- renderLeaflet({
    
      
      data4$Country <- as.character(data4$Country)
      cnt <- data4 %>%
        filter(Comment == "Bottom 6") %>%
        select(Country) %>%
        unique() 
      cnt <- unlist(unclass(cnt))
      
      target <- subset(world_map, country %in% cnt)
      
      leaflet(target) %>% 
        addTiles() %>% 
        addPolygons(weight=1) %>%
        addProviderTiles("Stamen.TonerLite") %>%
        addEasyButton(easyButton(
          icon="fa-globe", title="Zoom to Standard",
          onClick=JS("function(btn, map){ map.setZoom(3); }")))
  })
  
}

shinyApp(ui,server)
