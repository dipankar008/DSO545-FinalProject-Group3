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
    tabPanel("Component 3"),
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
        geom_col(position = position_dodge()) +
        geom_text(aes(label = Happiness.Rank)
    } else {
      data4 %>%
      gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
        filter(Comment == "Top 6") %>%
        ggplot(aes(x=Type, y= Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
        coord_flip() + facet_wrap(~Country)
    }
  })
  
}

shinyApp(ui,server)
