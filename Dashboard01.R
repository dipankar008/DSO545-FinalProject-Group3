library(shiny)
library(maptools)
library(plotly)
library(dplyr)


data = read.csv("WHR.csv")

ui <- fluidPage(
  titlePanel(title = "Pursuit of Happiness",
             windowTitle = "What are the parameters of happy country"),
  
  sidebarLayout(position = "left",
    sidebarPanel(
      helpText("Shows different parameters related with happiness for the selected country"),
      
      
      selectizeInput(inputId = "try", 
                     label = "Enter the name of Country" , 
                     choices = data$Country, selected = NULL, multiple = FALSE )
    ),
    mainPanel(width = 12,
      fluidRow(
        splitLayout(cellWidths = c("70%", "30%"), plotOutput(outputId = "map"), plotOutput(outputId = "graph"))
      )
     
      )
  
  
))

server <- function(input, output) {
  
  #x <- reactive({input$list})  output$text <- renderText({    x()  })
  
  
  output$graph <- renderPlotly({
    
      ggplotly(data %>%
      filter(Country ==  input$try) %>%
       ggplot(aes(x= Year, y = Happiness.Rank, fill = Year)) +
      geom_col(position = position_dodge())
      )
  })
  
  
  output$map <- renderPlot({
    data(wrld_simpl)
    myCountries = wrld_simpl@data$NAME %in% c(input$try)
    plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
  })
}

shinyApp(ui, server)

