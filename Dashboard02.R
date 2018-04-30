library(shiny)
library(maptools)
library(plotly)
library(dplyr)


data = read.csv("WHR.csv")
data2 = read.csv("easeofbiz_update.csv")


ui = shinyUI(
  fluidPage(
  
  title = "Pursuit of Happiness",
  windowTitle = "What are the parameters of happy country",
  
  
  fluidRow(
        column(3,
    
           helpText("Shows different parameters related with happiness for the selected country"),
           selectizeInput(inputId = "try", 
                   label = "Enter the name of Country" , 
                   choices = data$Country, selected = NULL, multiple = FALSE ))
    ),
  
  #plotOutput(outputId = "map")
  fluidRow(
    plotOutput(outputId = "map", width = "120%", height = "500px")
    ),
  fluidRow(
    splitLayout(cellWidths = c("70%", "30%"), plotOutput(outputId = "graph"), tableOutput(outputId = "table"))
  )
  ))
  
  

server <- function(input, output) {
  
 
  output$graph <- renderPlot({

    data %>%
      filter(Country ==  input$try) %>%
      ggplot(aes(x= as.factor(Year), y = Happiness.Score, fill = as.factor(Year))) +
      geom_col(position = position_dodge())

  })
 
  output$table <- renderTable({
    data2 %>%
      filter(Economy == input$try) %>%
      select(Parameter,Rank)
      
  }) 
  
  output$map <- renderPlot({
    data(wrld_simpl)
    myCountries = wrld_simpl@data$NAME %in% c(input$try)
    plot(wrld_simpl, col = c(gray(.80), "red")[myCountries+1])
  })
}

shinyApp(ui, server)
