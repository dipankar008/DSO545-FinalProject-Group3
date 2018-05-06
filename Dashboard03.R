library(shiny)
library(maptools)
library(dplyr)
library(sp)
library(maps)
library(leaflet)
library(tidyr)
library(ggplot2)


WHR <- read.csv("WHR.csv")
easeofbiz <- read.csv("easeofbiz.csv")
WHR_TopBot <- read.csv("WHR_TopBot.csv")
WHR_map <- read.csv("WHR_map.csv")
WorldMap <- read.csv("WorldMap.csv")
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
                                  includeMarkdown("intro.Rmd")
                                  ),
                         tabPanel("Heat Map",
                                  sidebarLayout(
                                  sidebarPanel(radioButtons(inputId = "Ye",
                                               label = "Select Year",
                                               choices = c("2015","2016","2017"),
                                               selected = NULL ),
                                  helpText("Countries without data are highlighted in grey")),
                                  mainPanel(plotOutput(outputId = "XY", height = 600)))),

                         tabPanel("Parameter Description")
                                  )
                         ),
 
    tabPanel("Country Specific Information",
             fluidRow(
               column(3,
                      
                      helpText("Shows different parameters related with happiness for the selected countries"),
                      helpText("Atleast 1 country and atmost 3 countries"),
                      selectizeInput(inputId = "try", 
                                     label = "Select Countries" , 
                                     choices = WHR$Country, 
                                     selected = NULL, 
                                     multiple = T, 
                                     options = list(maxItems = 3)  )),
                        
              column(9,
                 leafletOutput(outputId = "map",height = 400))),
             br(),
             
             fluidRow(
               column(4, 
                           plotOutput(outputId = "graph")),
               column(3, offset = 0.5,
                           verticalLayout(helpText("How selected countries match against each other per year"), 
                                          tableOutput(outputId = "table1"))),
               column(2, offset = 1,
                      verticalLayout(helpText("Which country is best for starting a business"),
                                     tableOutput(outputId = "table"))))
                        
             ),
    
    tabPanel("Top 6 : Countries with improved ranking",
            
             navlistPanel(
               tabPanel("Rank Comparision",
                        plotOutput("top", height = 500)),
               tabPanel("Parameter Comparision",
                        plotOutput("top1", height = 650)),
               tabPanel("location",
                        leafletOutput("top2"))
             )
            ),
    tabPanel("Bottom 6 : Countries with decreased rankings",
             navlistPanel(
               tabPanel("Rank Comparision",
                        plotOutput("bot", height = 500)),
               tabPanel("Parameter Comparision",
                        plotOutput("bot1", height = 650)),
               tabPanel("location",
                        leafletOutput("bot2"))
               )
             ),
    tabPanel("Component 4")
  )
  )

server <- function(input, output) {
  
 output$XY <- renderPlot({

    WHR_map %>%
      filter(Year == as.character(input$Ye)) %>%
      group_by(Country) %>%
      arrange(order) %>%
      ungroup() %>%
      ggplot() +
      geom_polygon(data = WorldMap,aes(x = long, y = lat, group = group), color = "black", fill = "lightgrey", alpha = 0.5 ) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = Happiness.Score),color = "black") +
      scale_fill_gradient2(low = "white", mid = "orange", high = "darkred") +
      theme(legend.position = "bottom", 
            axis.title = element_blank(),
            axis.text = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(), 
            axis.ticks.length = unit(0, "cm"),
            axis.ticks.margin = unit(0, "cm")) + xlab("") + ylab("") + 
      ggtitle("Heatmap for Happiness Score accross the globe for the selected year") 
  })
  
  output$graph <- renderPlot({

    WHR %>%
      filter(Country %in%  input$try) %>%
      ggplot(aes(x= Year, y = Happiness.Score, group = Country, color = Country)) +
      geom_line(size =2, linetype = "dashed") + 
      geom_point(size = 5) + 
      geom_text(aes(label= Happiness.Rank), vjust = -1) +
      scale_x_continuous(breaks = c(2015,2016,2017)) + ylab("Happiness Score") +
      ggtitle("Happiness Score with respect to year, with Happiness Rank per year") +
      theme_bw() +theme(legend.position = "bottom")

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
     WHR %>%
      filter(Country %in% input$try) %>%
      select(Year, Country,Happiness.Rank ,Happiness.Score,Economy..GDP.per.Capita.:Generosity) %>%
      gather(Happiness.Rank: Generosity,key = "Parameter", value = "Score") %>%
      spread(key = Year, value = Score) %>%
      arrange(factor(Parameter, levels = c("Happiness.Rank", "Happiness.Score",
                                           "Economy..GDP.per.Capita.","Family","Freedom","Health..Life.Expectancy.",
                                           "Trust..Government.Corruption.")))
    
  })
  
  output$table <- renderTable({
    easeofbiz %>%
      filter(Economy %in% input$try) %>%
      select(Economy,Parameter,Rank) %>%
      spread(key = Economy, value = Rank) %>%
      arrange(factor(Parameter, levels = c("Ease.of.Doing.Business.Rank","Dealing.with.Construction.Permits",
                                           "Enforcing.Contracts", "Getting.Credit","Starting.a.Business","Getting.Electricity",
                                           "Paying.Taxes","Protecting.Minority.Investors", "Registering.Property",
                                           "Resolving.Insolvency","Trading.across.Borders")))
    
  }) 
  
  output$top <- renderPlot({
    
      WHR_TopBot %>%
        filter(Comment == "Top 6") %>%
        ggplot(aes(x=Country, y = Happiness.Score, group = as.factor(Year), fill = as.factor(Year))) +
      geom_col(position = position_dodge()) +
      geom_text(aes(label = Happiness.Rank), vjust = -1 ,position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("red", "orange", "brown"),
                        guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                             direction = "horizontal"))  + theme_bw() +
      ggtitle("Variation in Rank for the Top 6 performing countries") +
      theme(legend.position = "bottom")
      
    })
  
  output$top1 <- renderPlot({
      
    WHR_TopBot %>%
      gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
        filter(Comment == "Top 6") %>%
        arrange(Happiness.Rank)%>%
        ggplot(aes(x=Type, y= Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
        geom_text(aes(label = Happiness.Rank), size = 2.5 , hjust = -1 ,position = position_dodge(width = 1)) + 
        scale_fill_manual(values = c("red", "orange", "brown"),
                          guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                               direction = "horizontal")) +
        coord_flip() + facet_wrap(~Country)  +
      ggtitle("Change in parameters for years 2015-17, for the 6 top performing countries") + 
      theme(legend.position = "bottom")
    
  
  })
  
  output$top2 <- renderLeaflet({
    
    
    WHR_TopBot$Country <- as.character(WHR_TopBot$Country)
    cnt <- WHR_TopBot %>%
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
    
      WHR_TopBot %>%
        filter(Comment == "Bottom 6") %>%
        ggplot(aes(x=Country, y = Happiness.Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
      geom_text(aes(label = Happiness.Rank), vjust = -1 ,position = position_dodge(width = 1)) +
      scale_fill_manual(values = c("red", "orange", "brown"),
                        guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                             direction = "horizontal")) +
      ggtitle("Variation in Rank for the Bottom 6 performing countries") + theme_bw() + 
      theme(legend.position = "bottom")
    })
  output$bot1 <- renderPlot({
    
      WHR_TopBot %>%
        gather(Economy..GDP.per.Capita.:Generosity ,key = "Type", value = "Score", convert = T) %>%
        filter(Comment == "Bottom 6") %>%
        ggplot(aes(x=Type, y= Score, group = as.factor(Year), fill = as.factor(Year))) +
        geom_col(position = position_dodge()) +
      geom_text(aes(label = Happiness.Rank), size = 2.5, hjust = -1 ,position = position_dodge(width = 1)) +
        scale_fill_manual(values = c("red", "orange", "brown"),
                          guide = guide_legend(title = "Year (Happiness Rank at the top)",
                                               direction = "horizontal")) +
        coord_flip() + facet_wrap(~Country) + theme_bw() +
      ggtitle("Change in parameters for years 2015-17, for the 6 lowest performing countries") +
      theme(legend.position = "bottom")
    
     
  })
  
  output$bot2 <- renderLeaflet({
    
      
      WHR_TopBot$Country <- as.character(WHR_TopBot$Country)
      cnt <- WHR_TopBot %>%
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
