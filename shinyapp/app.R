#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
acled_data <- readRDS("acled_data.RDS")
violence_comparison <- readRDS("violence_comparison.RDS")
rural <- readRDS("rural.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Milestone #5",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Protest Visualization"),
                 leafletOutput("mapPlot"))
    ),
    tabPanel("Discussion",
             titlePanel("Discussion"),
             p("Text")),
    tabPanel("About", 
             titlePanel("About"),
             h3("About Me"),
             p("My name is Victoria Wang, and I am a first year at Harvard.
                You can reach me at victoriawang@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        ggplot(comparison, aes(x = reorder(sub_event_type, count), 
                               y = count, fill = type)) +
            geom_col(position = "identity") + 
            labs(title = "Violence at Protests",
                 subtitle = "Summer and Fall 2020",
                 x = "Type of Protest",
                 y = "Number of Protests") + 
            coord_flip() + 
            scale_color_manual(breaks = c("all", "blm"),
                               labels = c("All Protests", "BLM")) + 
            theme_classic()
    })
    output$mapPlot <- renderLeaflet({
        subset <- acled_data %>%
            mutate(event_date = ymd(event_date)) %>%
            filter(event_date <= "2020-06-01")
        
        leaflet(rural) %>%
            addTiles() %>%
            addCircles(lng = subset$longitude, lat = subset$latitude)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
