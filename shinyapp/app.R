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
library(readxl)
library(janitor)
library(tidycensus)
library(lubridate)
acled_data <- readRDS("acled_data.RDS")
rural <- readRDS("rural.RDS")
acled_data_11_6 <- readRDS("acled_data_11_6.RDS")
acled_data_11_12 <- readRDS("acled_data_11_12.RDS")

ui <- navbarPage(
    "Final Project",
    tabPanel("Maps",
        sidebarLayout(
            sidebarPanel(
                selectInput("var", 
                            label = "Type of Protest",
                            choices = list("Peaceful protest", 
                                           "Protest with intervention",
                                           "Violent demonstration", 
                                           "Mob violence"),
                            selected = "Peaceful protest"),
                
                dateRangeInput("daterange1", "Date range:",
                               start = "2020-05-24",
                               end = "2020-10-31")
            ),
            
            mainPanel(leafletOutput("mapPlot"))
        ),
    ),
    tabPanel("Violence at Protests",
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("plot_type",
                                     label = "Subject of Protest",
                                     choices = list("BLM" = "blm", 
                                                 "Militia" = "militia", 
                                                 "Pro-Police" = "pro_police",
                                                 "Labor" = "labor")
                                     )),
                     mainPanel(plotOutput("protestPlot"))),
                  p("I have written a function to create this graph, but for 
                    some reason it is not working! However, I asked a question 
                    about it in the final-project channel in the Slack and 
                    am hoping to figure out what is going wrong!")
             ),
    tabPanel("Discussion"),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello, this is where I talk about my project."),
             h3("About Me"),
             p("My name is Victoria Wang. 
               You can reach me at victoriawang@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$protestPlot <- renderPlot({
        violence_plot <- function(ps){
            acled_data_11_12 %>%
                filter(sub_event_type %in% c("Peaceful protest", "Protest with intervention",
                                             "Violent demonstration", "Mob violence")) %>%
                filter(subject == ps & group_boolean == TRUE) %>%
                select(sub_event_type) %>%
                group_by(sub_event_type) %>%
                summarize(count = n(), .groups = "drop") %>%
                ggplot(aes(x = factor(sub_event_type, levels = c("Mob violence",
                                                                 "Violent demonstration",
                                                                 "Protest with intervention",
                                                                 "Peaceful protest")), 
                           y = count)) + 
                geom_col() + 
                labs(title = "Violence during Protests",
                     subtitle = "Summer and Fall 2020",
                     x = "Type of Protest",
                     y = "Number of Protests") +
                scale_x_discrete(drop = FALSE) + 
                coord_flip() + 
                theme_classic()
        }
        violence_plot(input$plot_type)
        })
    
    output$mapPlot <- renderLeaflet({
        subset <- acled_data_11_6 %>%
            mutate(event_date = ymd(event_date)) %>%
            filter(event_date %in% (input$daterange1[1]:input$daterange1[2])) %>%
            filter(sub_event_type == input$var)
        
        pal <- 
            colorFactor(palette = "Reds",
                        levels = c("Peaceful protest", "Protest with intervention", 
                                   "Violent demonstration", "Mob violence"))
        
        leaflet(rural) %>%
            addTiles() %>%
            addCircleMarkers(lng = subset$longitude, lat = subset$latitude, radius = 0.1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
