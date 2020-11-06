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
acled_data_11_6 <- readRDS("acled_data_11_6.RDS")

ui <- navbarPage(
    "Final Project",
    tabPanel("Violence at Protests",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Type of Protest",
                             c("BLM" = "blm", 
                               "Militia" = "militia", 
                               "Pro-Police" = "pro_police",
                               "Labor" = "labor")
                         )),
                     mainPanel(plotOutput("protestPlot")))
             )),
    tabPanel("Discussion"),
    tabPanel("About"))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$protestPlot <- renderPlot({
        violence_plot <- function(ps){
            acled_data_11_6 %>%
                filter(sub_event_type %in% c("Peaceful protest", "Protest with intervention",
                                             "Violent demonstration", "Mob violence")) %>%
                filter({{ps}} == TRUE) %>%
                select(sub_event_type, {{ps}}) %>%
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
}

# Run the application 
shinyApp(ui = ui, server = server)
