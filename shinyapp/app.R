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
acled_data <- readRDS("acled_data.RDS")
violence_comparison <- readRDS("violence_comparison.RDS")

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Final Project Milestone #5",
    tabPanel("Model",
             fluidPage(
                 titlePanel("Education Credentials and Parents' 
                            Immigration History"),
                 plotOutput("distPlot"))
    ),
    tabPanel("Discussion",
             titlePanel("Discussion"),
             p("I have created a few different data visualizations, but I was
               having issues uploading them to the ShinyApp — they can be 
               found in the RMD file for this project. I have included the one
               plot that I worked on during our recitation section, which
               looks at educational credentials and parents' immigration
               history. The dataset used for that plot was from the 
               Immigration and Intergenerational Mobility in Metropolitan 
               Los Angeles (IIMMLA) study. The other plots that I made (but 
               could not include in this ShinyApp) come from Opportunity 
               Insights, which I believe takes data from the Census.")),
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
}

# Run the application 
shinyApp(ui = ui, server = server)
