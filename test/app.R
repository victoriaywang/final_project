#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

### DUMP
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