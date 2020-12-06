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
library(rstanarm)
library(stringr)
library(naniar)
library(ggthemes)

acled_data <- readRDS("acled_data.RDS")
rural <- readRDS("rural.RDS")
acled_data_11_6 <- readRDS("acled_data_11_6.RDS")
acled_data_11_12 <- readRDS("acled_data_11_12.RDS")
model_12_2 <- readRDS("model_12_2.RDS")
model <- readRDS("model_12_6.RDS")
default <- readRDS("default_12_6.RDS")

ui <- navbarPage(
    "Violence at Protests: Summer and Fall 2020",
    tabPanel("Mapping Protests",
             titlePanel("Mapping protests in the United States during Summer and Fall 2020"),
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
    tabPanel("Correlation between Protest Subject and Violence",
                 titlePanel("How are the subjects of protests and violence correlated?"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("plot_type",
                                     label = "Subject of Protest",
                                     choices = list("BLM" = "blm", 
                                                 "Militia" = "militia", 
                                                 "Pro-Police" = "pro_police",
                                                 "Labor" = "labor")
                                     )),
                     mainPanel(plotOutput("protestPlot")))
             ),
    tabPanel("Modeling",
             titlePanel("What factors are correlated with the number of violent protests?"),
             sidebarLayout(
                 sidebarPanel(
                     sliderInput("number_protests",
                                 label = "Total Number of Protests",
                                 min = 0, max = 373, value = 4.89),
                     sliderInput("gini_score",
                                 label = "Income Inequality (higher = greater inequality)",
                                 min = 0, max = 1, value = 0.45),
                     sliderInput("poverty",
                                 label = "Poverty Rate",
                                 min = 2, max = 34, value = 12.41),
                     sliderInput("housing_problems",
                                 label = "Severe Housing Problems Rate",
                                 min = 0, max = 70, value = 13.87),
                     sliderInput("physical_distress",
                                 label = "Physical Distress Rate",
                                 min = 7, max = 25, value = 12.13),
                     sliderInput("mental_distress",
                                 label = "Mental Distress Rate",
                                 min = 8, max = 21, value = 12.99),
                     sliderInput("segregation_index_2",
                                 label = "Residential Segregation (higher = greater segregation)",
                                 min = 0, max = 90, value = 30.81),
                     sliderInput("police_killings",
                                 label = "Number of Police Killings",
                                 min = 0, max = 364, value = 2.7),
                     sliderInput("percent_black_hispanic",
                                 label = "% Black and Hispanic",
                                 min = 0, max = 96, value = 18.67),
                     sliderInput("teen_birth_rate",
                                 label = "Teen Birth Rate",
                                 min = 0, max = 10, value = 2.98)
                 ),
                 
                 mainPanel(plotOutput("modelPlot"))
             ),
    ),
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My final project studies the nationwide protests, peaceful and violent, of this past summer and fall. Using protest data from the Armed Conflict Location & Event Data Project’s US Crisis Monitor, I hope to understand what factors might help predict the likelihood of a protest turning violent. Combining protest data with county-level data about economic indicators (economic inequality, poverty, median household income, social mobility), demographic information (level of education, teenage birth rate, race), other indicators of quality of life (premature death rate, violent crime rate, housing problems), and other interesting indicators (political ideology), I find that one of the strongest predictors of violence at protests is income inequality."),
             h3("About Me"),
             p("My name is Victoria Wang. 
               You can reach me at victoriawang@college.harvard.edu.")))

# Define server logic required to draw a histogram
server <- function(input, output) {

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
            addCircleMarkers(lng = subset$longitude, lat = subset$latitude, radius = 0.1,
                             color = ~pal(acled_data_new$sub_event_type))
    })
    
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
                  geom_col(fill = "#d1495b", color = "white", alpha = 0.7) + 
                  labs(title = "Violence during Protests",
                       x = "Type of Protest",
                       y = "Number of Protests") +
                  scale_x_discrete(drop = FALSE, breaks = c("Mob violence",
                                                          "Violent demonstration",
                                                          "Protest with intervention",
                                                          "Peaceful protest"),
                                   labels = c("Mob \n violence",
                                              "Violent \n demonstration",
                                              "Protest with \n intervention",
                                              "Peaceful \n protest")) + 
                  theme_economist() + 
                  coord_flip() + 
                  theme(axis.text.y = element_text(size = 12, hjust = 1),
                        axis.text.x = element_text(size = 12),
                        axis.title.x = element_text(size = 14, face = "bold",
                                                    vjust = -1),
                        axis.title.y = element_text(size = 14, face = "bold",
                                                    vjust = 3))
        }
        violence_plot(input$plot_type)
        })
    
    output$modelPlot <- renderPlot({
        fit <- stan_glm(data = model,
                        formula = number_violent ~ teen_birth_rate + housing_problems + 
                            physical_distress + mental_distress + segregation_index_2 + 
                            police_killings + gini_score + number_protests + 
                            poverty + percent_black_hispanic, 
                        refresh = 0)
        
        new_obs <- tibble(number_protests = input$number_protests,
                          gini_score = input$gini_score,
                          poverty = input$poverty,
                          housing_problems = input$housing_problems,
                          physical_distress = input$physical_distress,
                          mental_distress = input$mental_distress,
                          segregation_index_2 = input$segregation_index_2,
                          police_killings = input$police_killings,
                          percent_black_hispanic = input$percent_black_hispanic,
                          teen_birth_rate = input$teen_birth_rate)
        
        new <- posterior_predict(fit_all, newdata = new_obs) %>%
            as_tibble() %>%
            mutate_all(as.numeric) %>%
            rename("new" = `1`)
        
        full <- default %>%
            bind_cols(new) %>%
            pivot_longer(cols = default:new,
                         names_to = "pp_results",
                         values_to = "number_violent")
        
        full %>%
            ggplot(aes(x = number_violent, fill = pp_results)) +
            geom_histogram(aes(y = after_stat(count/sum(count))), bins = 75,
                           color = "white", position = "identity", alpha = 0.6) +
            scale_y_continuous(labels = scales::percent_format()) + 
            xlim(c(-10,20)) +
            labs(x = "Number of Violent Protests",
                 y = "Probability",
                 title = "Model: Number of Violent Protests \n in a Hypothetical County") +
            theme_economist() + 
            theme(title = element_text(size = 10), 
                  legend.text = element_text(size = 10),
                  axis.text.y = element_text(size = 8),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 10, face = "bold"),
                  axis.title.y = element_text(size = 10, face = "bold")) +
            scale_fill_manual(name = "", 
                              breaks = c("default", "new"),
                              labels = c("Average County", "Hypothetical County"),
                              values = c("#00798c", "#d1495b"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
