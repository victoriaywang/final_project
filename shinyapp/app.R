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
library(shinythemes)
library(gtsummary)
library(broom.mixed)
library(gt)

acled_for_mapping <- readRDS("acled_for_mapping.RDS")
rural <- readRDS("rural.RDS")
acled_data_11_12 <- readRDS("acled_data_11_12.RDS")
model <- readRDS("model_12_6.RDS")
default <- readRDS("default_12_8.RDS")

ui <- navbarPage(theme = shinytheme("sandstone"), 
                 "Violence at Protests",
    tabPanel("Mapping Protests",
             titlePanel("Mapping protests in the United States"),

             # Select category of protest (peaceful, violent, etc). Then,
             # select a date range. The date range includes all of the dates
             # included in the ACLED dataset. 
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput("type",
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
                 
                 # Output is the leaflet map created in the server. 
                 
                 mainPanel(leafletOutput("mapPlot"))),
             ),
    
    tabPanel("Protest Subject and Violence",
             titlePanel("How are the subjects of protests and violence 
                        correlated?"),
             
             # Select the subject of the protest (BLM, pro-police, etc). 
             
             sidebarLayout(
                 sidebarPanel(
                     selectInput("plot_type",
                                 label = "Subject of Protest",
                                 choices = list("BLM" = "blm", 
                                                "Militia" = "militia", 
                                                "Pro-Police" = "pro_police",
                                                "Labor" = "labor")
                                 )),
                 
                 # Output is the plot of different types of protests by the
                 # subject of the protest, created in the server. 
                 
                 mainPanel(plotOutput("protestPlot")))
             ),
    
    tabPanel("Model: Posterior",
             titlePanel("What factors are correlated with the number of 
                        violent protests?"),
             
             # Used fluid_row, columns, and wellPanel to allow for greater 
             # flexibility in adjusting size of the panels. 
             
             # Set the default values for the sliderInput as the mean values 
             # for each parameter across my dataset, to two decimal places.
             # The max and min is the closest integer value to the max and min
             # of the dataset.
             
             fluidRow(column(width = 4, 
                             wellPanel(sliderInput("number_protests",
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
                                                   min = 0, max = 70, value = 9.87),
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
                                                   min = 0, max = 10, value = 2.98))),
                     
                      # Output will be the posterior probability distribution 
                      # created in the server. 
                      
                      column(width = 8, 
                             wellPanel(plotOutput("modelPlot"))))
             ),
    
    tabPanel("Model: Interpretation",
             titlePanel("Model Interpretation"),
             
             # Again, using fluid_row rather than sidebarPanel to allow for
             # greater flexibiliy in formatting. The left panel is the output
             # of the table created using gt. The right panel is text that 
             # offers some information and interpretation of my model.  
             
             fluidRow(column(width = 4, wellPanel(tags$h4(tags$b("Regression Table")),
                                                  gt_output("regressionTable"))),
                      
                      column(width = 8, wellPanel(
                          tags$h4(tags$b("Discussion")),
                          
                          p("I gathered and cleaned up data from various sources, ending up with 24 parameters that were not directly related to protests:",
                          tags$em("population, gini score (inequality), income, poverty, unemployment rate, % adults with less than a high school education, 
                          housing problems rate, % homeowners, segregation index 1, segregation index 2, % black, % non-hispanic white, % hispanic, 
                          % black and hispanic, teen birth rate, premature death rate, preventable hospitalization rate, life expectancy, physical distress,
                          mental distress, violent crime rate, homicide rate, firearm fatalities rate, number of police killings.")),
                          
                      p("Included in my final model are ten parameters that I found to be interesting: some have meaningful correlations 
                        (gini index, segregation, number of past police killings, housing problems), while others are interesting in their very lack of a meaningful correlation 
                        (poverty, physical distress)."),
                      
                      p("Below is a brief description for each parameter:"),
                      tags$ul(
                          tags$li(tags$b("Total Number of Protests"), "= number of protests, violent and non-violent, between May and October 2020"),
                          tags$li(tags$b("Income Inequality"), "= economic inequality in a population, where higher values indicate greater inequality"),
                          tags$li(tags$b("Poverty Rate"), "= poverty rate"),
                          tags$li(tags$b("Severe Housing Problems Rate"), "= % households with at least 1 of 4 housing problems: overcrowding, high housing costs, or lack of kitchen or plumbing facilities"),
                          tags$li(tags$b("Physical Distress Rate"), "= % adults reporting 14 or more days of poor physical health per month"),
                          tags$li(tags$b("Mental Distress Rate"), "= % adults reporting 14 or more days of poor mental health per month"),
                          tags$li(tags$b("Residential Segregation"), "= index of dissimilarity where higher values indicate greater residential segregation between non-White and White county residents."),
                          tags$li(tags$b("Number of Police Killings"), "= number of murders by police officers"),
                          tags$li(tags$b("% Black and Hispanic"), "= % Black and Hispanic"),
                          tags$li(tags$b("Teen Birth Rate"), "= births per 100 females ages 15-19")
                      )
)))),
                          
    tabPanel("About", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("My final project studies the nationwide protests, peaceful and violent, of this past summer and fall. Using protest data from the Armed Conflict Location & Event Data Project’s US Crisis Monitor, 
               I hope to understand what factors might help predict the likelihood of a protest turning violent. Combining protest data with county-level data about economic indicators (economic inequality, poverty, 
               median household income, social mobility), demographic information (level of education, teenage birth rate, race), other indicators of quality of life (premature death rate, violent crime rate, housing problems), 
               and other interesting indicators (political ideology), I find that one of the strongest predictors of violence at protests is income inequality."),
             h3("Data"),
             p("Data regarding protests comes primarily from", 
               tags$a(href = "https://acleddata.com/special-projects/us-crisis-monitor/", "The Armed Conflict Location & Event Data Project’s U.S. Crisis Monitor,"),
               "which offers data on political violence, demonstrations, and strategic developments in the United States beginning May 24, 2020."),
             p("Data for county-level parameters comes primarily from:"),
             p(tags$ul(
                 tags$li(tags$a(href = "https://data.census.gov/cedsci/", "The Census Bureau")),
                 tags$li(tags$a(href = "https://mappingpoliceviolence.org/", "Mapping Police Violence")),
                 tags$li(tags$a(href = "https://www.countyhealthrankings.org/", "The County Health Rankings & Roadmaps Program"))
             )),
             h3("About Me"),
             p("My name is Victoria Wang. I'm a first-year student at Harvard College taking Gov 50 this Fall 2020. You can reach me at victoriawang@college.harvard.edu!")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapPlot <- renderLeaflet({
        subset <- acled_for_mapping %>%
            mutate(event_date = ymd(event_date)) %>%
            filter(event_date %in% (input$daterange1[1]:input$daterange1[2])) %>%
            filter(sub_event_type == input$type)
        
        leaflet(rural) %>%
            addTiles() %>%
            addCircles(lng = subset$longitude, lat = subset$latitude, 
                       radius = 0.1, color = "navy")
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
                geom_col(fill = "dodgerblue4", color = "white", alpha = 0.7) + 
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
                theme(axis.text.y = element_text(size = 10, hjust = 1),
                      axis.text.x = element_text(size = 10),
                      axis.title.x = element_text(size = 12, face = "bold",
                                                  vjust = -1),
                      axis.title.y = element_text(size = 12, face = "bold",
                                                  vjust = 4),
                      panel.background = element_rect(fill = "white"), 
                      plot.background = element_rect(fill = "white"))
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
        
        new <- posterior_predict(fit, newdata = new_obs) %>%
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
                 title = "Posterior Prediction Distribution",
                 subtitle = "Number of Violent Protests in a Hypothetical County") +
            theme_economist() + 
            theme(title = element_text(size = 10), 
                  legend.text = element_text(size = 10),
                  axis.text.y = element_text(size = 8),
                  axis.text.x = element_text(size = 8),
                  axis.title.x = element_text(size = 10, face = "bold"),
                  axis.title.y = element_text(size = 10, face = "bold"),
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = "white")) +
            scale_fill_manual(name = "", 
                              breaks = c("default", "new"),
                              labels = c("Average County", "Hypothetical County"),
                              values = c("dodgerblue4", "darkgoldenrod"))
    })
    output$regressionTable <- render_gt({
        fit <- stan_glm(data = model,
                        formula = number_violent ~ teen_birth_rate + housing_problems + 
                            physical_distress + mental_distress + segregation_index_2 + 
                            police_killings + gini_score + number_protests + 
                            poverty + percent_black_hispanic, 
                        refresh = 0)
        
        tbl_regression(fit, 
                       estimate_fun = function(x) style_sigfig(x, digits = 3)) %>%
            as_gt() %>%
            gt::tab_header(title = "Regression of Violent Protest Occurrence")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
