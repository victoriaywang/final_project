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
acled_w_subjects <- readRDS("acled_w_subjects.RDS")

ui <- navbarPage(theme = shinytheme("sandstone"),
                 "Violence at Protests",
                 
  # Note: the tabPanel indentation is further left than it should be to make the
  # formatting for the rest of the Shiny app look slightly neater. 
    
  tabPanel("Mapping Protests",
           titlePanel("Mapping Protests in the United States"),

             # Select a date range. Then, select category of protest (peaceful, 
             # violent, etc). The date range includes all of the dates
             # included in the ACLED dataset. 
             
            sidebarLayout(
                sidebarPanel(
                    dateRangeInput("daterange1", "Date Range",
                                   start = "2020-05-24",
                                   end = "2020-10-31"),
                    selectInput("type",
                                label = "Type of Protest",
                                choices = list("Peaceful protest", 
                                               "Protest with intervention",
                                               "Violent demonstration",
                                               "Mob violence"),
                                selected = "Peaceful protest"),
                    p("- - - - - - - -"),
                    
                    # Offering more in-depth explanations of each type of 
                    # protest based on the ACLED Codebook.
                    
                    p(tags$b("Defining Protest Types:")),
                    tags$ul(
                      tags$li(
                        tags$b("Peaceful protest"), 
                        "is used when demonstrators are engaged in a protest 
                        while not engaging in violence or other forms of rioting
                        behaviour."),
                      tags$li(
                        tags$b("Protest with intervention"), 
                        "is used when individuals are engaged in a peaceful 
                        protest during which there is an attempt to disperse or 
                        suppress the protest without serious/lethal injuries 
                        being reported or the targeting of protesters with 
                        lethal weapons."),
                      tags$li(
                        tags$b("Violent demonstration"), 
                        "is used when a group of individuals engages in a 
                        demonstration involving violence. Examples of rioting 
                        behaviour include vandalism; road-blocking using 
                        barricades, burning tires, or other material; other 
                        types of violent and/or destructive behaviour are also 
                        included here."),
                      tags$li(
                        tags$b("Mob violence"), 
                        "is used when rioters violently interact with other 
                        rioters, another armed group, or civilians."))),
                 
                 # Output is the leaflet map created in the server. 
                 
                mainPanel(leafletOutput("mapPlot"),
                          "- - - - - - - -",
                          p(tags$em("Note: darker points indicate a larger
                                     number of protests. The map is created this
                                     way because the data regarding protests
                                     included general county-level location 
                                     coordinates rather than the specific 
                                    location of the protest itself.")))),
             ),
    
  
  tabPanel("Visualizing Protest Data",
           titlePanel("Data Visualizations"),
           
           # Select the subject of the protest (BLM, pro-police, etc). 
             
           # I used fluidRow because I had two separate visualizations (and 
           # accompanying selectInput boxes), and I wanted to be able to have
           # more control over the sizes of these panels.  
           
           fluidRow(
               column(width = 8, wellPanel(plotOutput("timePlot"))),
               column(width = 4, 
                      wellPanel(
                          
                          # I chose checkboxGroupInput because I wanted the user
                          # to be able to pick the type of protest in question. 
                          # This was particularly necessary for this plot 
                          # because the number of peaceful protests was so large
                          # relative to the other types that it can be difficult
                          # to see the other types of protests on the graph when
                          # all four types are included. 
                          
                          checkboxGroupInput(
                              "protest_type",
                              label = "Select the type of protest:",
                              choices = list("Peaceful protest",
                                             "Protest with intervention",
                                             "Violent demonstration",
                                             "Mob violence"),
                              selected = c("Peaceful protest",
                                           "Protest with intervention",
                                           "Violent demonstration",
                                           "Mob violence"))))),
             
           fluidRow(
               column(width = 8, wellPanel(plotOutput("protestPlot"))),
               column(width = 4, 
                      wellPanel(
                          selectInput(
                              "plot_type",
                              label = "Select the subject of protest",
                              choices = list("BLM" = "blm", 
                                             "Militia" = "militia",
                                             "Pro-Police" = "pro_police",
                                             "Labor" = "labor")))))
                 
                 # Output is the plot of different types of protests by the
                 # subject of the protest, created in the server. 
             ),
    
  tabPanel("Model",
           titlePanel("Model Interpretation"),
             
             # Again, using fluid_row rather than sidebarPanel to allow for
             # greater flexibiliy in formatting. The right panel is the output
             # of the table created using gt. The left panel is text that 
             # offers some information and interpretation of my model.  
           
           # Note: the following lines extend for beyond 80 characters. This was
           # intentional, because I had many long chunks of text, and it would 
           # likely take up a slightly ridiculous number of lines if I actually
           # kept line length to 80 characters.
             
           fluidRow(
               column(width = 8,
                      wellPanel(
                          tags$h4(tags$b("About the Model")), 
                          p("I gathered and cleaned data from various sources, ending up with 24 parameters that were not directly related to protests:",
                          tags$em("population, gini score (inequality), income, poverty, unemployment rate, % adults with less than a high school education, 
                                  housing problems rate, % homeowners, segregation index 1, segregation index 2, % black, % non-hispanic white, % hispanic, 
                                  % black and hispanic, teen birth rate, premature death rate, preventable hospitalization rate, life expectancy, physical distress,
                                  mental distress, violent crime rate, homicide rate, firearm fatalities rate, number of police killings.")),
                           p("Included in my final model are ten parameters that I found to be interesting: some have meaningful correlations (gini index, 
                             segregation, number of past police killings, housing problems), while others are interesting in their very lack of a meaningful 
                             correlation (poverty, physical distress)."),
                          p("Below is a brief description for each parameter:"),
                          tags$ul(
                              tags$li(tags$b("Total Number of Protests (number_protests)"), "= number of protests, violent and non-violent, between May and October 2020"),
                              tags$li(tags$b("Gini Index (gini_score)"), "= economic inequality in a population, where higher values indicate greater inequality"),
                              tags$li(tags$b("Poverty Rate (poverty)"), "= poverty rate"),
                              tags$li(tags$b("Severe Housing Problems Rate (housing_problems)"), "= % households with at least 1 of 4 housing problems: overcrowding, 
                                             high housing costs, or lack of kitchen or plumbing facilities"),
                              tags$li(tags$b("Physical Distress Rate (physical_distress)"), "= % adults reporting 14 or more days of poor physical health per month"),
                              tags$li(tags$b("Mental Distress Rate (mental_distress)"), "= % adults reporting 14 or more days of poor mental health per month"),
                              tags$li(tags$b("Residential Segregation (segregation_index_2)"), "= index of dissimilarity where higher values indicate greater residential 
                                             segregation between non-White and White county residents."),
                              tags$li(tags$b("Number of Police Killings (police_killings)"), "= number of murders by police officers"),
                              tags$li(tags$b("% Black and Hispanic (percent_black_hispanic)"), "= % Black and Hispanic"),
                              tags$li(tags$b("Teen Birth Rate (teen_birth_rate)"), "= births per 100 females ages 15-19")),
                          tags$h4(tags$b("Interpretation")),
                          p("Based on the this model, income inequality as measured by the gini coefficient is a powerful predictor of the number of violent protests 
                            that will occur in a specific county. The model suggests a surprising inverse correlation between economic inequality and number of violent 
                            protests: counties where inequality is higher are likely to experience a smaller number of protests. Several other parameters also had surprising 
                            impacts on the number of violent protests: greater residential segregation, a higher number of police killings, and a higher rate of individuals 
                            facing severe housing problems were all associated with a smaller number of violent protests. On the other hand, a higher percentage of adults 
                            experiencing poor mental health was correlated with a higher number of violent protests. Factors such as poverty, physical distress, racial demographics, 
                            and teen birth rate did not have a definitive correlation with number of violent protests."),
                          p("Why do counties with greater inequality — both in terms of economic inequality and in residential segregation — tend to experience fewer violent protests?"),
                          p("One potential explanation lies in the relative power theory, which highlights money as a political resource. Frederick Solt’s research on
                            economic inequality and nonviolent protest revealed a similar pattern of higher inequality corresponding to fewer protests.", 
                            tags$sup(1), 
                            "Solt theorizes that inequality results in an imbalance in political as well as economic power; when political discourse is almost entirely defined 
                            by the wealthiest individuals, individuals of lower socioeconomic backgrounds may simply withdraw from a political environment that is unresponsive 
                            to their demands. As individuals become less politically engaged, the likelihood of a nonviolent protest occurring could also decrease — and the same reasoning 
                            may apply to violent protests."),
                          p("Another possibility relates to the situational aspects that tend to lead to violence at protests. Research by sociologist Anne Nassauer and research
                            by Brandon Ives and Jacob S. Lewis independently concluded that protests tend to become violent for situational reasons, with disorganized protests 
                            being more likely than organized ones to turn violent.", 
                            tags$sup(2), 
                            "Counties with higher inequality could be more likely to have existing structures for organizing protests and other forms of political action, 
                            potentially decreasing the likelihood of the emergence of violence."),
                          p("Finally, the correlation could also be due to a limitation in the creation of this model. The difficulty — or even impossibility — of obtaining data 
                            on the specific individuals who participated in violent protests means that the model relied on data for the counties in which protests occurred. As a result, 
                            the degree of inequality that exists in a county may not effectively reflect the situation at the scene of each protest."),
                          p("- - - - - - - -"),
                          p(tags$sup(1), "Frederick Solt, “Economic Inequality and Nonviolent Protest,”", 
                            tags$em("Social Science Quarterly"), 
                            "96, no. 5 (2015): 1314–1327,",
                            tags$a("https://doi.org/10.1111/ssqu.12198", href = "https://onlinelibrary.wiley.com/doi/abs/10.1111/ssqu.12198"),
                            "."),
                          p(tags$sup(2), "Anne Nassauer, “Situational Dynamics and the Emergence of Violence in Protests,”",
                            tags$em("Psychology of Violence"), 
                            "8, no. 3 (2018): 293–304,",
                            tags$a("https://doi.org/10.1037/vio0000176", href = "https://doi.org/10.1037/vio0000176"),
                            "; Brandon Ives and Jacob S. Lewis, “From Rallies to Riots: Why Some Protests Become Violent,”", 
                            tags$em("The Journal of Conflict Resolution"), 
                            "64, no. 5 (2019):",
                            tags$a("https://doi.org/10.1177/0022002719887491", href = "https://doi.org/10.1177/0022002719887491"),
                            "."))),
               
               column(width = 4, wellPanel(gt_output("regressionTable"))))
           ),

  
  tabPanel("Violent Protests in a Hypothetical County",
           titlePanel("What factors are correlated with the number of violent protests?"),
           p("Use the sliders to change the ten given parameters, creating a 
             hypothetical county. The model will then output a posterior 
             probability distribution of the number of violent protests that 
             would occur in this hypothetical county. Sliders are currently set 
             to represent the mean value for each parameter for all counties in 
             this dataset."),
             p(tags$em("Note: more detailed information about each of these 
                       parameters can be found under the"),
               "Model: Interpretation", 
               tags$em("tab.")),
         
         # Used fluid_row, columns, and wellPanel to allow for greater 
         # flexibility in adjusting size of the panels. 
         
         # Set the default values for the sliderInput as the mean values 
         # for each parameter across my dataset, to two decimal places.
         # The max and min is the closest integer value to the max and min
         # of the dataset.
         
         fluidRow(
             column(
                 width = 4, 
                 wellPanel(
                     p(tags$h4(
                         tags$b("Violent Protests in a Hypothetical County"))),
                     sliderInput("number_protests",
                                 label = "Total Number of Protests",
                                 min = 0, max = 373, value = 4.89),
                     sliderInput("gini_score",
                                 label = "Income Inequality (higher = greater 
                                 inequality)",
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
                                 label = "Residential Segregation (higher = 
                                 greater segregation)",
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
                  
             column(
                 width = 8,
                 wellPanel(plotOutput("modelPlot"))))
         ),
  
  # The text in the following tabPanel often go beyond 80 characters, like in
  # the second tabPanel. Again, this is because many of the lines contain
  # a large amount of text and I did not want to separate it into an 
  # extremely large number of lines. 
                          
  tabPanel("About", 
           h3("Project Background and Motivations"),
           p("My final project studies nationwide protests, peaceful and violent, of summer and fall 2020. Using protest data from the Armed Conflict Location & Event Data Project’s", 
             tags$em("US Crisis Monitor,"),
             "I hope to understand what factors might help predict the likelihood of a protest turning violent. Combining protest data with county-level data about economic indicators 
             (economic inequality, poverty, median household income, social mobility), demographic information (level of education, teenage birth rate, race), and other indicators of 
             quality of life (premature death rate, violent crime rate, housing problems), I find that one of the strongest predictors of violence at protests is income inequality."),
           p(tags$a(href = "https://github.com/victoriaywang/final_project", "Here"), "is a link to the GitHub repo."),
           h3("About the Data"),
           p("Data regarding protests come primarily from The Armed Conflict Location & Event Data Project's", 
             tags$a(href = "https://acleddata.com/special-projects/us-crisis-monitor/", "U.S. Crisis Monitor,"),
             "which offers 2,400 sources and has partnered with organizations such as MilitiaWatch and Political Research Associates to improve the quality of its data. ACLED does not 
             crowdsource data, given the difficulty of verifying this information. For each protest event, ACLED reports factual information: who was involved, where it happened, 
             when it occurred, what occurred, 
             and a basic chronology of events."),
           p("Data for county-level parameters come from:"),
           p(tags$ul(
               tags$li(tags$a(href = "https://data.census.gov/cedsci/", 
                              "The Census Bureau")),
               tags$li(tags$a(href = "https://mappingpoliceviolence.org/", 
                              "Mapping Police Violence")),
               tags$li(tags$a(href = "https://www.countyhealthrankings.org/", 
                              "The County Health Rankings & Roadmaps Program"))
               )),
           p("I also explored county-level data from the following
             sources, though they were not included in the final model:"),
           p(tags$ul(
               tags$li(tags$a(href = "https://americanideologyproject.com/", 
                              "The American Ideology Project")),
               tags$li(tags$a(href = "https://www.opportunityatlas.org/",
                              "The Opportunity Atlas")),
               tags$li(tags$a(href = "https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/",
                              "USA Facts"))
               )),
           h3("About Me"),
           p("My name is Victoria Wang. I'm a first-year student at Harvard 
             College taking Gov 50 this Fall 2020. You can reach me at 
             victoriawang@college.harvard.edu!")))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$mapPlot <- renderLeaflet({
        subset <- acled_for_mapping %>%
            mutate(event_date = ymd(event_date)) %>%
            filter(event_date %in% 
                       (input$daterange1[1]:input$daterange1[2])) %>%
            filter(sub_event_type == input$type)
        
        # The previous lines of code create a subset of the ACLED dataset that 
        # would reflect the choices the user makes in the type of protest
        # and in the event date. 
        
        leaflet(rural) %>%
            addTiles() %>%
            addCircles(lng = subset$longitude, lat = subset$latitude, 
                       radius = 0.1, color = "navy", opacity = 0.2)
        
        # The longitude and latitude are set to come from subset, which is the 
        # new dataset that is responsive to the user's options for type of 
        # protest and date range. Opacity is set at 0.2 because the ACLED data
        # provides longitude and latitude based on the city/county where 
        # protests occurred, not the specific location — therefore, there are
        # many overlapping points. Changing the opacity to be quite light allows
        # us to see which areas actually had more protests. 
        
    })
    
    output$protestPlot <- renderPlot({
        violence_plot <- function(ps){
            acled_data_11_12 %>%
            
            # acled_data_11_12 is a version of the ACLED data that was lightly
            # cleaned in the data_gathering.Rmd file. 
            
                filter(sub_event_type %in% c("Peaceful protest", 
                                             "Protest with intervention",
                                             "Violent demonstration", 
                                             "Mob violence")) %>%
            
            # I filter for only protest activity — 'strategic developments', 
            # 'attacks', and other sorts of unrest are not important for this 
            # section. 
            
                filter(subject == ps & group_boolean == TRUE) %>% 
                select(sub_event_type) %>%
                group_by(sub_event_type) %>%
                summarize(count = n(), .groups = "drop") %>%
            
            # This filters for the subject of the protest (which is indicated
            # by 'ps', which will later be set as the input$plot_type as 
            # defined in the ui section). It then counts the number of rows,
            # grouped by the type of protest, which reflects the number of 
            # protests of each type. 
            
                ggplot(aes(x = factor(sub_event_type, 
                                      levels = c("Mob violence",
                                                 "Violent demonstration",
                                                 "Protest with intervention",
                                                 "Peaceful protest")), 
                           y = count)) + 
            
            # I used factor and levels to sort the x axis variables in a way
            # that made logical sense — they are sorted in order of the degree 
            # of violence, with "Peaceful Protest" being the least escalated
            # and "Mob Violence" the most. 
            
                geom_col(fill = "dodgerblue4", color = "white", alpha = 0.7) + 
                labs(title = "Violence at Protests",
                     x = "Type of Protest",
                     y = "Number of Protests") +
                scale_x_discrete(drop = FALSE, 
                                 breaks = c("Mob violence",
                                            "Violent demonstration",
                                            "Protest with intervention",
                                            "Peaceful protest"),
                                 labels = c("Mob \n violence",
                                            "Violent \n demonstration",
                                            "Protest with \n intervention",
                                            "Peaceful \n protest")) + 
            
            # I drop all NA values and change the labels slightly for aesthetic
            # purposes. 
            
                theme_economist() + 
                coord_flip() + 
                theme(title = element_text(size = 14),
                      axis.text.y = element_text(size = 10, hjust = 1),
                      axis.title.x = element_text(vjust = -1),
                      axis.title.y = element_text(vjust = 2),
                      panel.background = element_rect(fill = "white"), 
                      plot.background = element_rect(fill = "white"))
          
          # Some modifications to make the plot prettier. 
          
        }
        violence_plot(input$plot_type)
        })
    
    output$timePlot <- renderPlot({
        acled_by_date <- acled_w_subjects %>%
            filter(sub_event_type %in% c("Peaceful protest", 
                                         "Protest with intervention",
                                         "Violent demonstration", 
                                         "Mob violence")) %>%
            group_by(event_date, sub_event_type) %>%
            summarize(count = n(), .groups = "drop", subject) %>%
            filter(sub_event_type %in% input$protest_type)
        
        # I used group_by and summarize to find the number of protests that
        # would occur on a given date, separated by the type of protest.
        
        ggplot(acled_by_date, aes(x = event_date, y = count, 
                                  color = sub_event_type)) +
            geom_line() +
            
            # I chose to use a regular geom_line, though I also experimented
            # with stacking the numbers for the different types of protests.
            
            scale_color_manual(breaks = c("Peaceful protest", 
                                          "Protest with intervention",
                                          "Violent demonstration", 
                                          "Mob violence"),
                               name = "Type of Protest",
                               values = c("dodgerblue4", "seagreen4", 
                                          "#d1495b", "#edae49"),
                               labels = c("Peaceful protest", 
                                          "Protest with intervention",
                                          "Violent demonstration", 
                                          "Mob violence")) + 
            theme_economist() + 
            labs(x = "Date", y = "Number of Protests",
                 title = "Protests Over Time") + 
            
            # Changing the font sizes for aesthetic purposes and also to
            # standardize sizes between the different visualizations in this 
            # Shiny app.
            
            theme(title = element_text(size = 14), 
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = "white"),
                  axis.title.y = element_text(vjust = 2)) 
    })
    
    output$modelPlot <- renderPlot({
        fit <- stan_glm(data = model,
                        formula = number_violent ~ number_protests + 
                          gini_score + poverty + housing_problems + 
                          physical_distress + mental_distress + 
                          segregation_index_2 + police_killings  + 
                          percent_black_hispanic + teen_birth_rate,
                        refresh = 0)
        
        # Creating the actual model based off of cleaned data (ACLED data in 
        # addition to parameter data) from the data_gathering.Rmd file. This 
        # formula predicts the number of violent protests based on various
        # other parameters. 
        
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
        
        # I create a new_obs tibble that uses values that the user inputs using
        # the slider function in the ui section. 
        
        new <- posterior_predict(fit, newdata = new_obs) %>%
            as_tibble() %>%
            mutate_all(as.numeric) %>%
            rename("new" = `1`)
        
        # I used posterior_predict because I am trying to predict the number of
        # violent protests that would occur in a hypothetical county, rather
        # than making statements about all counties in general. 
        
        full <- default %>%
            bind_cols(new) %>%
          
          # "default" is a dataset that I created in the data_gathering.Rmd 
          # file. It is essentially the same as the "new" object, except that 
          # the inputs in newdata are the mean values for the dataset rather
          # than the values that the user inputs. I chose to keep this dataset
          # static so that the model wouldn't be rerunning the posterior_predict
          # every single time the slider was altered. In addition, the general
          # reason for including this default set is to allow for an easier
          # comparison, making it easier to see how changing the input 
          # parameters changes the number of violent protests. 
          
          # I then bind the "default" and "new" datasets together.
          
            pivot_longer(cols = default:new,
                         names_to = "pp_results",
                         values_to = "number_violent")
        
        # pivot_longer() in this case is helpful only for the creation of the 
        # ggplot. 
        
        full %>%
            ggplot(aes(x = number_violent, fill = pp_results)) +
            geom_histogram(aes(y = after_stat(count/sum(count))), bins = 75,
                           color = "white", position = "identity", 
                           alpha = 0.6) +
            scale_y_continuous(labels = scales::percent_format()) + 
          
          # I create a histogram with values from the full dataset, with 
          # results from the posterior_predicts of our hypothetical county 
          # ("new") as well as our mean county ("default"). The y = after_stat()
          # changes the y-axis from a count to a probability, and the 
          # percent_format() clarifies that it is a percentage. 
          
            labs(x = "Number of Violent Protests",
                 y = "Probability",
                 title = "Posterior Prediction Distribution",
                 subtitle = 
                     "Number of Violent Protests in a Hypothetical County") +
            theme_economist() + 
            theme(title = element_text(size = 14), 
                  panel.background = element_rect(fill = "white"), 
                  plot.background = element_rect(fill = "white")) +
            scale_fill_manual(name = "", 
                              breaks = c("default", "new"),
                              labels = c("Average County", 
                                         "Hypothetical County"),
                              values = c("dodgerblue4", "darkgoldenrod"))
        
        # Aesthetic modifications. 
        
    })
    output$regressionTable <- render_gt({
        fit <- stan_glm(data = model,
                        formula = number_violent ~ number_protests + 
                          gini_score + poverty + housing_problems + 
                          physical_distress + mental_distress + 
                          segregation_index_2 + police_killings  + 
                          percent_black_hispanic + teen_birth_rate,
                        refresh = 0)
        
        # This is the same fit model as the one in the previous section. 
        
        tbl_regression(fit, 
                       estimate_fun = function(x) 
                           style_sigfig(x, digits = 3)) %>%
            as_gt() %>%
            gt::tab_header(title = "Regression of Violent Protest Occurrence",
                           
                           # The following line goes beyond 80 characters
                           # because I don't want to add a line break in the 
                           # subtitle.
                           
                           subtitle = "The Effect of Various Parameters on Number of Violent Protests")
        
        # I used tbl_regression to create a nice table of results, which
        # includes a 95% confident interval. I set the number of digits to 
        # 3 because many of my values are quite small, given the generally 
        # small number of violent protests that occur in any given county. 
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
