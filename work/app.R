
library(shinythemes)
library(tidyverse)
library(shiny)

# I copied in the needed data



#I used the navbar template and then defined all the panels

ui <- navbarPage(theme = shinytheme("united"), "Effects of IMF Structural Adjustment Programs on Angola",
                 tabsetPanel(
                     
                     #I then created the dropdown menu in the ui
                     
                     tabPanel("Interactive Comparison with Regression: Angola and Namibia",
                              
                              # Application title
                              
                              titlePanel("Interactive Comparison with Regression: Angola and Namibia"),
                              
                              #Select a single, unique metric by consulting the code found here https://github.com/diegomartinez1221/baseball_aging_curve/blob/master/Baseball_Aging_Curve/app.R
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("Metric",label = strong("Metric"),
                                                  choices = unique(complete_data_untrimmed$Metric),
                                                  selected = "Adjusted savings: education expenditure (% of GNI)",
                                                  multiple = FALSE
                                      )),
                                  
                                  # Show the dropdown plot
                                  
                                  mainPanel(
                                      plotOutput("dropdown"),
                                      p("This graphic allows you to parse through all of the World Bank and World Health Organization data for these two countries based upon Angola's Structural Adjustment Program status."),
                                      p("Explore Away!")))),
                     tabPanel("About",
                              titlePanel("About"),
                              p("This is the demo day draft of Sam Lowry's final project for GOV 1005 at Harvard College."),
                              p("You can contact him at samlowry@college.harvard.edu"),
                              uiOutput("tab")),
                     tabPanel("Context",
                              titlePanel("Context"),
                              p("This project centers around the effects of International Monetary Fund loans in the form of Structural Adjustment Programs on developing countries."),
                              p("The issue at hand is that these loans have existed since the 1950s, and the data are sparse during that period in developing countries. In order to combat these issues, I am using Angola as a case study due to its unique position. It had never received any IMF assistance until late in 2009 (after the composition of the 2009 budget hence beginning the comparison in 2010). Thus, I am able to compare Angola's data before and after the introduction of the SAP."),
                              p("In addition, I am using Namibia as a control. It is a similar country to Angola, but it has never received IMF loans. Thus, I am able to somewhat eliminate selection bias."),
                              
                              #used this website to imput image https://www.r-bloggers.com/building-shiny-app-exercises-part-1/\
                              
                              img(src = "map.png", align = "center", height = 500, width = 500)),
                     tabPanel("Selection Bias",
                              titlePanel("Selection Bias"),
                              p("Here, there will be graphics and context information around selection bias. I will compare the economies and history of the two countries while also acknowledging the shortcomings of the comparison."), plotOutput("selectionbias")),
                     tabPanel("Significant Differences",
                              titlePanel("Significant Differences"),
                              #Select a single, unique metric by consulting the code found here https://github.com/diegomartinez1221/baseball_aging_curve/blob/master/Baseball_Aging_Curve/app.R
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("Metric",label = strong("Metric"),
                                                  choices = unique(complete_data_untrimmed$Metric),
                                                  selected = "Adjusted savings: education expenditure (% of GNI)",
                                                  multiple = FALSE
                                      )),
                                  
                                  # Show the dropdown plot
                                  
                                  mainPanel(
                                      plotOutput("dropdown"),
                                      p("work?"))
                              ))))

# Define the server

server <- function(input, output) {
    
    #specified reactive drop down based on metric
    
    subset<-reactive({complete_data_untrimmed %>% filter(Metric %in%input$Metric)})
    
    
    output$dropdown <- renderPlot({
        
        # dcreates plot from groups of metrics
        
        ggplot(subset(), aes(x=subset()$Year, y=subset()$Value, color = subset()$SAP)) +
            geom_point() +
            
            #created linear regression 
            
            geom_smooth(method = "lm") +
            facet_wrap(~ Country) +
            labs(x = "Year",
                 y = "Value Based Upon Metric",
                 title = "World Bank and World Health Organization Data",
                 color = "Structural Adjustment Program")
    }
    )
    
    #https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny used code from here to make URL link
    
    url <- a("GitHub Repo", href="https://github.com/SamuelLowry/gov_1005_final_project")
    output$tab <- renderUI({
        tagList("Link to Code:", url)
    })
    
    
    
    
    
    output$selectionbias <- renderPlot({
        
        #showed an example of a comparison to account for selection bias
        
        complete_data_untrimmed %>% 
            filter(Metric == "Access to clean fuels and technologies for cooking (% of population)") %>% 
            ggplot(aes(x = Year, y = Value, color = SAP)) +
            geom_point() +
            geom_smooth(method = "lm") +
            facet_wrap(~ Country) +
            labs(title = "Access to clean fuels and technologies for cooking (% of population)",
                 y = "Percent of Population",
                 color = "Structural Adjustment Program") })
    
    
}


# Run the application 
shinyApp(ui, server)
