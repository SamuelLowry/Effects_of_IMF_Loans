library(shiny)
library(shinythemes)
library(tidyverse)

# I copied in the needed data

complete_data <- readRDS("complete_data.rds")

complete_data_untrimmed <- readRDS("complete_data_untrimmed.rds")

#I used the navbar template and then defined all the panels

ui <- navbarPage("Effects of IMF Structural Adjustment Programs on Angola",
                 theme = shinytheme("united"),
                 tabsetPanel(
                     tabPanel("About", p("This is the rough draft of Sam Lowry's final project for GOV 1005 at Harvard College. Here is a link to my github repo: https://github.com/SamuelLowry/gov_1005_final_project")),
                     tabPanel("Context", p("Here I will give background as to the scholarly disputes around IMF loans and how they affect the spending of recipient countries. The basic idea is using Namibia as a control in comparison to Angola. Angola received IMF loans starting in 2009. Namibia has never received them.")),
                     tabPanel("Selection Bias",p("Here, there will be graphics and context information around selection bias. I will compare the economies and history of the two countries while also acknowledging the shortcomings of the comparison."), plotOutput("selectionbias")),
    
    
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
                     plotOutput("dropdown")
                 )
             )
    ))
             )

# Define the server

server <- function(input, output) {
    
    #library needed
    
    library(tidyverse)
    

    
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
                 color = "Structural Adjustment Program")
    }
    )
 
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)