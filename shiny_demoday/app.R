library(shinythemes)
library(tidyverse)
library(shiny)

# I copied in the needed data

complete_data <- readRDS("complete_data.rds")

complete_data_binary <- readRDS("complete_data_binary.rds")

significant_regression <- readRDS("significant_regression.rds")


#I used the navbar template and then defined all the panels

ui <- navbarPage(theme = shinytheme("united"), "Effects of IMF Structural Adjustment Programs on Angola",
                 tabsetPanel(
                   
                   #I then created the dropdown menu in the ui
                   
                   tabPanel("Interactive Comparison: Angola and Namibia",
                            
                            # Application title
                            
                            titlePanel("Interactive Comparison: Angola and Namibia"),
                            
                            #Select a single, unique metric by consulting the code found here https://github.com/diegomartinez1221/baseball_aging_curve/blob/master/Baseball_Aging_Curve/app.R
                            
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Metric",label = strong("Metric"),
                                            choices = unique(complete_data$Metric),
                                            selected = "Adjusted savings: education expenditure (% of GNI)",
                                            multiple = FALSE
                                )),
                              
                              # Show the dropdown plot
                              
                              mainPanel(
                                plotOutput("dropdown"),
                                p("This graphic allows you to parse through all of the World Bank and World Health Organization data for these two countries based upon Angola's Structural Adjustment Program status."),
                                p("Explore Away!")))),
                   tabPanel("Context",
                            titlePanel("Context"),
                            p("This project centers around the effects of International Monetary Fund loans in the form of Structural Adjustment Programs on developing countries."),
                            p("The issue at hand is that these loans have existed since the 1950s, and the data are sparse during that period in developing countries. In order to combat these issues, I am using Angola as a case study due to its unique position. It had never received any IMF assistance until late in 2009 (after the composition of the 2009 budget hence beginning the comparison in 2010). Thus, I am able to compare Angola's data before and after the introduction of the SAP."),
                            p("In addition, I am using Namibia as a control. It is a similar country to Angola, but it has never received IMF loans. Thus, I am able to somewhat eliminate selection bias."),
                            
                            #used this website to imput image https://www.r-bloggers.com/building-shiny-app-exercises-part-1/\
                            
                            img(src = "map.png", align = "center", height = 500, width = 500)),
                   tabPanel("Selection Bias",
                            titlePanel("Selection Bias"),
                            plotOutput("selectionbias"),
                            p("I performed an analysis of variance (ANOVA) test on both the Angola and Namibia datasets in order to determine if there was a significant change in the various metrics before and after Angola received the IMF loans. I then looked at all of the metrics with a p-value larger than or equal to 0.05."),
                            p("These values displayed metrics which did not exihibit significant change with the implementation of IMF loans in Angola. Thus it can be presumed that the loans had little to no affect on such metrics showing the semi-similar nature of these countries. Nevertheless, the direct comparison of two countries will never come without its faults"),
                            p("This graphic displays an example of one of those metrics which did not undergo a significant change: GDP Growth.")),
                   tabPanel("Significant Changes in Angola",
                            titlePanel("About"),
                            plotOutput("sig"),
                            p("Again, using the analysis of variance (ANOVA test), I was able to determine which metrics experienced change in Angola but not Namibia after the implementation of IMF loans."),
                            p("This plot specifically shows the drop in the tax rate in Angola due to the IMF loans which only goes to show how the mantra of stabalize, liberalize, privatize, affected Angolan policy.")),
                   tabPanel("About",
                            titlePanel("About"),
                            p("This is the demo day draft of Sam Lowry's final project for GOV 1005 at Harvard College."),
                            p("You can contact him at samlowry@college.harvard.edu"),
                            uiOutput("tab"))
                      ))


# Define the server

server <- function(input, output) {

  #specified reactive drop down based on metric
  
  subset<-reactive({complete_data %>% filter(Metric %in%input$Metric)})
  
  
  output$dropdown <- renderPlot({
    
    # dcreates plot from groups of metrics
    
    ggplot(subset(), aes(x=subset()$Year, y=subset()$Value, color = subset()$SAP)) +
      geom_point() +
      
      #created linear regression 
      
      geom_smooth(method = "lm", se = FALSE) +
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
    
    complete_data %>% 
      filter(Metric == "GDP growth (annual %)") %>% 
      ggplot(aes(x = Year, y = Value, color = SAP)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ Country) +
      labs(title = "GDP Growth",
           y = "Annual Percent",
           color = "Structural Adjustment Program")
  }
  )
  
  output$sig <- renderPlot({
    
    #showed an example of a comparison to account for angolan signifcance 
    
    complete_data %>% 
      filter(Metric == "Taxes on income, profits and capital gains (% of revenue)") %>% 
      ggplot(aes(x = Year, y = Value, color = SAP)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ Country) +
      labs(title = "Taxes on income, profits and capital gains",
           y = "Percent of Revenue",
           color = "Structural Adjustment Program")
  }
  )
  
}

# Run the application 
shinyApp(ui, server)