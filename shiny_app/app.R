#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)



 

shinyApp(ui, server)

#I defined the ui using navbarPage and created the app title, panels, and included the proper information within the panels.
#The plot was stored as plot1 and thus put it within the app as such using imageOutput.

ui <- navbarPage("The Effects of IMF Loans: Angola vs. Namibia",
                 tabPanel("About", p("About Section")),
                 tabPanel("Context", p("Context")),
                 tabPanel("Selection Bias",p("Selection Bias")),
                 tabPanel("Regressions",p("Regressions")),
                 tabPanel("The Countries Compared",
                          fluidPage(
                              titlePanel("The Countries Compared"),
                              
                              # Tab one allows users to compare the crime rates between states and regions on a generated line graph
                              
                              
                              # Sidebar that enables user to select the crime type, areas, years, and regions they'd like to 
                              # see on the line graph. 
                              
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput("Metric", "Metric",
                                                  c("Adjusted savings: education expenditure (% of GNI)" = "Adjusted savings: education expenditure (% of GNI)",
                                                    "Adjusted savings: education expenditure (current US$)" = "Adjusted savings: education expenditure (current US$)", "Adjusted savings: education expenditure (% of GNI)")
                                  
                                  # Main panel shows a plot of the generated line plot
                                  
                                  ))))))
                          
                
server <- function(input, output) {
    
    # Generates the line graph for tab one based on inputs from ui
    
    output$countries_compared <- renderPlot({
        
        # Prepares data table for line graph by filtering out the user-selected areas, crime types and years 
        
            # Draw the line graph with the filtered data set 
            complete_data %>% 
            ggplot(aes(x = Year, y = Value, color = Country)) + 
            geom_point() 
    })
}

shinyApp(ui = ui, server = server())
