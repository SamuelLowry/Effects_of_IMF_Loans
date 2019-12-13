library(shinythemes)
library(gt)
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
                            
                            titlePanel("Interactive Comparison: Angola and Namibia"),
                            p("This graphic allows you to parse through all of the World Bank and World Health Organization data for these two countries based upon Angola's Structural Adjustment Program status."),
                            p("Explore Away!"),
                            
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
                                plotOutput("dropdown")))),
                   tabPanel("Background Information",
                            titlePanel("Background Information"),
                            p("Since its conception in 1944, the International Monetary Fund (IMF) has provided structural adjustment loans to developing countries conditional upon their adoption of neoliberal economic policies (Keshavjee, 2014). While such policies aim to stabilize the economies of recipient countries, they also require a decrease in government spending forcing public healthcare programs to contract (Stuckler, 2008). While literature exists on this phenomenon, there is little written on healthcare spending specifically. Thus I performed a multivariate regression on Angola which began receiving IMF loans in late 2009 by comparing it to the neighboring country of Namibia which has never received IMF loans (Press Release, 2009). I aim to analyze and contextualize, the IMF, the effects of its structural adjustment loans, and specifically their effect on Angola all through a biosocial approach in order to best depict their ill effects."),
                            p("The IMF was created in July of 1944 at the United Nations Monetary and Financial Conference held in Bretton Wood, New Hampshire. The conference drew, “together 730 delegates from 44 nations. . . . men of power from government, academia, and large financial institutions” (Keshavjee, 2014, p. 85). The delegates’ goal was to develop an agreement that fostered Europe’s post-WWII recovery. Out of their discussions came the Bretton Woods Agreement which created, “the International Stabilization Fund (which later became the International Monetary Fund, IMF) and the International Bank for Reconstruction and Development (IBRD)” (Keshavjee, 2014, p. 85- 86)."),
                            p("The agreement’s final form was predicated upon ideals of neoliberalism, but at the conferences there was great discussion: “British economist John Maynard Keynes. . . argued that totally free markets were not ideal: sometimes the market failed to properly allocate resources, which could be corrected through appropriate intervention by the state” (Keshavjee, 2014, p. 86). On the other hand, Friedrich von Hayek and Milton Friedman, “called for laissez-faire economics, free trade, and the repeal of restrictive trade laws,” what we now think of as neoliberal economics (Keshavjee, 2014, p. 87). Ultimately, the Hayeks and Friedmans won out, and the IMF was in turn founded upon their laissez-faire beliefs."),
                            p("The International Stabilization Fund became the IMF and gave developing countries loans conditionally upon them implementing neoliberal economic policies which was thought to bring about economic growth, and these practices were only further cemented by the debt crisis in 1982. Chapter four of Reimagining Global Health emphasizes how Mexico defaulting on its loans in 1982 signaled, “systemic debt problems across the developing world” (p. 87). The IMF’s reaction was to double down on their neoliberal mantra of “Stabilize, liberalize, privatize” formalizing the structural adjustment aspect of the loans (p. 87)."),
                            p("While this mantra may help alleviate the possibility of defaulting on loans, it entails cutting government spending on social services, primarily healthcare. More specifically, David Stuckler’s 2008 article entitled “International Monetary Fund Programs and Tuberculosis Outcomes in Post-Communist Countries” found that receiving IMF loans were associated with, “an 8 percent drop in government spending as a percentage of gross domestic product” (Farmer, 2013, p. 89). Adia Benton and colleagues went so far as to include such ideas within their analysis of the 2014 West African ebola outbreak entitled “International Political Economy and the 2014 West African Ebola Outbreak.” They provided it as a background societal force which led to the lack of an adequate response to the 2014 outbreak: “Structural adjustment policies. . . . had enduring negative consequences for health care provision. IMF loan conditionalities restricted government spending on social services, including public health and health care” (Benton, 2015)."),
                            p("Ironically, though, IMF structural adjustment loans do not seem to be effective in their pursuits. Graham Bird of the University of Surrey found in his article “IMF Programs: Do They Work? Can They be Made to Work Better?” that IMF loans, “do not work in the way intended; or more accurately, often do not work,” due to, “high rates of recidivism, low rates of completion, and an insignificant catalytic effect on other capital inflows” (Bird, 2001, p. 1862). Reimagining Global Health suggests that IMF loans have in part caused repeated financial crises in Latin America and Southeast Asia. Specifically, “Some scholars believe that the deregulation of capital markets under structural adjustment helped trigger the East Asian financial crisis of 1998” (Farmer, 2013, p. 92).")),
                   tabPanel("Selection Bias",
                            titlePanel("Selection Bias"),
                            p("Angola is able to act as a case study due to its unique quality of only having started to receive IMF loans in 2009 (Press Release, 2009). The World Bank and World Health Organization’s data of various economic metrics only go back as far as 2000 for many loan-receiving countries making Angola an ideal country to examine, for data exists both prior to and after the loans took effect (Countries, 2019). Nevertheless, comparing pre-loan Angola to post-loan Angola would be flawed due to selection bias comparable to the fundamental problem of causal inference but for an observational study. Thus comparing Angola to Namibia, a similar country which has never received an IMF loan, allows the regression to account for variables such as year to year economic differences and the inherent differences between countries."),
                            p("While it is far-fetched to claim that any two countries are practically the same, the use of a multivariate regression as well as a shared history of colonialism between Angola and Namibia allow for this comparison. Both countries have a violent colonial history. Angola specifically suffered from hundreds of years of slavery. The Portuguese government called Angola its “crown jewel” during this period due to the profitability of such extractive practices (Thornton, 2019). Slavery persisted for much longer in Angola than one might think. While, “the export of slaves was banned in Angola in 1836,” the Portuguese empire did not abolish it until 1875, and it continued, “in thinly disguised forms. . . into the 1960s” (Thornton, 2019). Namibia’s colonial history also included great amounts of violence and disregard for the humanity of the local populations—especially during times of rebellion. Namibia was colonized in the 1880s by Germany, “but from 1904 to 1907 a great war of resistance broke out, nearly expelling the Germans before it was quelled with extreme savagery by tactics including extermination, hangings, and forced detention in concentration camps” (Green, 2019). Namibia only became independent on March 21, 1990 while Angola became independent in 1975."),
                           
                             #I used this website to imput image https://www.r-bloggers.com/building-shiny-app-exercises-part-1/\
                            
                            img(src = "map.png", align = "center", height = 250, width = 250),
                            p("Due to a shared history of colonialism, both countries experience negative effects to this day. The average income for a white person is several times that of a black person in Namibia (Green, 2019), and while Angola experienced an economic surge in the middle of the 20th century most of the wealth went to the Portuguese settlers (Thornton, 2019)."),
                            p("The countries do admittedly differ in two ways: their 21st century political stability and the prevalence of HIV. Namibia has been quite politically and economically stable all throughout the 21st century, yet Angola’s civil war only ended in 2002 giving Namibia an economic head start. While the civil war stunted Angola’s economic growth, it ironically kept the prevalence of HIV much lower than many African nations due to it isolating communities (Thornton, 2019). On the other hand, around 20 percent of Namibian adults had HIV in 2000. Luckily, the inclusion of country as a variable within the regression can partially account for these inherent differences."),
                            p("I ran a multivariate linear regression with the value of various metrics as the left-hand side variable and year, country, and whether or not a structural adjustment loan was in place as the right-hand side variables. After running the regression, the presence of an IMF structural adjustment loan was associated with a significant decrease in revenue from various taxes, government health expenditure per capita, and GDP growth.")),
                   tabPanel("Regression Highlights",
                            titlePanel("Regression Highlights"),
                            p("Use the tabs in order to toggle between three of the regression's findings and analysis of them."),
                           
                            #created a tabset panel within a tabset panel in order to show different highlights
                             tabsetPanel(type = "tabs",
                                        tabPanel("Taxes", 
                                                 plotOutput("tax"), 
                                                 p("Unsurprisingly, the presence of an IMF structural adjustment loan was associated with a significant decrease in revenue from various corporate oriented taxes due to the content of Angola’s structural adjustment agreement including, “measures to safeguard the financial sector” (Press Release, 2009). Such measures would no doubt include tax cuts for profits off of the financial sector in order to encourage economic growth."),
                                                 gt_output("tax_gt"),
                                                p("The bolded coefficient for SAP (meaning structural adjustment program) can be read as the effect of an IMF structural adjustment loan on the percent tax revenue from taxes on income, profits, and capital gains controlling for year and country. Given that Angola was the only country which received such a loan, it can be inferred that Angola experienced around a 16 percent decrease—granted this is still an observational study and not an experiment.")),
                                        tabPanel("Health Expenditure", 
                                                 plotOutput("health"), 
                                                 gt_output("health_gt"),
                                                 p("Even though Angola’s original structural adjustment policy attempted to preserve, “an adequate level of social spending” (Press Release, 2009), we still an association between Angola partaking in an IMF loan and about a 48 dollar decrease in government health expenditure per capita controlling for year and country. This can be explained by what was deemed to be “an adequate level of social spending.” The program allowed for, “ 30% of total central government expenditures [to be] on social issues over the duration of the program” (Press Release, 2009). For comparison, in 2015, Social Security and unemployment benefits made up 33 percent of the US budget alone with Medicare taking up an additional 27 percent (Federal, n.d.). With both of the categories being around 30 percent on their own, it begins to become clear as to how the IMF’s regulations can end up limiting healthcare spending even though it tried to preserve, “an adequate level of social spending.”")),
                                        tabPanel("GDP Growth", 
                                                 plotOutput("gdp"), 
                                                 gt_output("gdp_gt"),
                                                 p("The most ironic of all of the effects of IMF loans on Angola is the negative association between the presence of an IMF loan and GDP growth. Admittedly, while this may appear to showcase how IMF loans halted an economy instead of bolstering it, two historical aspects must be taken into consideration. Firstly, the 2009 recession coincided with the introduction of IMF loans in Angola. Nevertheless, that would not explain why 2015 had less than a third of the growth of 2012 (Countries, 2019). In addition, the civil war did not end until 2002 (Thornton, 2019). The lack of immediate violence allowed for a surge in GDP growth as shown by the difference between the 2001 and 2002 points.")))),
                   tabPanel("References",
                            titlePanel("References"),
                            p("Archived Data (I.D. 1 to 434; 1993-2003). (n.d.). Retrieved from https://www.imf.org/external/np/pdr/mona/HistoricalData.aspx"),
                            p("Benton, A., & Dionne, K. (2015). International Political Economy and the 2014 West African Ebola Outbreak. African Studies Review, 58(1), 223-236."),
                            p("Bird, G. (2001). IMF Programs: Do They Work? Can They be Made to Work Better? World Development, 29(11), 1849-1865."),
                            p("Countries and Economies. (2019). Retrieved from https://data.worldbank.org/country"),
                            p("Edwards, S. (2019, February 04). “As Jim Kim steps down, a tumultuous World Bank presidency comes to an end”. Devex, Retrieved from https://www.devex.com/news/as-jim-kim-steps-down-a-tumultuous-world-bank-presidency-comes-to-an-end-94247"),
                            p("Farmer, P., Kleinman, A., Kim, J., & Basilico, M. (2013). Reimagining Global Health: An Introduction. Berkeley: University of California Press."),
                            p("Farmer, P., Saussy, H., & Kidder, T. (2010). Partner to the Poor : A Paul Farmer Reader.Berkeley: University of California Press."),
                            p("Federal Spending: Where Does the Money Go. (n.d.). Retrieved from https://www.nationalpriorities.org/budget-basics/federal-budget-101/spending/"),
                            p("Green, R. (2019). Namibia. Britannica Online Academic Edition, Encyclopædia Britannica, Inc."),
                            p("Global Health Observatory data repository. (n.d.) Retrieved from http://apps.who.int/gho/data/node.home"),
                            p("Kleinman, A. (2010). Four Social Theories for Global Health. The Lancet, 375(9725), 1518-1519."),
                            p("Keshavjee, S., & Farmer, P. (2014). Blind Spot : How Neoliberalism Infiltrated Global Health (California series in public anthropology ; 30). Oakland, California: University of California Press."),
                            p("Press Release: IMF Executive Board Approves US.4 Billion Stand-By Arrangement with Angola. (2009, November 23). Retrieved from https://www.imf.org/en/News/Articles/2015/09/14/01/49/pr09425"),
                            p("Stuckler, D., King, L., Basu, S., & Murray, M. (2008). International Monetary Fund Program and Tuberculosis Outcomes in Post-Communist Countries (IMF Programs and Tuberculosis Outcomes). PLoS Medicine, 5(7), E143."),
                            p("Thornton, J. (2019). Angola. Britannica Online Academic Edition, Encyclopædia Britannica, Inc."),
                            p("Weber, M. (1930). The protestant ethic and the spirit of capitalism. Unwin University Books."),
                            p("Weber, M. (1946). Max Weber: Essays in Sociology. New York: Oxford University Press.")),
                   tabPanel("About",
                            titlePanel("About"),
                            p("This is Sam Lowry's final project for GOV 1005 at Harvard College which he took in the fall of 2019. He is interested in studying a combination of social and data science."),
                            p("You can contact him at samlowry@college.harvard.edu"),
                            uiOutput("link"),
                            uiOutput("tab"))))
                    


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
           title = "Metrics from World Bank and World Health Organization Data",
           color = "Structural Adjustment Program")
  }
  )
  
#Plots for the regression highlights tab
  
  #tax section
  
  output$tax <- renderPlot({
    
    #plot just like the shiny drop down
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
  
  output$tax_gt <- render_gt({
    
    #Taxes on income, profits and capital gains (% of revenue)	
    reg_tax <- significant_regression[52, 3] %>% 
      unnest()
    
    #I bolded the SAP coefficient 
    gt(reg_tax) %>% 
      tab_header(title = "Taxes on income, profits and capital gains (% of revenue)") %>% 
      cols_label(
        term = " ",
        estimate = "Coefficient",
        std.error = "Standard Error",
        statistic = "Statistic",
        p.value = "P Value") %>% 
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_data(
                  columns = vars(estimate),
                  rows = 3
                )) 
  }
  )
  
  
  #health expenditure section
  
  output$health <- renderPlot({
    
    complete_data %>% 
      filter(Metric == "Domestic general government health expenditure (GGHE-D) per capita in US$ (WHO)") %>% 
      ggplot(aes(x = Year, y = Value, color = SAP)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ Country) +
      labs(title = "Domestic general government health expenditure per capita",
           y = "US$",
           color = "Structural Adjustment Program")
  }
  )
  
  output$health_gt <- render_gt({
    
    #showed an example of a comparison to account for angolan signifcance 
    
    #Domestic general government health expenditure (GGHE-D) per capita in US$ (WHO)	
    reg_gov_health_expend_percap <- significant_regression[18, 3] %>% 
      unnest()
    
    #I bolded the SAP coefficient 
    gt(reg_gov_health_expend_percap) %>% 
      tab_header(title = "Domestic general government health expenditure per capita in US$") %>% 
      cols_label(
        term = " ",
        estimate = "Coefficient",
        std.error = "Standard Error",
        statistic = "Statistic",
        p.value = "P Value") %>% 
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_data(
                  columns = vars(estimate),
                  rows = 3
                ))
  }
  ) 
  
  #GDP section
  
  output$gdp <- renderPlot({
    
    complete_data %>% 
      filter(Metric == "GDP growth (annual %)") %>% 
      ggplot(aes(x = Year, y = Value, color = SAP)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      facet_wrap(~ Country) +
      labs(title = "GDP growth",
           y = "Annual Percent",
           color = "Structural Adjustment Program")
  }
  )
  
  output$gdp_gt <- render_gt({
    
    #showed an example of a comparison to account for angolan signifcance 
    
    #GDP growth (annual %)
    reg_gdp_growth <- significant_regression[22, 3] %>% 
      unnest()
    
    #I bolded the SAP coefficient 
    gt(reg_gdp_growth) %>% 
      tab_header(title = "GDP growth (Annual Percent)") %>% 
      cols_label(
        term = " ",
        estimate = "Coefficient",
        std.error = "Standard Error",
        statistic = "Statistic",
        p.value = "P Value") %>% 
      tab_style(style = cell_text(weight = "bold"),
                locations = cells_data(
                  columns = vars(estimate),
                  rows = 3
                ))
  }
  )
  
  #https://stackoverflow.com/questions/42047422/create-url-hyperlink-in-r-shiny used code from here to make URL link
  
  url <- a("GitHub Repo", href="https://github.com/SamuelLowry/Effects_of_IMF_Loans")
  output$tab <- renderUI({
    tagList("Link to Code:", url)
  })
  
  data_source <- a("World Bank Data", href="https://data.worldbank.org/country")
  output$link <- renderUI({
    tagList("Link to my primary data source:", data_source)
  })
  
}

# Run the application 
shinyApp(ui, server)