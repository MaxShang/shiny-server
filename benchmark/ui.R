library(shinydashboard)
ui <- dashboardPage(
  dashboardHeader(title = "Dairy Benchmark"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("",tabName = "welcome"),
      menuItem("Farm Info", tabName = "info", icon = icon("user")),
      #menuItem("Raw Data", tabName = "data", icon = icon("th")),
      menuItem("Overall Efficiency", tabName = "overall_eff", icon = icon("bar-chart-o")),
      menuItem("Feed Efficiency", tabName = "feed_eff", icon = icon("bar-chart-o")),
      menuItem("Milk Output", tabName = "milk_output", icon = icon("table")),
      menuItem("Expenses", tabName = "expenses", icon = icon("pie-chart")),
      menuItem("Download", tabName = "download", icon = icon("download")),
      menuItem("About us", tabName = "about_us", icon = icon("info-circle")),
      menuItem("Contact us", tabName = "contact_us", icon = icon("envelope-square"))
      
    )
  ),
  dashboardBody(
    # tab info--------------------
    tabItems(
     
      tabItem(tabName = "info",
              fluidPage(
                tags$head(
                  tags$style(HTML('#submit{background-color:orange}'))
                ),
                tags$head(tags$style(
                  type="text/css",
                  "#cowbg img {max-width: 100%; width: 100%; height: auto}"
                )),
                
                # basic info----------
                
               
                box(
                  title = "Basic Information",status = "primary", solidHeader = TRUE,
                  selectInput(inputId = "year",label = "Year",
                              choices = c(2007:2050),selected = 2018),
                  
                  radioButtons(inputId = "business_type",
                               label = "Farm Business Type",
                               choices= c("Sole Propritor", 
                                          "Corporation",
                                          "Partnership")),
                  radioButtons(inputId = "edu",
                               label = "Education of Principal Operator",
                               choices= c("Primary School", 
                                          "High School",
                                          "Diploma",
                                          "Community College",
                                          "Bachelor's Degree",
                                          "Post Grad Degree")),
                  
                  radioButtons(inputId = "housing",
                               label = "Housing System",
                               choices= c("Tie Stall", 
                                          "Free Stall",
                                          "Loose Housing")),
                  
                  radioButtons(inputId = "milking_system",
                               label = "Milking System",
                               choices= c("Pipeline", 
                                          "Parlour")),
                  
                  radioButtons(inputId = "feeding_system",
                               label = "Feeding System",
                               choices= c("Manual", 
                                          "Semi-automated",
                                          "Fully automated")),
                  
                  radioButtons(inputId = "robots",
                               label = "Were milking cows milked using robots",
                               choices= c("Yes", 
                                          "No")),
                  
                  radioButtons(inputId = "milking_cows_tmr",
                               label = "Were milking cows fed using Total Mixed Ration (TMR)",
                               choices= c("Yes", 
                                          "No")),
                  
                  radioButtons(inputId = "heifers_tmr",
                               label = "Were heifers fed using Total Mixed Ration (TMR)",
                               choices= c("Yes", 
                                          "No")),
                  
                  radioButtons(inputId = "separated",
                               label = "Were milking cows separated and fed according to production level",
                               choices= c("Yes", 
                                          "No")),
                  
                  radioButtons(inputId = "manure",
                               label = "Manure Handling System",
                               choices= c("Manual", 
                                          "Stable Cleaner",
                                          "Manure Pack",
                                          "Liqud System")),
                  
                  radioButtons(inputId = "breed",
                               label = "Breed of Herd",
                               choices= c("Holstein", 
                                          "Guernsey",
                                          "Jersey",
                                          "Brown Swiss",
                                          "Milking Shorthorn",
                                          "Ayrshire")),
                  
                  radioButtons(inputId = "milk_recording",
                               label = "Enrolled in Milk Recording",
                               choices= c("Yes", 
                                          "No")),
                  
                  numericInput(inputId = "age_calving", value = 1,
                               "Average age of heifers at first calving (months)", 
                               min = 0, max = NA, step = NA,
                               width = NULL),
                  
                  numericInput(inputId = "calving_interval", value = 1,
                               "Average calving interval of herd (months)", 
                               min = 0, max = NA, step = NA,
                               width = NULL),
                  
                  numericInput(inputId = "barn_capacity", value = 1,
                               "Maximum Capacity of Dairy Cow Barn (head)", 
                               min = 0, max = NA, step = NA,
                               width = NULL)
                ),
                
                
                
               
                
                # output info --------------------------
                box(
                  title = "Output Information",status = "primary", solidHeader = TRUE,
                #titlePanel("Output/Production Information"),
                
                numericInput(inputId = "q_milk", value = 1,
                             "Total Quantity of Milk Shipped for Year (litres)", 
                             min = 0, max = NA, step = NA,
                             width = NULL),
                
                numericInput(inputId = "q_butterfat", value = 1,
                             "Total Butterfat Production for Year (kilograms)", 
                             min = 0, max = NA, step = NA,
                             width = NULL),
                
                numericInput(inputId = "n_lactating_cows", value = 1,
                             "Average Number of Lactating Cows for Year (head)", 
                             min = 0, max = NA, step = NA,
                             width = NULL),
                
                numericInput(inputId = "n_dry_cows", value = 1,
                             "Average Number of Dry Cows for Year (head)", 
                             min = 0, max = NA, step = NA,
                             width = NULL),
                
                numericInput(inputId = "n_heifers", value = 1,
                             "Average Number of Heifers for Year (head)", 
                             min = 0, max = NA, step = NA,
                             width = NULL)
                ),
                # expense info --------------------------
                #titlePanel("Expense Information"),
                box(
                  title = "Expense Information",status = "primary", solidHeader = TRUE,
                numericInput(inputId = "purchased_feed", value = 1,
                             "Purchased feed expense", 
                             min = 0, max = NA, step = NA,
                             width = NULL),
                
                numericInput(inputId = "homegrown_feed", 
                             "Homegrown feed expense valued at market value/prices", 
                             value = 1,
                             min = 0, 
                             max = NA, 
                             step = NA,
                             width = NULL),
                
                numericInput(inputId = "capital", 
                             "Capital expense", 
                             value = 1,
                             min = 0, 
                             max = NA, 
                             step = NA,
                             width = NULL),
                
                
                numericInput(inputId = "labour_family", 
                             "Number of family labour hours", 
                             value = 1,
                             min = 0, 
                             max = NA, 
                             step = NA,
                             width = NULL),
                
                numericInput(inputId = "exp_l_family", 
                             "Family labour expense valued at market wage rates", 
                             value = 1,
                             min = 0, 
                             max = NA, 
                             step = NA,
                             width = NULL),
                
                
                numericInput(inputId = "labour_hired", 
                             "Number of hired labour hours", 
                             value = 1,
                             min = 0, 
                             max = NA, 
                             step = NA,
                             width = NULL),
                
                
                
                numericInput("exp_l_hired", 
                             "Hired labour expense", 
                             value = 1,
                             min = 0, 
                             max = NA, 
                             step = NA,
                             width = NULL),
                uiOutput("rendered"),
                
                  helpText("Note:"),
                  helpText("*    To estimate your family labour expense, multiply the number 
                                    of family hours worked by the average wage per hour you pay your hired labour. 
                                    If you do not have any hired labour then use $20/hour as a default wage rate."),
                  helpText("** To estimate homegrown feed expense valued at market prices, multiply the quantity 
                                    of feed produced by its per unit price if you were to buy it from your local distributor.")
                
                ),
                # numericInput("misc", 
                #              "Miscellaneous expense",
                #              value = 1,
                #              min = 0, 
                #              max = NA, 
                #              step = NA,
                #              width = NULL),
                
                #helpText("Includes Hydro, Gasoline, Diesel, Fertilizer, Herbicides, Seeds, and other costs."),
                # submit-------------
                fluidRow(
                  column(6, align="center", offset = 0,
                
                #actionButton("submit", "Submit", width = "200px"),
                actionButton(inputId= "submit",label = "Submit", width = "25%", 
                             style = "color: white; 
                             background-color: #3c8dbc; 
                             position: relative; 
                             left: 2%;
                             height: 50px;
                             text-align:center;
                             border-radius: 6px;
                             border-width: 2px")
               
                  ))
              )
      ),
      #weolcome------
      tabItem(tabName = "welcome",
              
              fluidRow(column(12,align="center",
                              plotOutput("cowbg")))
              ),
      # tab data ------------------------
      tabItem(tabName = "data",
              DT::dataTableOutput("responses", width = "auto"), tags$hr()
      ),
      
      # overall_eff------------------------
      tabItem(tabName = "overall_eff",
              fluidRow(
                column(6, 
                       valueBoxOutput("infobox_overall_es",width = "25%")),
                column(6, 
                       valueBoxOutput("infobox_overall_esf",width = "25%"))

              ),
              
              fluidRow(
              #titlePanel("Efficiency Score"),
              box(  width = 6, 
                    plotOutput("main_plot"), collapsible = TRUE,
                    textOutput(outputId = "text_eff_score"),
                    title = "Plot of Overall Efficiency Scores", status = "primary", solidHeader = TRUE),
              
              #plotOutput(outputId = "main_plot", height = "300px"),
              
             
              box(  width = 6,  
                    plotOutput(outputId = "eff_hist"), 
                    textOutput(outputId = "text_eff_score_dist"),
                    collapsible = TRUE,
                    title = "Distribution of Overall Efficiency Scores", status = "primary", solidHeader = TRUE)
              
              ),
              
              box(  width = NULL, tableOutput(outputId = "eff_score_summary"), collapsible = TRUE,
                    title = "Summary", status = "primary", solidHeader = TRUE)
              
              #textOutput(outputId = "text_sum"),
              #h4("Summary of all Efficiency Scores"),
              #tableOutput(outputId = "eff_score_summary"),
              
             
              
      ),
      # feed_eff------------------------
      tabItem(tabName = "feed_eff",
              
              #titlePanel("Efficiency Score"),
              #plotOutput(outputId = "main_plot.f", height = "300px")
              box(  width = NULL, plotOutput(outputId = "main_plot.f"), collapsible = TRUE,
                    title = "Plot of Feed Efficiency Scores", status = "primary", solidHeader = TRUE)
              #textOutput(outputId = "text_eff_score"),
              #hr()
              #h4("Summary of all Efficiency Scores"),
              #tableOutput(outputId = "eff_score_summary"),
              #hr(),
              #plotOutput(outputId = "eff_hist")
              
      ),
      # milk output---------------
      tabItem(tabName = "milk_output",
              #tableOutput(outputId="tb_milk_output")
              box(  width = NULL, tableOutput(outputId="tb_milk_output"), collapsible = TRUE,
                    title = "Milk Output", status = "primary", solidHeader = TRUE)
              ),
      # expenses------------------
      tabItem(tabName = "expenses",
              fluidRow(
              box(  width = 6, plotOutput(outputId="my.exp.brkd"), collapsible = TRUE,
              title = "Figure: My Expenses", status = "primary", solidHeader = TRUE),
              
              box(  width = 6, plotOutput(outputId="my.exp.brkd.ave"), collapsible = TRUE,
              title = "Figure: Average Operation's Expenses", status = "primary", solidHeader = TRUE)
              ),
              
              fluidRow(
                box(  width = 6,height = 300, tableOutput(outputId="my.exp.brkd.tb"), collapsible = TRUE,
                      title = "Table: My Expenses", status = "primary", solidHeader = TRUE),
                
                box(  width = 6, height = 300, tableOutput(outputId="my.exp.brkd.ave.tb"), collapsible = TRUE,
                      title = "Table: Average Operation's Expenses", status = "primary", solidHeader = TRUE)
              )
      ),
      
      # download---------------------------
      tabItem(tabName = "download",
              box(  width = 12,  
                    
                    downloadButton("report", "Generate PDF Report"),
                    collapsible = FALSE,
                    title = "Download Report",status = "primary",solidHeader = FALSE)
              
      ),
      # about us---------------------------
      tabItem(tabName = "about_us",
              box(  width = 12,  
                    textOutput(outputId = "about_us_txt"),
                    collapsible = FALSE,
                    title = "About Us", status = "primary", solidHeader = FALSE)
              
      ),
      
      # contact us---------------------------
      tabItem(tabName = "contact_us",
              box(  width = 12, 
                    h4("Email: ghailu@uoguelph.ca"),
                    h4("Phone: (519) 824-4120"),
                    collapsible = FALSE,
                    title = "Contact Us", status = "primary", solidHeader = FALSE)
              
      )
    )
  )
)