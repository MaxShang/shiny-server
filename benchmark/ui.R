## app.R ##
library(shinydashboard)
library(shiny)
library(googlesheets)
library(Benchmarking)
library(ggplot2)
library(tidyverse)
library(shinyBS) # for tooltip
ui <- dashboardPage(
  dashboardHeader(title = "Dairy Benchmark"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Farm Info", tabName = "info", icon = icon("dashboard")),
      #menuItem("Raw Data", tabName = "data", icon = icon("th")),
      menuItem("Overall Efficiency", tabName = "overall_eff", icon = icon("th")),
      menuItem("Feed Efficiency", tabName = "feed_eff", icon = icon("th")),
      menuItem("Milk Output", tabName = "milk_output", icon = icon("th")),
      menuItem("Expenses", tabName = "expenses", icon = icon("th")),
      menuItem("Download", tabName = "download", icon = icon("th")),
      menuItem("About us", tabName = "about_us", icon = icon("th")),
      menuItem("Contact us", tabName = "contact_us", icon = icon("th"))
      
    )
  ),
  dashboardBody(
    # tab info--------------------
    tabItems(
      tabItem(tabName = "info",
              fluidPage(
                # basic info----------
                titlePanel("Basic Information"),
                selectInput(inputId = "year",label = "Year",
                            choices = c(2007:2050),selected = 2018),
                
                radioButtons(inputId = "business_type",
                             label = "Farm Business Type",
                             choices= c("Sole Propritor", 
                                        "Corporation","Partnership")),
                
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
                             width = NULL),
                
                # output info --------------------------
                titlePanel("Output/Production Information"),
                
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
                             width = NULL),
                
                # expense info --------------------------
                titlePanel("Expense Information"),
                
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
                
                # numericInput("misc", 
                #              "Miscellaneous expense",
                #              value = 1,
                #              min = 0, 
                #              max = NA, 
                #              step = NA,
                #              width = NULL),
                uiOutput("rendered"),
                #helpText("Includes Hydro, Gasoline, Diesel, Fertilizer, Herbicides, Seeds, and other costs."),
                # submit-------------
                actionButton("submit", "Submit"),
                # note --------------
                helpText("Note:"),
                helpText("*To estimate your family labour expense multiply the number 
                          of family hours worked by the average wage per hour you pay your hired labour. 
                          If you do not have any hired labour then use $20/hour as a default wage rate."),
                helpText("**To estimate homegrown feed expense valued at market prices multiply the quantity of feed produced by its per unit price if you were to buy it from your local distributor.")
              )
      ),
      
      
      # tab data ------------------------
      tabItem(tabName = "data",
              DT::dataTableOutput("responses", width = "auto"), tags$hr()
      ),
      
      # overall_eff------------------------
      tabItem(tabName = "overall_eff",
              #titlePanel("Efficiency Score"),
              plotOutput(outputId = "main_plot", height = "300px"),
              textOutput(outputId = "text_eff_score"),
              hr(),
              #textOutput(outputId = "text_sum"),
              #h4("Summary of all Efficiency Scores"),
              tableOutput(outputId = "eff_score_summary"),
              hr(),
              plotOutput(outputId = "eff_hist")
              
      ),
      # feed_eff------------------------
      tabItem(tabName = "feed_eff",
              #titlePanel("Efficiency Score"),
              plotOutput(outputId = "main_plot.f", height = "300px")
              #textOutput(outputId = "text_eff_score"),
              #hr()
              #h4("Summary of all Efficiency Scores"),
              #tableOutput(outputId = "eff_score_summary"),
              #hr(),
              #plotOutput(outputId = "eff_hist")
              
      ),
      # milk output---------------
      tabItem(tabName = "milk_output",
              tableOutput(outputId="tb_milk_output")
              ),
      # expenses------------------
      tabItem(tabName = "expenses",
              plotOutput(outputId="my.exp.brkd"),
              tableOutput(outputId="my.exp.brkd.tb"),
              plotOutput(outputId="my.exp.brkd.ave"),
              tableOutput(outputId="my.exp.brkd.ave.tb")
              
              
      ),
      
      # download---------------------------
      tabItem(tabName = "download",
              downloadButton("report", "Generate report")
      ),
      # about us---------------------------
      tabItem(tabName = "about_us",
              h3("about us"),
              textOutput(outputId = "about_us_txt")
      ),
      
      # contact us---------------------------
      tabItem(tabName = "contact_us",
              h2("contact us"),
              h4("Email: ghailu@uoguelph.ca"),
              h4("Phone: (519) 824-4120")
      )
    )
  )
)