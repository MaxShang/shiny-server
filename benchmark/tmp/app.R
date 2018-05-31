library(shiny)
library(shinyBS)
 server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')

  output$rendered <-   renderUI({
    checkboxGroupInput("qualdim",  tags$span("Auswahl der Qualitätsdimension",   
      tipify(bsButton("pB2", "?", style = "inverse", size = "extra-small"),
             "Here, I can place some help")),

                       c("Lernerfolg"             = "Lernerfolg"   , 
                         "Enthusiasmus"           = "Enthusiasmus"          
                         ),
                       selected = c("Lernerfolg"))


  })

  })
}

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100),
      uiOutput("rendered")
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

shinyApp(ui = ui, server = server)