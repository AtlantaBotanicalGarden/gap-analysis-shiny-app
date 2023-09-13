library(shiny)

ui <- 
  fluidPage(
    selectInput(
      inputId = "xvar",
      label = "Pick a variable for the x-axis:",
      choices = c("wt", "hp"),
      selected = "wt"
    ),
    
    plotOutput(outputId = "myPlot")
  )

server <- 
  function(input, output) {
    output$myPlot <- 
      renderPlot({
        plot(mtcars[, input$xvar], mtcars$mpg)
      })
  }

shinyApp(ui = ui, server = server, options = list(display.mode = "showcase"))