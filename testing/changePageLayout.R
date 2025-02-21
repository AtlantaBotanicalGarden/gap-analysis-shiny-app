library(shiny)
library(bslib)

ui <- fluidPage(
  
  theme = bs_theme(version = 5),
  
  sidebarLayout(
    
    sidebarPanel(
      actionButton("sideBySide", "Side-by-Side"),
      actionButton("topBottom", "Top-Bottom")
    ),
    
    mainPanel(
      uiOutput("cardLayout") 
    )
  )
)

server <- function(input, output) {
  
  layoutState <- reactiveVal("sideBySide") # Store layout state ("sideBySide" or "topBottom")
  
  observeEvent(input$sideBySide, {
    layoutState("sideBySide") 
  })
  
  observeEvent(input$topBottom, {
    layoutState("topBottom")
  })
  
  output$cardLayout <- renderUI({
    if (layoutState() == "topBottom") { # Top-bottom layout
      div(
        card(width = 12, title = "Card 1", "Some content..."),
        card(width = 12, title = "Card 2", "More content...")
      )
    } else { # Side-by-side layout
      div(
        class = "d-flex",
        card(width = 6, title = "Card 1", "Some content..."),
        card(width = 6, title = "Card 2", "More content...")
      )
    }
  })
}

shinyApp(ui = ui, server = server)
