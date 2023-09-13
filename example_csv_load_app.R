library(shiny)
library(DT)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(selectInput("input_type","Select Month:", c("Oct 2021", "Nov 2021")),
                 actionButton('load_csv', 'Load csv')),
    mainPanel(dataTableOutput('dt'))
  )
)

server <- function(input, output, session) {
  
  data <- eventReactive(input$load_csv, {
    read.csv(paste0('~/Github/gap-analysis-shiny-app/', input$input_type, '.csv'))
  })
  
  output$dt <- renderDataTable({
    req(data())
    datatable(
      data()
    )}
  )
}

shinyApp(ui, server)
