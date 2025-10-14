# Load the Shiny and bslib libraries
library(shiny)
library(bslib)

# ---- Landing Page Module ----
landingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      /* ## MODIFIED: Hero section is now taller and has a wavy bottom clip-path */
      .hero-section {
        position: relative;
        /* Calculate height to fill space between header and footer */
        height: calc(100vh - 125px); 
        min-height: 500px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        text-align: center;
        overflow: hidden;
        /* This creates the curved/wavy bottom effect */
        clip-path: ellipse(80% 40% at 50% 50%);
      }
      .hero-image {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        object-fit: cover;
        z-index: -2;
      }
      .hero-overlay {
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background-color: rgba(0, 0, 0, 0.5);
        z-index: -1;
      }
      .hero-content {
        max-width: 800px;
        padding: 20px;
      }
      
      /* Original CSS for other elements */
      .top-nav-banner {
        background-color: #f8f9fa;
        padding: 10px 20px;
        border-bottom: 1px solid #dee2e6;
        display: flex;
        gap: 10px;
        align-items: center;
      }
      .top-nav-banner .brand {
        font-size: 1.5rem;
        font-weight: bold;
        margin-right: auto;
      }
      .footer-banner {
        background-color: #f8f9fa;
        padding: 20px;
        border-top: 1px solid #dee2e6;
        text-align: center;
      }
      .footer-banner img {
        height: 50px; /* Logo height */
        margin: 0 20px;
        vertical-align: middle;
      }
    ")),
    
    # Only the first hero section remains
    div(class = "hero-section",
        tags$img(class = "hero-image", src="https://plus.unsplash.com/premium_photo-1690031000842-1ac0508f18b7?ixlib=rb-4.1.0&ixid=M3wxMjA3fDB8MHxwaG90by1wYWdlfHx8fGVufDB8fHx8fA%3D%3D&auto=format&fit=crop&q=80&w=1470"),
        div(class = "hero-overlay"),
        div(class = "hero-content",
            h1("GAMMA"),
            p("Observing the meta collection.", style="font-size: 1.2rem;")
        )
    )
    
    ## MODIFICATION: The 'info-section' and 'feature-section' divs have been removed.
  )
}

# No changes below this line
# ... (rest of the code is unchanged) ...

# ---- Landing Page Server (simplified) ----
landingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}


# ---- Data Analysis Module ----
dataAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Exploratory Data Analysis"),
    p("This section is for general data exploration and visualization. The controls in the sidebar will be active for this page."),
    hr(),
    h4("Dataset Summary"),
    verbatimTextOutput(ns("summary")), # To display a summary of the dataset
    h4("Data Plot"),
    plotOutput(ns("distPlot")) # To display a plot
  )
}

dataAnalysisServer <- function(id, dataset) {
  moduleServer(id, function(input, output, session) {
    output$summary <- renderPrint({
      req(dataset())
      summary(dataset())
    })
    output$distPlot <- renderPlot({
      d <- dataset()
      req(d)
      plot(d[,1], d[,2], 
           xlab=colnames(d)[1], 
           ylab=colnames(d)[2],
           main = paste("Plot of", colnames(d)[2], "vs", colnames(d)[1]),
           pch=19,
           col="darkgreen")
    })
  })
}


# ---- Gap Analysis Module ----
gapAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Gap Analysis"),
    p("This section is dedicated to performing gap analysis. You can add specific modules, tables, and charts here related to identifying gaps in data or performance."),
    hr(),
    h4("Dataset Table"),
    tableOutput(ns("view")) # To display a table of the data
  )
}

gapAnalysisServer <- function(id, dataset, obs) {
  moduleServer(id, function(input, output, session) {
    output$view <- renderTable({
      req(dataset(), obs())
      head(dataset(), n = obs())
    })
  })
}


# ---- About Page Module ----
aboutUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("About This Application"),
    p("This Shiny application was created to demonstrate a multi-page layout using a combination of full-width and sidebar layouts with modules. It serves as a template for building more complex data science applications in R."),
    br(),
    h4("Technologies Used:"),
    tags$ul(
      tags$li("R"),
      tags$li("Shiny (with Modules)"),
      tags$li("bslib for theming")
    ),
    br(),
    p("Feel free to replace this content with information about your specific project, data sources, and contact information.")
  )
}

aboutServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # No server logic needed
  })
}


# ---- Main App UI ----
ui <- fluidPage(
  title = "GAMMA Shiny App",
  theme = bs_theme(version = 4, bootswatch = "litera"),
  
  # Top navigation banner is ALWAYS visible
  div(class="top-nav-banner",
      div(class="brand", "GAMMA"),
      actionButton("nav_da", "Data Analysis", class="btn-light"),
      actionButton("nav_ga", "Gap Analysis", class="btn-light"),
      actionButton("nav_about", "About", class="btn-light"),
      conditionalPanel(
        condition = "output.page !== 'landing'",
        actionButton("go_home", "Home", class="btn-secondary")
      )
  ),
  
  # A container for the main page content
  div(
    # Conditional panel for the landing page (no sidebar)
    conditionalPanel(
      condition = "output.page === 'landing'",
      landingUI("landing_page")
    ),
    
    # Conditional panel for all other pages (with sidebar layout)
    conditionalPanel(
      condition = "output.page !== 'landing'",
      sidebarLayout(
        sidebarPanel(
          # Conditional controls now depend on output.page
          conditionalPanel(
            condition = "output.page === 'data_analysis'",
            h4("Data Analysis Controls"),
            selectInput("dataset_da", "Choose a dataset:",
                        choices = c("iris", "mtcars", "trees"))
          ),
          conditionalPanel(
            condition = "output.page === 'gap_analysis'",
            h4("Gap Analysis Controls"),
            selectInput("dataset_ga", "Choose a dataset:",
                        choices = c("iris", "mtcars", "trees")),
            numericInput("obs_ga", "Number of observations to view:", 10)
          ),
          conditionalPanel(
            condition = "output.page === 'about'",
            p("Navigate using the banner above.")
          )
        ),
        mainPanel(
          # Main content panels are also conditional on the current page
          conditionalPanel(
            condition = "output.page === 'data_analysis'",
            dataAnalysisUI("data_analysis_page")
          ),
          conditionalPanel(
            condition = "output.page === 'gap_analysis'",
            gapAnalysisUI("gap_analysis_page")
          ),
          conditionalPanel(
            condition = "output.page === 'about'",
            aboutUI("about_page")
          )
        )
      )
    )
  ),
  
  # Footer Banner
  div(class="footer-banner",
      tags$a(href="https://www.bgci.org/", target="_blank",
             tags$img(src="https://upload.wikimedia.org/wikipedia/en/thumb/a/a2/Botanic_Gardens_Conservation_International_logo.svg/320px-Botanic_Gardens_Conservation_International_logo.svg.png", 
                      alt="Botanic Gardens Conservation International Logo")
      ),
      tags$a(href="https://www.usbg.gov/", target="_blank",
             tags$img(src="https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/US_Botanic_Garden_logo.svg/320px-US_Botanic_Garden_logo.svg.png", 
                      alt="US Botanic Garden Logo")
      )
  )
)

# ---- Main App Server ----
server <- function(input, output, session) {
  
  # --- Page Navigation State ---
  currentPage <- reactiveVal("landing")
  
  # Expose page state to the UI for conditionalPanels
  output$page <- reactive(currentPage())
  outputOptions(output, "page", suspendWhenHidden = FALSE)
  
  # --- Navigation Event Handling ---
  landingServer("landing_page") # Call the module server
  
  observeEvent(input$go_home, {
    currentPage("landing")
  })
  observeEvent(input$nav_da, {
    currentPage("data_analysis")
  })
  observeEvent(input$nav_ga, {
    currentPage("gap_analysis")
  })
  observeEvent(input$nav_about, {
    currentPage("about")
  })
  
  # --- Shared State for Dataset Selection ---
  sharedDatasetName <- reactiveVal("iris") # Default dataset
  
  # Observer to update shared state from Data Analysis page
  observeEvent(input$dataset_da, {
    sharedDatasetName(input$dataset_da)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Observer to update shared state from Gap Analysis page
  observeEvent(input$dataset_ga, {
    sharedDatasetName(input$dataset_ga)
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Observer to keep the two selectInput controls in sync
  observe({
    current_dataset <- sharedDatasetName()
    updateSelectInput(session, "dataset_da", selected = current_dataset)
    updateSelectInput(session, "dataset_ga", selected = current_dataset)
  })
  
  # --- Reactive Inputs for Modules ---
  sharedDataset <- reactive({
    switch(sharedDatasetName(), "iris" = iris, "mtcars" = mtcars, "trees" = trees)
  })
  
  obsInput_ga <- reactive(input$obs_ga)
  
  # --- Initialize Module Servers ---
  dataAnalysisServer("data_analysis_page", dataset = sharedDataset)
  gapAnalysisServer("gap_analysis_page", dataset = sharedDataset, obs = obsInput_ga)
  aboutServer("about_page")
}

# Run the application
shinyApp(ui = ui, server = server)