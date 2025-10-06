# Load the Shiny and bslib libraries
library(shiny)
library(bslib)

# ---- Landing Page Module ----
landingUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(HTML("
      .hero-section {
        position: relative;
        height: 60vh;
        min-height: 400px;
        display: flex;
        align-items: center;
        justify-content: center;
        color: white;
        text-align: center;
        overflow: hidden;
        clip-path: ellipse(120% 100% at 50% 0%);
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
      .content-section {
        padding: 60px 20px;
        text-align: center;
      }
      .features-container {
        display: flex;
        justify-content: center;
        gap: 20px;
        margin-top: 40px;
        margin-bottom: 50px;
        flex-wrap: wrap;
        align-items: center;
      }
      .feature-box {
        flex-basis: 280px;
        padding: 20px;
        border-radius: 8px;
        box-shadow: 0 4px 12px rgba(0,0,0,0.1);
        transition: transform 0.2s ease-in-out, box-shadow 0.2s ease-in-out;
      }
      .feature-box:hover {
        transform: translateY(-5px);
        box-shadow: 0 8px 20px rgba(0,0,0,0.15);
      }
      .feature-box img {
        width: 100%;
        height: 160px;
        object-fit: cover;
        border-radius: 4px;
        margin-bottom: 15px;
      }
      .feature-arrow {
        font-size: 2.5rem;
        color: #cccccc;
      }
      .nav-buttons .btn {
        margin: 0 10px;
      }
      .top-nav-banner {
        background-color: #f8f9fa;
        padding: 10px 20px;
        border-bottom: 1px solid #dee2e6;
        margin-bottom: 20px;
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
        margin-top: 50px;
      }
      .footer-banner img {
        height: 50px; /* Logo height */
        margin: 0 20px;
        vertical-align: middle;
      }
    ")),
    div(class = "hero-section",
        tags$img(class = "hero-image", src="https://images.unsplash.com/photo-1497990571654-77aa8ec36038?q=80&w=2864&auto=format&fit=crop"),
        div(class = "hero-overlay"),
        div(class = "hero-content",
            h1("Discover the World of Plants"),
            p("Leverage the power of R for interactive data exploration. This Shiny application offers dynamic visualizations and statistical analysis for botanists and data scientists.", style="font-size: 1.2rem;")
        )
    ),
    div(class="container content-section",
        h2("Application Features"),
        p("Select a section below to begin your analysis.", style="font-size: 1.1rem; max-width: 700px; margin: auto;"),
        
        # Feature Boxes
        div(class="features-container",
            div(class="feature-box",
                tags$img(src="https://images.unsplash.com/photo-1521405924733-921b3d80a22b?q=80&w=2940&auto=format&fit=crop"),
                h4("Gather your Data"),
                p("Upload your datasets and prepare them for analysis.")
            ),
            div(class="feature-arrow", HTML("&#8594;")),
            div(class="feature-box",
                tags$img(src="https://images.unsplash.com/photo-1543286386-71314a00d3e6?q=80&w=2940&auto=format&fit=crop"),
                h4("Gap Analysis"),
                p("Utilize powerful tools to identify key insights and gaps.")
            ),
            div(class="feature-arrow", HTML("&#8594;")),
            div(class="feature-box",
                tags$img(src="https://images.unsplash.com/photo-1557804506-669a67965ba0?q=80&w=2874&auto=format&fit=crop"),
                h4("Share Results"),
                p("Easily export and share your findings with colleagues.")
            )
        ),
        
        # Navigation Buttons
        div(class="nav-buttons",
            actionButton(ns("go_da"), "Get Started", class="btn-primary btn-lg"),
            actionButton(ns("go_about"), "Learn More", class="btn-info btn-lg")
        )
    )
  )
}

landingServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Return a list of reactive expressions that fire on button clicks
    return(
      list(
        go_da = reactive(input$go_da),
        go_ga = reactive(input$go_ga),
        go_about = reactive(input$go_about)
      )
    )
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
  title = "Plantify Shiny App",
  theme = bs_theme(version = 4, bootswatch = "litera"),
  
  # A container for all UI
  div(
    # Conditional panel for the landing page (no sidebar)
    conditionalPanel(
      condition = "output.page === 'landing'",
      landingUI("landing_page")
    ),
    
    # Conditional panel for all other pages (with top nav banner and sidebar)
    conditionalPanel(
      condition = "output.page !== 'landing'",
      
      # Top navigation banner
      div(class="top-nav-banner",
          div(class="brand", "Plantify"),
          actionButton("nav_da", "Data Analysis", class="btn-light"),
          actionButton("nav_ga", "Gap Analysis", class="btn-light"),
          actionButton("nav_about", "About", class="btn-light"),
          actionButton("go_home", "Home", class="btn-secondary")
      ),
      
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
  # 1. From Landing Page Buttons
  landing_nav <- landingServer("landing_page")
  
  observeEvent(landing_nav$go_da(), {
    currentPage("data_analysis")
  })
  observeEvent(landing_nav$go_ga(), {
    currentPage("gap_analysis")
  })
  observeEvent(landing_nav$go_about(), {
    currentPage("about")
  })
  
  # 2. From Top Nav Banner Buttons
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
  # A single reactive expression for the dataset, based on the shared name
  sharedDataset <- reactive({
    switch(sharedDatasetName(), "iris" = iris, "mtcars" = mtcars, "trees" = trees)
  })
  
  obsInput_ga <- reactive(input$obs_ga)
  
  # --- Initialize Module Servers ---
  # Pass the single shared dataset reactive to both modules
  dataAnalysisServer("data_analysis_page", dataset = sharedDataset)
  gapAnalysisServer("gap_analysis_page", dataset = sharedDataset, obs = obsInput_ga)
  aboutServer("about_page")
}

# Run the application
shinyApp(ui = ui, server = server)

