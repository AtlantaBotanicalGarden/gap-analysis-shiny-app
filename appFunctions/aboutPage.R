 
aboutPage<-function(){
  ## landing page panel ------------------------------------------------------
  
  fluidPage(
    br(),
  tags$blockquote("The GAMMA tool will allow users to quantify and assess the completeness of recent collections made, as well as enable meta collection communities to assess the current ex situ conservation status and collection gaps across all participating collections.", cite = "Hadley Wickham"),
  br(),
  card(full_screen = TRUE,
       h2("Gap Analysis Method"),
       p("This version of the Gap Analysis conservation assessment evaluates the ex situ conservation status of a taxon, combines these metrics into an integrated assessment, and calculates an indicator metric that can be compared across taxa.",
         "The quantitative and spatial outputs demonstrate the state of conservation by highlighting where gaps in protection exist. The methods are fully described in Carver et al. (2021). Articles by Ramirez-Villegas et al. (2010), Castañeda-Álvarez and Khoury et al. (2016), and Khoury et al. (2019a, b; 2020))"),
       
       h3("Definitions of occurrence data categories"),
       p(strong("Germplasm Records (G)")," : Occurrences in which a living sample (via plant or seed) is present in an ",em("ex situ")," conservation system (i.e., botanical garden, seed bank, genebank, etc.). "),
       p(strong("Reference Records (H)")," : Occurrences that have a supporting herbarium or other reference record."),
       
       
       h3("Definitions of conservation gap analysis scores"),
       p(strong("The Sampling Representativeness Score ") ,em("ex situ"), "(SRS ex) calculates the ratio of germplasm accessions (G) available in ", em("ex situ")," repositories to reference (H) records for each taxon, making use of all compiled records irrespective of whether they include coordinates."),
       p(strong("The Geographic Representativeness Score ") ,em("ex situ"), "(GRS ex situ) uses 50-km-radius buffers created around each G collection coordinate point to estimate geographic areas already well collected within the potential distribution models of each taxon and then calculates the proportion of the potential distribution model covered by these buffers."),
       p(strong("The Ecological Representativeness Score ") ,em("ex situ"), "(ERS ex situ) calculates the proportion of terrestrial ecoregions (25) represented within the G buffered areas out of the total number of ecoregions occupied by the potential distribution model."),
       p(strong("The  Final Conservation Score "), em("ex situ")," (FCS ex situ) was derived by calculating the average of the three ",em("ex situ")," conservation metrics."),
  ),
  fluidRow(
    column(12,
           div(em("For any questions or feedback please reach out to Maintainer of the Website @ there email.com"),
               style="text-align:left"))
  ),
  h2("Project Background"),
  p(style="text-align: center;",
    "This project was led by Alanta Botanical Garden in collaboration with the IMLS GCC Growing
      Metacollections team which includes individuals from ",
    tags$a(href = "https://mortonarb.org/", "The Morton Arboretum", target = "_blank"),
    ", ",
    tags$a(href = "https://sdbg.org/", "San Diego Botanic Garden", target = "_blank"),
    ", ",
    tags$a(href = "https://www.montgomerybotanicalgardens.com/", "Montgomery Botanical Gardens at Oak Park", target = "_blank"),
    ", ",
    tags$a(href = "https://www.smith.edu/", "Smith College", target = "_blank"),
    ", ",
    tags$a(href = "https://www.auburn.edu/", "Auburn University", target = "_blank"),
    ", ",
    tags$a(href = "https://www.lotusland.org/", "Lotus Land", target = "_blank"),
    ".",
    tags$br(),
    tags$strong("This work is made possible by the Institute of Museum and Library Services ",
                tags$a(href = "https://www.imls.gov/", "(MG-252894-OMS-23).", target = "_blank"),
                "." ),
  ),
  br(),
  fluidRow(
    # p("Or a single logo?"),
    column(2),
    column(8,
           div(img(src="Metacollections project logo_color.png", alt="Logo 2", align="center", width="60%"), style="text-align:center")),
    column(2),
  ),
  
  card(full_screen = TRUE,
       h3("Learn More"),
       p("add material"),
       br()
  ),
  hr(),
  h2("Who We Are"),
  p("This website is the result of a collaboration among the following individuals and institutions:")
  )
}
