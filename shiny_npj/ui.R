library(shinydashboard)
library(ggcorrplot)
library(shinyBS)

ui <- dashboardPage(
  skin = "purple",
  title = "Vaginal ",
  dashboardHeader(
    title = 'Vaginal microbiome atlas',
    titleWidth = 300
  ),
  
  dashboardSidebar(
    sidebarMenu(
    
    menuItem("Metadata information", icon = icon("th"), tabName = "metadata",
             # Age
             sliderInput('Age', 'Age',20,100,1, value = 50),
             # Project
             selectizeInput('projects',label = 'Project',
                            choices = c('A','B','C','D','E','F','G','H','I'),multiple = TRUE,
                            selected = c('A','B')),
             
             # Trimester
             
             selectInput('trimester',label = 'Trimester',
                         choices =  c("First" = 1,
                                      "Second" = 2,
                                      "Third" = 3), selected = 2),
             
             # Alpha diversity metric
             
             selectizeInput('Race',label = 'NIH Racial Category',
                            choices = c('White','Asian','American Indian or Alaska Native',
                                        'Black or African American',
                                        'Native Hawaiian or Other Pacific Islander',
                                        'Unknown'),
                            multiple = TRUE, selected = 'White'
             )),
    
    menuItem("Alpha diversity", icon = icon("bacteria", class = NULL,
                                            lib = "font-awesome"),
             tabName = "diversity", 
             
             # Alpha diversity metric
             selectizeInput('metrics',label = 'Alpha diversity metrics',
                            choices = c("Shannon" = 'shannon',
                                        "Inv Simpsons" = 'inv_simpson',
                                        "BWPD" = 'bwpd',
                                        'Phylotype entropy' = 'phylo_entropy',
                                        'Rooted PD' = 'rooted_pd',
                                        'Unrooted PD' = 'unrooted_pd',
                                        'Quadratic' = 'quadratic'),
                            selected = c('shannon','inv_simpson'),
                            multiple = TRUE)
    ),
    
    bsButton(inputId = "goButton", label = "Start Analysis")
    
  
  
  )),
  dashboardBody(
    
    fluidRow(
    
    # Barplot per Type
    
    box(
      width = 3,
      title = "No. Samples per condition", solidHeader = TRUE,
      collapsible = TRUE,background = "purple",
      plotOutput("bpType", height = 500, width = 350)
    ),
    
    # Piechart Racial
    
    box(width = 4,
      title = "Race distribution", solidHeader = TRUE,
      collapsible = TRUE,background = "purple",
      plotOutput("PCrace", height = 500)
    ),
    
    # Barplot per project
    
    box(width = 5,
      title = "No. Samples per project", solidHeader = TRUE,
      collapsible = TRUE, background = "purple",
      plotOutput("bpProject", height = 500)
    ),
    
    
    # PairPlot metrics
    
    box(width = 12,
        title = "Correlation between metrics", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("cpDiversity", height = 800)
    ),
    
    
    # TabBox
    tabBox(
      title = "First tabBox",
      # The id lets us use input$tabset1 on the server to find the current tab
      id = "tabset1", height = "250px",
      tabPanel("Tab1", "First tab content"),
      tabPanel("Tab2", "Tab content 2")
    ),
    
  ))
  
  # End of dashboardpage
  )
