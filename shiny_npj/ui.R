library(shinydashboard)
library(ggcorrplot)
library(shinyBS)
library(ggalluvial)

ui <- dashboardPage(
  skin = "purple",
  title = "Vaginal ",
  dashboardHeader(
    title = 'Vaginal microbiome atlas',
    titleWidth = 300,
    
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = tags$div("There are 1,702 samples with",
                                   tags$br(),
                                   "unknown age which will be included.",
                                   style = "display: inline-block; vertical-align: middle;"),
                   icon("circle-info")
                 ))
  ),
  
  dashboardSidebar(
    sidebarMenu(
    
    # Select Type
      
      selectInput("type", "Choose the category",
                         choices =
                           c('Term' = 'term',
                             'Preterm' = 'preterm',
                             'Early Preterm' = 'early'),
                         selected = 'term'),
      
      
    menuItem("Metadata information", icon = icon("th"), tabName = "metadata",
             # Age
             checkboxGroupInput("Age", "Choose the age range:",
                                choiceNames =
                                  list('Below 18','18 to 28', '29 to 38','Above 38', 'Unknown'),
                                choiceValues =
                                  list("Below 18", "18 to 28", "29 to 38", "Above 38",'Unknown'),
                                selected = c("Below 18", "18 to 28", "29 to 38", "Above 38",'Unknown')),
             # Project
             selectizeInput('projects',label = 'Project',
                            choices = c('A','B','C','D','E','F','G','H','I','S','W'),multiple = TRUE,
                            selected = c('A','B','C','D','E','F','G','H','I','S','W')),
             
             # Trimester
             
             selectizeInput('trimester',label = 'Trimester',
                         choices =  c("First" = 1,
                                      "Second" = 2,
                                      "Third" = 3), selected = c(1,2,3),
                         multiple = TRUE),
             
             # Race
             
             selectizeInput('Race',label = 'NIH Racial Category',
                            choices = c('White','Asian','American Indian or Alaska Native',
                                        'Black or African American',
                                        'Native Hawaiian or Other Pacific Islander',
                                        'Unknown'),
                            multiple = TRUE, selected = c('White','Asia',
                                                          'American Indian or Alaska Native',
                                                          'Black or African American',
                                                          'Native Hawaiian or Other Pacific Islander',
                                                          'Unknown')
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
                            selected = c("Shannon" = 'shannon',
                                         "Inv Simpsons" = 'inv_simpson',
                                         "BWPD" = 'bwpd',
                                         'Phylotype entropy' = 'phylo_entropy',
                                         'Rooted PD' = 'rooted_pd',
                                         'Unrooted PD' = 'unrooted_pd',
                                         'Quadratic' = 'quadratic'),
                            multiple = TRUE)
             # actionButton(inputId = "goButton", label = "Start Analysis")
    ),
    
    menuItem("Phylotypes", icon = icon("network-wired", class = NULL,
                                            lib = "font-awesome"),
             tabName = "diversity", 
             
             # Alpha diversity metric
             radioButtons('division',label = 'Split by',
                            choices = c("Projects" = 'project',
                                        "Type" = 'Type'),
                            selected = c('project'))
             # actionButton(inputId = "startPhylo", label = "Update UMAP")
    ),
    
    actionButton(inputId = "start", label = "Update plots")
  
  )),
  dashboardBody(
    
    fluidRow(
    
    # Barplot per Type
    
    box(
      width = 3,
      title = "No. samples per condition", solidHeader = TRUE,
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
      title = "No. samples per project", solidHeader = TRUE,
      collapsible = TRUE, background = "purple",
      plotOutput("bpProject", height = 500)
    ),
    
    
    # PairPlot metrics
    
    box(width = 12,
        title = "Correlation between metrics", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("cpDiversity", height = 800)
    ),
    
    # Alluvial Plot CST
    
    box(width = 6,
        title = "CST by trimester", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("apCST", height = 500)
    ),
    
    # UMAP Phylotypes
    
    box(width = 6,
        title = "UMAP phylotypes", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("upPhylo", height = 500)
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
