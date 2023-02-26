library(shinydashboard)
library(ggcorrplot)
library(shinyBS)
library(ggalluvial)

ui <- dashboardPage(
  skin = "purple",
  title = "Vaginal ",
  dashboardHeader(
    title = 'VMAP: Vaginal Microbiome Atlas in Pregnancy',
    titleWidth = 500,
    
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
    
    # Select Sample or Patricipant
      
      selectInput("sample", "Show data by:",
                  choices =
                    c('Sample' = 'specimen',
                      'Individual' = 'participant_id'),
                  selected = 'specimen'),
      
      checkboxGroupInput("type", "Choose the Outcome Category",
                         choices =
                           c('Term' = 'term',
                             'Preterm' = 'preterm',
                             'Early Preterm' = 'early'),
                         selected = c('term','preterm','early')),
      selectInput("feature", "Select a demogrpahic feature to filter by:",
                  choices =
                    c('Trimester' = 'Trimester',
                      'Race' = 'Race',
                      'Type' = 'Type',
                      'Age' = 'Age',
                      'Project' = 'project'),
                  selected = 'Age'),
      
      
    menuItem("Metadata information", icon = icon("th"), tabName = "metadata",
             # Age
             checkboxGroupInput("Age", "Choose the age range:",
                                choiceNames =
                                  list('Below 28', '29 to 38','Above 38', 'Unknown'),
                                choiceValues =
                                  list("Below 28", "29 to 38", "Above 38",'Unknown'),
                                selected = c("Below 28", "29 to 38", "Above 38",'Unknown')),
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
                            choices = c('White','Asian','Others',
                                        'Black or African American',
                                        'Unknown'),
                            multiple = TRUE, selected = c('White','Asian',
                                                          'Others',
                                                          'Black or African American',
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
                            selected = c('shannon','inv_simpson','bwpd',
                                         'phylo_entropy','rooted_pd',
                                         'unrooted_pd','quadratic'),
                            multiple = TRUE)
             # actionButton(inputId = "goButton", label = "Start Analysis")
    ),
    
    # menuItem("Phylotypes", icon = icon("network-wired", class = NULL,
    #                                         lib = "font-awesome"),
    #          tabName = "diversity", 
    #          
    #          # Alpha diversity metric
    #          radioButtons('division',label = 'Split by',
    #                         choices = c("Projects" = 'project',
    #                                     "Type" = 'Type'),
    #                         selected = c('project'))
    #          # actionButton(inputId = "startPhylo", label = "Update UMAP")
    # ),
    
    submitButton(text = "Update plots", icon = icon("play", class = NULL,
                                                    lib = "font-awesome"),
                 width = 230)
  
  )),
  dashboardBody(
    
    fluidRow(
    
    # Barplot per Type
    
    box(
      width = 3,
      title = "% of Samples by outcome", solidHeader = TRUE,
      collapsible = TRUE,background = "purple",
      plotOutput("bpType", height = 500, width = 350)
    ),
    
    # Piechart Racial
    
    box(width = 4 ,
      title = "% of Samples by Race", solidHeader = TRUE,
      collapsible = TRUE,background = "purple",
      plotOutput("PCrace", height = 500)
    ),
    
    # Barplot per project
    
    box(width = 5,
      title = "% of Samples by Project", solidHeader = TRUE,
      collapsible = TRUE, background = "purple",
      plotOutput("bpProject", height = 500)
    ),
    
    
    # PairPlot metrics
    
    box(width = 12,
        title = "Diversity Measures: Correlation Between Measures", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("cpDiversity", height = 800)
    ),
    
    # Violin Plot diversity
    
    box(width = 12,
        title = "Diversity Measures: Box Plot Stratified Visualization", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("vpDiversity", height = 400)
    ),
    
    # Alluvial Plot CST
    
    box(width = 12,
        title = "VALENCIA Community State Types (CST) by Trimester", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("apCST", height = 500)
    ),
    
    # UMAP Phylotypes
    
    box(width = 6,
        title = "Dimensionality Reduction Plot (UMAP) Based on Phylotypes", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("upPhylo", height = 500)
    ),
    
    
    # TabBox
    # tabBox(
    #   title = "First tabBox",
    #   # The id lets us use input$tabset1 on the server to find the current tab
    #   id = "tabset1", height = "800px",
    #   tabPanel("Tab1", "First tab content",
    # ),
    #   tabPanel("Tab2", "Tab content 2")
    # )
    
  ))
  
  # End of dashboardpage
  )
