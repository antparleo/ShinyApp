library(shinydashboard)
library(ggcorrplot)
library(shinyBS)
library(ggalluvial)

ui <- dashboardPage(
  skin = "purple",
  title = "Vaginal ",
  dashboardHeader(
    title = 'VMAP: Vaginal Microbiome Atlas in Pregnancy',
    titleWidth = 500
  ),
  
  dashboardSidebar(
    sidebarMenu(
    
    # Select Sample or Patricipant
      
      selectInput("sample", "Show data by:",
                  choices =
                    c('Sample' = 'specimen',
                      'Individual' = 'participant_id'),
                  selected = 'participant_id'),
      
      checkboxGroupInput("type", "Choose the Outcome Category",
                         choices =
                           c('Term (>= 37 weeks gestational age (GA))' = 'term',
                             'Preterm (< 37 weeks GA)' = 'preterm',
                             'Early Preterm (< 32 weeks GA)' = 'early'),
                         selected = c('term','preterm','early')),
      selectInput("feature", "Select a demogrpahic feature to filter by:",
                  choices =
                    c('Trimester' = 'Trimester',
                      'Race' = 'Race',
                      'Outcome' = 'Type',
                      'Age' = 'Age',
                      'Project' = 'project'),
                  selected = 'Type'),
      
      
    menuItem("Metadata information", icon = icon("th"), tabName = "metadata",
             # Age
             checkboxGroupInput("Age", "Choose the age range:",
                                choiceNames =
                                  list('Below 28', 'Above 29', 'Unknown'),
                                choiceValues =
                                  list("Below 28", "Above 29",'Unknown'),
                                selected = c("Below 28", "Above 29",'Unknown')),
             # Project
             selectizeInput('projects',label = 'Project',
                            choices = c('A: SDY465' = 'A',
                                        'B: PRJEB11895' = 'B',
                                        'C: PRJEB21325' = 'C',
                                        'D: PRJEB30642' = 'D',
                                        'E: PRJNA242473' = 'E',
                                        'F: PRJNA294119' = 'F',
                                        'G: PRJNA393472' ='G',
                                        'H: PRJNA430482' = 'H',
                                        'I: PRJNA504518' = 'I',
                                        'J: PRJEB12577' = 'J',
                                        'S: Validation-Stanford/UCSF' = 'S',
                                        'W: Validation-Wayne State' = 'W'),multiple = TRUE,
                            selected = c('A','B','C','D','E','F','G','H','I','J','S','W')),
             
             # Trimester
             
             selectizeInput('trimester',label = 'Trimester',
                         choices =  c("First" = 1,
                                      "Second" = 2,
                                      "Third" = 3), selected = c(1,2,3),
                         multiple = TRUE),
             
             # Race
             
             selectizeInput('Race',label = 'NIH Racial Category',
                            choices = c('White','Other',
                                        'Black or African American',
                                        'Unknown'),
                            multiple = TRUE, selected = c('White',
                                                          'Other',
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
    
    menuItem("Composition", icon = icon("network-wired", class = NULL,
                                            lib = "font-awesome"),
             tabName = "Taxonomy",

             # Alpha diversity metric
             selectizeInput('Specie',label = 'Select specie',
                            choices = phylo_specie,
                            selected = phylo_specie[1:10],
                            multiple = TRUE)
    ),
    
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
    
    # Legend for Race
    
    box(width = 12 ,
        title = "Legend for Race", solidHeader = TRUE,
        collapsible = TRUE,background = "purple",
        uiOutput('my_race')
    ),
    
    # Legend for project
    
    box(width = 12 ,
        title = "Legend for Project", solidHeader = TRUE,
        collapsible = TRUE,background = "purple",
        tableOutput('studies')
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
    
    # Legend for diversity
    
    box(width = 12 ,
        title = "Legend for Diversity Metrics", solidHeader = TRUE,
        collapsible = TRUE,background = "purple",
        uiOutput('my_text')
    ),
    
    
    # Alluvial Plot CST
    
    box(width = 12,
        title = "VALENCIA Community State Types (CST) by Trimester", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("apCST", height = 500)
    ),
    
    
    
    # Heatmap Phylotypes
    
    box(width = 12,
        title = "Heatmap of Taxonomical Features", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("hmPhylo", height = 800)
    ),
    
    # UMAP Phylotypes
    
    box(width = 12,
        title = "Dimensionality Reduction Plot (UMAP) Based on Phylotypes", solidHeader = TRUE,
        collapsible = TRUE, background = "purple",
        plotOutput("upPhylo", height = 900)
    ),
    
  ))
  
  # End of dashboardpage
  )
