library(shinydashboard)

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
             sliderInput('Age', 'Age',20,100,1, value = 26),
             # Project
             selectizeInput('projects',label = 'Project',
                            choices = c('A','B','C','D'),multiple = TRUE),
             
             # Trimester
             
             selectInput('trimester',label = 'Trimester',
                            choices = c('First','Second','Third')),
             
             # Alpha diversity metric
             
             selectizeInput('Race',label = 'NIH Racial Category',
                            choices = c('White','Asian','Alaska Native',
                                        'African or Black America People'),
                            multiple = TRUE)
             ),
    
    menuItem("Alpha diversity", icon = icon("bacteria", class = NULL,
                                            lib = "font-awesome"),
             tabName = "diversity", 
             
             # Alpha diversity metric
             selectInput('metrics',label = 'Alpha diversity metrics',
                            choices = c('Shannon','Inver Simpsons','BWPD',
                                        'Quadratic'))
    )
    
  
  
  )),
  dashboardBody(
    
    fluidRow(
    
    # Barplot per project
    
    box(
      width = 4,
      title = "Histogram", solidHeader = TRUE,
      collapsible = TRUE,background = "purple",
      plotOutput("plot3", height = 500, width = 500)
    ),
    
    # Piechart Racial
    
    box(
      title = "Histogram", status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("plot1", height = 250)
    ),
    
    box(
      title = "Histogram", status = "primary", solidHeader = TRUE,
      collapsible = TRUE,
      plotOutput("plot2", height = 250)
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
