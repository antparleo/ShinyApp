# 01/26/2023
# Antonio Parraga Leo
# Shiny app for microbiome project


# Prepare environment -----------------------------------------------------

rm(list = ls())
# setwd('~/Microbiome_PTB/Shiny/')


# Libraries ---------------------------------------------------------------

library(shiny)
library(shinythemes)
library(bslib)
library(DT)
library(plotly)

# UI ----------------------------------------------------------------------

navbarPage(theme = bs_theme(version='5', bootswatch = 'pulse'),
           title = "DREAM Challenge", 
           tabPanel('Abstract'),
           tabPanel("Sample charateristics",
                    sidebarLayout(
                      sidebarPanel(selectInput('trimester',
                                               label = "Trimester",
                                               choices = c('First','Second',
                                                           'Third'))
                                               ),
                      mainPanel(
                        fluidRow(
                          
                          column(width = 4,
                              
                        plotlyOutput('barplottype')),
                        
                          column(width = 8,
                                 plotlyOutput('barplotRace')
                          )),
                        fluidRow(
                          column(width = 6,
                                 plotlyOutput('barplotproject'))
                        )
                        
                      )
                    )),
           tabPanel("UMAP"),
           tabPanel("Modeled Diversity",
                    sidebarLayout(
                      sidebarPanel(selectInput('metric',
                                               label = "Metric",
                                               choices = c('Shannon',
                                                           'Inv Simpson',
                                                           'BWPD',
                                                           'Phylotype entropy'))
                      ),
                      mainPanel(
                        plotOutput('modeldivShannon',width = 900))
                    )
           ),
           tabPanel('Metadata Information',
                    dataTableOutput("table")),
           tabPanel('Significant pylotypes',
                    dataTableOutput("sigphylo"))
)
