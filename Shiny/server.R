# 01/26/2023
# Antonio Parraga Leo
# Shiny app for microbiome project


# Prepare environment -----------------------------------------------------

library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(lme4)
library(stringr)
rm(list = ls())


# Load data ---------------------------------------------------------------

md <- read.csv('metadata_train.csv', header = T, row.names = 1)
sigphylo <- read.csv('results_ptb_lmm.csv', header = T, row.names = 1)
load('to_model_diversity.RData')
# df, fit

server <- function(input, output) {
  
  output$barplottype <- renderPlotly({
    
    if (input$trimester == 'First'){
      
      df <- md %>% filter(collect_wk <= 13)
      
    } else if (input$trimester == 'Second'){
      
      df <- md %>% filter(collect_wk > 13 & collect_wk < 26)
      
    } else {df <- md %>% filter(collect_wk >=26)}
    
    ggplot(df,
           aes(x = Type, fill = Type)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),
               color = 'black') +
      scale_fill_manual(labels = c('Preterm','Term'),
                        values = c('#CBC3E3','grey'))+
      labs(fill = 'Type', y = 'Frequency', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20))
    
    
  })

  
  output$barplotRace <- renderPlotly({
    
    if (input$trimester == 'First'){
      
      df <- md %>% filter(collect_wk <= 13)
      
    } else if (input$trimester == 'Second'){
      
      df <- md %>% filter(collect_wk > 13 & collect_wk < 26)
      
    } else {df <- md %>% filter(collect_wk >=26)}
    
    ggplot(df,
           aes(x = NIH.Racial.Category, fill = NIH.Racial.Category)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),
               color = 'black') +
      scale_fill_brewer(palette = 'Set2')+
      labs(fill = 'Racial Category', y = 'Frequency', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
    
  })

  
  output$barplotproject <- renderPlotly({
    
    if (input$trimester == 'First'){
      
      df <- md %>% filter(collect_wk <= 13)
      
    } else if (input$trimester == 'Second'){
      
      df <- md %>% filter(collect_wk > 13 & collect_wk < 26)
      
    } else {df <- md %>% filter(collect_wk >=26)}
    
    ggplot(df,
           aes(x = project, fill = project)) +
      geom_bar(aes(y = (..count..)/sum(..count..)),
               color = 'black') +
      scale_fill_brewer(palette = 'Paired')+
      labs(fill = 'Project', y = 'Frequency', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank())
    
  })
  
  output$modeldivShannon <- renderPlot({
    
    diver_meta_l$metrics <- str_replace_all(diver_meta_l$metrics, c('shannon' = 'Shannon',
                                        'inv_simpson'='Inv Simpson' ,
                                        'bwpd' = 'BWPD',
                                        'phylo_entropy' = 'Phylotype entropy'))
    df <- diver_meta_l[diver_meta_l$metrics == input$metric,]
    
    rownames(df) <- df$specimen
    
    # To shiny
    
    fit <- lmer(alpha_diversity ~ trimester + Type + NIH.Racial.Category + (1|participant_id),
                data = df)
    
    ggplot(df, aes(x = collect_wk, y = predict(fit), color = Type)) +
      scale_colour_manual(values = c("#CBC3E3", "grey"))  +
      geom_smooth(method = loess,
                  aes(group = Type),
                  alpha = 0.1,
                  fill = 'lightblue') +
      theme_classic(base_size = 22) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual(values = c("#CBC3E3", "grey")) +
      ylab('Modeled diversity')
    
  })
  
  output$table <- renderDataTable(datatable({
    md
  }))
  
  output$sigphylo <- renderDataTable(datatable({
      sigphylo %>%
      mutate_if(is.numeric, ~round(., 3))
  }))
  
  
}


