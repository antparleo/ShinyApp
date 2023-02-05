library(tidyverse)
library(GGally)

md <- read.delim('metadata_train.csv', sep = ',', row.names = 1)
diver <- read.delim('diversity_train.csv', sep = ',')


server <- function(input, output, session) {
  
  output$bpType <- renderPlot({
    
    df <- md %>% 
      filter((age <= input$Age | age == 'Unknown') &
               project%in%input$projects & 
               NIH.Racial.Category %in% input$Race &
               trimester == input$trimester)
    
    ggplot(df,
           aes(x = Type, fill = Type)) +
      geom_bar(color = 'black') +
      scale_fill_manual(labels = c('Preterm','Term'),
                        values = c('#CBC3E3','grey'))+
      labs(fill = 'Type', y = 'Frequency', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20))
    
  })
  
  output$PCrace <- renderPlot({
    
    df <- md %>% 
      filter((age <= input$Age | age == 'Unknown') &
               project%in%input$projects & 
               NIH.Racial.Category %in% input$Race &
               trimester == input$trimester)
    
    to_plot <- count(df,NIH.Racial.Category) %>%
      arrange(desc(NIH.Racial.Category)) %>%
      mutate(prop = n / sum(.$n) *100) %>%
      mutate(ypos = cumsum(prop)- 0.5*prop)
    
    ggplot(to_plot,
           aes(x = '', fill = NIH.Racial.Category, y = prop)) +
      geom_bar(stat = 'identity', color = 'black') +
      coord_polar('y', start = 0)+
      labs(fill = '', y = '', x = '')+
      theme_void() + 
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 20),
            legend.position = 'bottom') +
      guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
      geom_text(aes(y = ypos, label = n), color = "white", size=6) 
    
  })
  
  output$bpProject <- renderPlot({
    
    df <- md %>% 
      filter((age <= input$Age | age == 'Unknown') &
               project%in%input$projects & 
               NIH.Racial.Category %in% input$Race &
               trimester == input$trimester)
    
    ggplot(df,
           aes(x = project, fill = project)) +
      geom_bar(color = 'black') +
      scale_fill_brewer(palette = 'Paired') +
      labs(fill = 'Project', y = 'No. Samples', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20))
    
  })
  
  output$cpDiversity <- renderPlot({
    if(input$goButton){
    
    df <- md %>% 
      filter((age <= input$Age | age == 'Unknown') &
               project%in%input$projects & 
               NIH.Racial.Category %in% input$Race &
               trimester == input$trimester)
    
    to_plot <- diver %>% filter(specimen%in%md$specimen)
    
    rownames(to_plot) <- to_plot$specimen
    to_plot <- to_plot[df$specimen,]
    diver_md <- cbind(to_plot,df[-1])
    
    ggpairs(data = diver_md, columns = input$metrics,
            aes(colour = Type),
            upper = list(continuous = wrap("cor", size = 6)))+
      scale_fill_manual(labels = c('Preterm','Term'),
                                                   values = c('#CBC3E3','grey')) +
      scale_colour_manual(labels = c('Preterm','Term'),
                          values = c('#CBC3E3','grey'))+
      theme_bw()+
      theme(strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20))
    }
    
  })
  
  
}
