# Server

server <- function(input, output, session) {

  
  metadataType <- reactive({
    
    metadata <- metadataSelection(metadata,input$sample)
    
  }) %>%
    bindCache(input$sample)
  
  
  
  metadataInput <- reactive({
    
    df <- metadataType() %>%
      filter((Age %in% input$Age) &
               project%in%input$projects &
               Type%in%input$type &
               Race %in% input$Race &
               Trimester %in% input$trimester)

  })


  cstInput <- reactive({
    
   cst_alluvia2 <- cst_alluvial %>%
     filter((Age %in% input$Age) &
              project%in%input$projects &
              Type%in%input$type &
              Race %in% input$Race &
              Trimester %in% input$trimester)

  })
  
  
  # Barplot Outcome
  
  output$bpType <- renderPlot({
    
    df <- metadataInput()
    plot <- data.frame(table(df[,c('Type',input$feature)]))
    
    ggplot(plot,
                   aes(x = Type, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'fill', stat = 'identity', color = 'black') +
              scale_fill_manual(labels=c("term" = "Term",
                                         "preterm" = "Preterm",
                                         "early" = "Early Preterm",
                                         "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                         "Asian" = "Asian",
                                         "Black or African American" = "Black or African\nAmerican",
                                         "Others" = "Others",
                                         "Unknown" = "Unknown",
                                         "White" = "White"
              ),values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'Frequency', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)) +
              scale_x_discrete(labels=c("term" = "Term",
                                        "preterm" = "Preterm",
                                        "early" = "Early Preterm",
                                        "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                        "Asian" = "Asian",
                                        "Black or African American" = "Black or African\nAmerican",
                                        "Others" = "Others",
                                        "Unknown" = "Unknown",
                                        "White" = "White"
              ))
    
  })
  
  # Barplot Race
  
  output$PCrace <- renderPlot({
    
    df <- metadataInput()
    plot <- data.frame(table(df[,c('Race',input$feature)]))
    
    ggplot(plot,
                   aes(x = Race, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'fill', stat = 'identity', color = 'black') +
              scale_fill_manual(labels=c("term" = "Term",
                                         "preterm" = "Preterm",
                                         "early" = "Early Preterm",
                                         "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                         "Asian" = "Asian",
                                         "Black or African American" = "Black or African\nAmerican",
                                         "Others" = "Others",
                                         "Unknown" = "Unknown",
                                         "White" = "White"),
                                         values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'Frequency', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)) +
              scale_x_discrete(labels=c("American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                        "Asian" = "Asian",
                                        "Black or African American" = "Black or African\nAmerican",
                                        "Others" = "Others",
                                        "Unknown" = "Unknown",
                                        "White" = "White"
                                        ))
  })
  
  # Barplot Project
  
  output$bpProject <- renderPlot({
  
    df <- metadataInput()
    plot <- data.frame(table(df[,c('project',input$feature)]))
    
    ggplot(plot,
           aes(x = project, fill = plot[,input$feature], y = Freq)) +
      geom_bar(position = 'fill', stat = 'identity', color = 'black') +
      scale_fill_manual(labels=c("term" = "Term",
                                 "preterm" = "Preterm",
                                 "early" = "Early Preterm",
                                 "American Indian or Alaska Native" = "American Indian\nor Alaska Native",
                                 "Asian" = "Asian",
                                 "Black or African American" = "Black or African\nAmerican",
                                 "Others" = "Others",
                                 "Unknown" = "Unknown",
                                 "White" = "White"),
                                 values = my_colors[[input$feature]]) +
      labs(fill = input$feature, y = 'Frequency', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20))
    
  })
  
  
  diversityType <- reactive({
    
    df <- diversitySelection(diversity_all,input$sample)
    
  })
  
  output$my_race <- renderUI(HTML(markdown(
    
    
    ' #### Race categories
    
      Other category contains:
      - American Indian or Alaska Native
      - Native Hawaiian or Other Pacific Islander
      - Asian
      
    '
    
  )))
  
  output$studies <- renderTable(studies)
  
  
  # Pairplot diversity
  
 
  output$cpDiversity <- renderPlot({
    
   to_plot <- merge(diversityType(),
                             metadataInput(),
                             by=input$sample)

    ggpairs(data = to_plot, columns = input$metrics,
            aes(color = to_plot[,input$feature]),
            upper = list(continuous = wrap("cor", size = 6)))+
       scale_fill_manual(values = my_colors[[input$feature]]) +
      scale_color_manual(values = my_colors[[input$feature]]) +
      theme_bw()+
      theme(strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            legend.position = 'bottom')
    


  })
  
  # Violin Plot diversity
  
  output$vpDiversity <- renderPlot({
    
    md <- metadataInput()

    diversity_all_long <- melt(diversityType(), 
                              id.vars = input$sample,
                              variable.name = 'Metrics',
                              value.name = 'Score')
   
    
    diver_md <- merge(x = md, y = diversity_all_long,
                      by = input$sample)
    
    diver_md <- diver_md[diver_md$Metrics%in%input$metrics,]

    list_plot <- lapply(unique(diver_md[,'Metrics']), function(my_metric){
      to_plot <- diver_md[diver_md$Metrics == my_metric,]
      ggplot(data = to_plot,
             aes(x = Metrics, y = Score,
                 fill = to_plot[,input$feature]))+
        geom_violin()+
        scale_fill_manual(values = my_colors[[input$feature]]) +
        labs(fill = input$feature, y = 'Score', x = '')+
        theme_bw() +
        theme(axis.text = element_text(size = 25),
              axis.title = element_text(size = 25),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 20))

    })

    ggarrange(plotlist = list_plot, ncol=length(list_plot),
              common.legend = TRUE, legend="right")

  })
  
  
  # Legend text
  
  output$my_text <- renderUI(HTML(markdown(
    
    
    ' #### Diversity Metrics
    
      - Balance weighted phylogenetic diversity (bwpd)
      - Inverse Simpson (inv_simpson)
      - Phylogenetic entropy (phylo_entropy)
      - Quadratic (quadratic)
      - Rooted phylogenetic diversity (rooted_pd)
      - Shannon (shannon)
      - Unrooted phylogenetic diversity (unrooted_pd)'
    
  )))


  # CST alluvial plot

  output$apCST <- renderPlot({

   cst_alluvia2 <- cstInput()
   values <- unique(cst_alluvia2[,input$feature])

    list_df <- lapply(values, function(x){

      df <- cst_alluvia2[cst_alluvia2[,input$feature] == x,]

      to_plot <- df[,c("Trimester","participant_id","CST")]
      df_freq <- to_plot %>% group_by(participant_id,Trimester,CST) %>% summarise(Freq = n())
      df_freq <- df_freq %>%
        group_by(Trimester) %>%
        mutate(pct = Freq / sum(Freq)*100)


      p <- ggplot(df_freq,
                  aes(x = Trimester, stratum = CST, alluvium = participant_id,
                      fill = CST, label = CST, y = pct)) +
        scale_fill_manual(values = my_colors[['CST']], limits = names(my_colors[['CST']])) +
        geom_flow(stat = "alluvium", lode.guidance = "frontback") +
        geom_stratum() +
        scale_x_discrete(breaks=c("1","2","3")) +
        ggtitle(x)+
        theme_bw()+
        ylab("Freq")+
        theme(legend.position = "none", text = element_text(size = 20),
              plot.title = element_text(hjust = 0.5))
      return(p)

    })


    ggarrange(plotlist = list_df, ncol=length(list_df),
              common.legend = TRUE, legend="right")

  })

  
  umapInput <-  reactive({
    
    umap2plot(metadataInput(),umap_dfs[[input$sample]], input$sample)

  }) %>%
    bindCache(metadataInput(), input$sample)
  
  
  # UMAP phylotype plot

  output$upPhylo <- renderPlot({

    toplot <- umapInput()
    ggplot(toplot, aes(x = UMAP1,
                                y = UMAP2,
                       color = toplot[,input$feature]))+
              geom_point(size = 4) +
              scale_color_manual(values = my_colors[[input$feature]])+
              labs(x = "UMAP1",
                   y = "UMAP2",
                   color = "")+
              theme_bw() +
      theme(legend.position = "bottom",text = element_text(size = 20),
            legend.text = element_text(size=15)) +
      guides(fill=guide_legend(nrow=3,byrow=TRUE))


  })
  
  
  phyloType <- reactive({
    phyloSelection(heatmap_dfs,input$sample)
  }) %>% bindCache(input$sample)


  # Heatmap phylotype plot

  output$hmPhylo <- renderPlot({

    metadata <- metadataInput()

    list_plots <- lapply(sort(input$trimester), function(my_tri){

      metadata_trimester <- metadata[metadata$Trimester == my_tri,]
      phylo_trimester <- merge(phyloType(), metadata_trimester[,c(input$sample,input$feature)],
                               by = input$sample)

      test <- do.call('cbind',lapply(sort(unique(metadata_trimester[,input$feature])), function(my_feat){

        # print(my_feat)

        phylo_feat <- phylo_trimester[phylo_trimester[,input$feature] == my_feat,
                                      c(phylo_specie,input$sample)] %>%
          remove_rownames %>% column_to_rownames(var = input$sample)

        if(nrow(phylo_feat) > 0 ){

          counts <- as.data.frame(apply(phylo_feat>0,2,sum) / nrow(phylo_feat))
          colnames(counts) <- my_feat
        } else{

          warning(paste0('No information for', my_feat, collapse = ' '))

        }

        return(counts)

      }))

      if(ncol(test) == 1) {
        
        id <- colnames(test)
        test$ID <- rownames(test)
        test <- test[input$Specie,]
        test_sorted <- test[order(test[,id], decreasing = F),]
        
      } else {
        test_sorted <- test[names(sort(apply(test[input$Specie,],1,mean), decreasing = F)),]
        test_sorted$ID <- rownames(test_sorted)
      }

      to_plot <- melt(test_sorted, id.vars = 'ID') %>% rename(!!input$feature := variable, Counts = value) %>%
        arrange(Counts)
      to_plot$Counts <- round(to_plot$Counts*100,0)
      to_plot$ID <- factor(to_plot$ID,levels = rownames(test_sorted))

      plot <- ggplot(to_plot, aes(x = to_plot[,input$feature], y = ID, fill = Counts)) +
        xlab(input$feature) +
        geom_tile(color='black')+
        scale_fill_gradient(low = "white", high = "purple", limits = c(0,100))+
        geom_text(aes(label = Counts), color = 'black',size = 6)+
        theme_bw()+
        labs(fill = 'Percentage') +
        ggtitle(paste0('Trimester ',my_tri)) +
        theme(axis.text = element_text(size = 15),
              axis.title.y = element_blank(),
              axis.title.x  = element_text(size = 20),
              legend.text = element_text(size = 15),
              legend.title = element_text(size = 15),
              plot.title = element_text(size = 20))

      if (my_tri != 3)
      {plot = plot+theme(axis.title.x = element_blank())}

      return(plot)

    })

    ggarrange(plotlist = list_plots, nrow=length(list_plots),
              common.legend = TRUE, legend="right")


  })
  
}
