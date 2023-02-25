# Server

server <- function(input, output, session) {
  
  # observeEvent(input$goButton, ({
  #   updateButton(session, "goButton", disabled = !input$goButton)
  # }))
  
  # TypeInput <- reactive({
  #   
  #   index <- ifelse(input$type == 'preterm',
  #                   c('early', 'preterm'),
  #                   input$type)
  #   
  #   metadata <- metadata %>% 
  #     filter(Type %in% index)
  #   
  # })
  
  metadataType <- reactive({
    
    metadata <- metadataSelection(metadata,input$sample)
    
  })
  
  
  
  metadataInput <- reactive({
  
    input$start
    
    df <- metadataType() %>%
      filter((Range %in% input$Age) &
               project%in%input$projects &
               Type%in%input$type &
               NIH.Racial.Category %in% input$Race &
               Trimester %in% input$trimester)

  })


  cstInput <- reactive({

   cst_alluvia2 <- cst_alluvial %>%
     filter((Range %in% input$Age) &
              project%in%input$projects &
              Type%in%input$type &
              NIH.Racial.Category %in% input$Race &
              Trimester %in% input$trimester)

  })
  
  
  
  output$bpType <- renderPlot({
    
    input$start
    
    isolate(df <- metadataInput())
    plot <- data.frame(table(df[,c('Type',input$feature)]))
    
    isolate(ggplot(plot,
                   aes(x = Type, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'fill', stat = 'identity', color = 'black') +
              scale_fill_manual(values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'No. Samples', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)) +
              scale_x_discrete(labels=c("term" = "Term",
                                        "preterm" = "Preterm",
                                        "early" = "Early Preterm"
              )))
    
  })
  
  output$PCrace <- renderPlot({
    
    input$start
    
    isolate(df <- metadataInput())
    plot <- data.frame(table(df[,c('NIH.Racial.Category',input$feature)]))
    
    isolate(ggplot(plot,
                   aes(x = NIH.Racial.Category, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'fill', stat = 'identity', color = 'black') +
              scale_fill_manual(values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'No. Samples', x = '')+
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
                                        )))
  })
  
  output$bpProject <- renderPlot({
    
    input$start
    
    isolate(df <- metadataInput())
    plot <- data.frame(table(df[,c('project',input$feature)]))
    
    isolate(ggplot(plot,
           aes(x = project, fill = plot[,input$feature], y = Freq)) +
      geom_bar(position = 'fill', stat = 'identity', color = 'black') +
      scale_fill_manual(values = my_colors[[input$feature]]) +
      labs(fill = input$feature, y = 'No. Samples', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20)))
    
  })
  
  
  diversityType <- reactive({
    
    df <- diversitySelection(diversity_all,input$sample)
    
  })
  
 
  output$cpDiversity <- renderPlot({

    input$start
    
    isolate(md <- metadataInput())
    to_plot <- merge(diversityType(),
                        md,
                        by=input$sample)

    isolate(ggpairs(data = to_plot, columns = input$metrics,
            aes(color = to_plot[,input$feature]),
            upper = list(continuous = wrap("cor", size = 6)))+
       scale_fill_manual(values = my_colors[[input$feature]]) +
      scale_color_manual(values = my_colors[[input$feature]]) +
      theme_bw()+
      theme(strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            legend.position = 'bottom')
    )


  })
  
  
  
  output$vpDiversity <- renderPlot({

    input$start
    isolate(md <- metadataInput())

    diversity_all_long <- melt(diversityType(), 
                              id.vars = input$sample,
                              variable.name = 'Metrics',
                              value.name = 'Score')
   
    
    diver_md <- merge(x = md, y = diversity_all_long,
                      by = input$sample)
    
    isolate(diver_md <- diver_md[diver_md$Metrics%in%input$metrics,])

    list_plot <- lapply(unique(diver_md[,'Metrics']), function(my_metric){
      to_plot <- diver_md[diver_md$Metrics == my_metric,]
      ggplot(data = to_plot,
             aes(x = Metrics, y = Score,
                 fill = to_plot[,input$feature]))+
        geom_violin()+
        scale_fill_manual(values = my_colors[[input$feature]]) +
        labs(fill = input$feature, y = 'Score')+
        theme_bw() +
        theme(axis.text = element_text(size = 15),
              axis.title = element_text(size = 25),
              legend.text = element_text(size = 12),
              legend.title = element_text(size = 20))

    })

    ggarrange(plotlist = list_plot, ncol=length(list_plot),
              common.legend = TRUE, legend="right")

  })



  output$apCST <- renderPlot({

    input$start

    isolate(cst_alluvia2 <- cstInput())
    isolate(values <- unique(cst_alluvia2[,input$feature]))

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
        scale_fill_manual(values = my_colors[['CST']]) +
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
  # 
  # 
  # umapInput <-  reactive({
  # 
  #   umap2plot(metadataInput(),phylotypes_umap)
  # 
  # }) %>%
  #   bindCache(metadataInput())
  # 
  # output$upPhylo <- renderPlot({
  # 
  #   input$start
  # 
  #   # df <- metadataInput()
  #   # to_plot <- phylotypes[df$specimen,]
  #   #
  #   # phylo_umap <- umap(d = to_plot,method = 'umap-learn',
  #   #                    metric = 'braycurtis',
  #   #                    n_neighbors = 45, n_components = 2,
  #   #                    min_dist = 1, spread = 1.1,random_state = 6)
  #   #
  #   # umap_df <- phylo_umap$layout %>%
  #   #   as.data.frame()%>%
  #   #   rename(UMAP1="V1",
  #   #          UMAP2="V2") %>%
  #   #   mutate(specimen=rownames(to_plot)) %>%
  #   #   inner_join(df, by='specimen')
  #   #
  #   # isolate(umap_df$Division <- umap_df[,input$divison])
  # 
  #   isolate(toplot <- umapInput())
  #   # isolate(toplot$Division <- toplot[,input$division])
  #   isolate(ggplot(toplot, aes(x = UMAP1,
  #                               y = UMAP2,
  #                               color = toplot[,input$feature]))+
  #             geom_point()+
  #             scale_color_manual(values = my_colors[[input$feature]])+
  #             labs(x = "UMAP1",
  #                  y = "UMAP2",
  #                  color = "")+
  #             theme_bw() +
  #     theme(legend.position = "bottom",text = element_text(size = 20),
  #           legend.text = element_text(size=15)) +
  #     guides(fill=guide_legend(nrow=3,byrow=TRUE)))
  #            # shape = guide_legend(override.aes = list(size = 5))))
  # 
  # 
  # })
  # 
  
}
