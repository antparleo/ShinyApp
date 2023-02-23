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
  
  
  
  metadataInput <- reactive({
  
    input$start
    
    df <- metadata %>%
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
    df <- distinct(df[,c('participant_id','Type')])
    
    isolate(df <- metadataInput())
    df <- distinct(df[,c('participant_id', 'Type', input$feature)])
    plot <- data.frame(table(df[,c('Type',input$feature)]))
    
    isolate(ggplot(plot,
                   aes(x = Type, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'stack', stat = 'identity', color = 'black') +
              scale_fill_manual(values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'No. Samples', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)))
    
  })
  
  output$PCrace <- renderPlot({
    
    input$start
    
    isolate(df <- metadataInput())
    df <- distinct(df[,c('participant_id', 'NIH.Racial.Category', input$feature)])
    plot <- data.frame(table(df[,c('NIH.Racial.Category',input$feature)]))
    
    isolate(ggplot(plot,
                   aes(x = NIH.Racial.Category, fill = plot[,input$feature], y = Freq)) +
              geom_bar(position = 'stack', stat = 'identity', color = 'black') +
              scale_fill_manual(values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'No. Samples', x = '')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15, angle = 90),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20)))
    
    # isolate(to_plot <- count(metadataInput(),NIH.Racial.Category) %>%
    #   arrange(desc(NIH.Racial.Category)) %>%
    #   mutate(prop = n / sum(.$n) *100) %>%
    #   mutate(ypos = cumsum(prop)- 0.5*prop))
    # 
    # isolate(ggplot(to_plot,
    #        aes(x = '', fill = NIH.Racial.Category, y = prop)) +
    #   geom_bar(stat = 'identity', color = 'black') +
    #   scale_fill_manual(values=my_colors_race)+
    #   coord_polar('y', start = 0)+
    #   labs(fill = '', y = '', x = '')+
    #   theme_void() + 
    #   theme(legend.text = element_text(size = 12),
    #         legend.title = element_text(size = 20),
    #         legend.position = 'bottom') +
    #   guides(fill=guide_legend(nrow=3,byrow=TRUE)) +
    #   geom_text(aes(y = ypos, label = n), color = "white", size=6))
    
  })
  
  output$bpProject <- renderPlot({
    
    input$start
    
    isolate(df <- metadataInput())
    df <- distinct(df[,c('participant_id', 'project', input$feature)])
    plot <- data.frame(table(df[,c('project',input$feature)]))
    
    isolate(ggplot(plot,
           aes(x = project, fill = plot[,input$feature], y = Freq)) +
      geom_bar(position = 'stack', stat = 'identity', color = 'black') +
      scale_fill_manual(values = my_colors[[input$feature]]) +
      labs(fill = input$feature, y = 'No. Samples', x = '')+
      theme_bw() + 
      theme(axis.text = element_text(size = 15),
            axis.title = element_text(size = 25),
            legend.text = element_text(size = 12),
            legend.title = element_text(size = 20)))
    
  })
  
  
 
  output$cpDiversity <- renderPlot({

    input$start

    isolate(md <- metadataInput())

    to_plot <- diversity_all %>% filter(specimen%in%md[,'specimen'])

    rownames(to_plot) <- to_plot$specimen
    to_plot <- to_plot[md$specimen,]
    diver_md <- cbind(to_plot,md[, -grep('specimen',colnames(md))])

    isolate(ggpairs(data = diver_md, columns = input$metrics,
            aes(color = diver_md[,input$feature]),
            upper = list(continuous = wrap("cor", size = 6)))+
       scale_fill_manual(values = my_colors[[input$feature]]) +
      scale_color_manual(values = my_colors[[input$feature]]) +
      theme_bw()+
      theme(strip.text.x = element_text(size = 20),
            strip.text.y = element_text(size = 20),
            legend.position = 'bottom')
    )


  }) %>% bindCache(metadataInput(), input$feature, input$metrics)
  
  
  
  output$vpDiversity <- renderPlot({
    
    input$start
    
    isolate(md <- metadataInput())
    diver_md <- merge(x = md, y = diversity_all_long,
                      by = 'specimen')
    isolate(diver_md <- diver_md[diver_md$Metrics%in%input$metrics,])
    isolate(ggplot(data = diver_md,
                   aes(x = Metrics, y = Score,
                                        fill = diver_md[,input$feature]))+
              geom_violin()+
              scale_fill_manual(values = my_colors[[input$feature]]) +
              labs(fill = input$feature, y = 'Score')+
              theme_bw() + 
              theme(axis.text = element_text(size = 15),
                    axis.title = element_text(size = 25),
                    legend.text = element_text(size = 12),
                    legend.title = element_text(size = 20))
            )
    
    
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
    
    # cst_alluvia_ept <- cst_alluvia2 %>% filter(delivery_wk < 32)
    # 
    # df_term <- cst_alluvia2[cst_alluvia2$Type == "term",]
    # df_preterm <- cst_alluvia2[cst_alluvia2$Type == "preterm",]
    # 
    # 
    # to_plot_term <- df_term[,c("Trimester","participant_id","CST")]
    # term_freq <- to_plot_term %>% group_by(participant_id,Trimester,CST) %>% summarise(Freq = n() )
    # term_freq <- term_freq %>%
    #   group_by(Trimester) %>%
    #   mutate(pct = Freq / sum(Freq)*100)
    # 
    # 
    # to_plot_preterm <- df_preterm[,c("Trimester","participant_id","CST")]
    # preterm_freq <- to_plot_preterm %>% group_by(participant_id,Trimester,CST) %>% summarise(Freq = n() )
    # preterm_freq <- preterm_freq %>%
    #   group_by(Trimester) %>%
    #   mutate(pct = Freq / sum(Freq)*100)
    # 
    # to_plot_epreterm <- cst_alluvia_ept[,c("Trimester","participant_id","CST")]
    # epreterm_freq <- to_plot_epreterm %>% group_by(participant_id,Trimester,CST) %>% summarise(Freq = n() )
    # epreterm_freq <- epreterm_freq %>%
    #   group_by(Trimester) %>%
    #   mutate(pct = Freq / sum(Freq)*100)
    # 
    # p1 <- ggplot(term_freq,
    #              aes(x = Trimester, stratum = CST, alluvium = participant_id,
    #                  fill = CST, label = CST, y = pct)) +
    #   scale_fill_brewer(palette = "Set2") +
    #   geom_flow(stat = "alluvium", lode.guidance = "frontback") +
    #   geom_stratum() +
    #   scale_x_discrete(breaks=c("1","2","3"),
    #                    labels=c("1 \n(n = 67)", "2 \n(n = 182)", "3 \n(n = 128)")) +
    #   ggtitle("Term")+
    #   theme_bw()+
    #   ylab("Freq")+
    #   theme(legend.position = "none", text = element_text(size = 20))
    # 
    # p2 <- ggplot(preterm_freq,
    #              aes(x = Trimester, stratum = CST, alluvium = participant_id,
    #                  fill = CST, label = CST, y = pct)) +
    #   scale_fill_brewer(palette = "Set2") +
    #   geom_flow(stat = "alluvium", lode.guidance = "frontback") +
    #   geom_stratum() +
    #   scale_x_discrete(breaks=c("1","2","3"),
    #                    labels=c("1 \n(n = 43)", "2 \n(n = 88)", "3 \n(n = 75)")) +
    #   ggtitle("Preterm")+
    #   ylab("Freq")+
    #   theme_bw()+
    #   theme(legend.position = "none",text = element_text(size = 20))
    # 
    # p3 <- ggplot(epreterm_freq,
    #              aes(x = Trimester, stratum = CST, alluvium = participant_id,
    #                  fill = CST, label = CST, y = pct)) +
    #   scale_fill_brewer(palette = "Set2") +
    #   geom_flow(stat = "alluvium", lode.guidance = "frontback") +
    #   geom_stratum() +
    #   scale_x_discrete(breaks=c("1","2","3"),
    #                    labels=c("1 \n(n = 9)", "2 \n(n = 21)", "3 \n(n = 13)")) +
    #   ggtitle("Early preterm")+
    #   ylab("Freq")+
    #   theme_bw()+
    #   theme(legend.position = "right",text = element_text(size = 20),
    #         legend.text = element_text(size=15))
    # 
    # ggarrange(p1, p2, p3, ncol=3, common.legend = TRUE, legend="right")

  })


  umapInput <-  reactive({

    umap2plot(metadataInput(),phylotypes_umap)

  }) %>%
    bindCache(metadataInput())

  output$upPhylo <- renderPlot({

    input$start

    # df <- metadataInput()
    # to_plot <- phylotypes[df$specimen,]
    #
    # phylo_umap <- umap(d = to_plot,method = 'umap-learn',
    #                    metric = 'braycurtis',
    #                    n_neighbors = 45, n_components = 2,
    #                    min_dist = 1, spread = 1.1,random_state = 6)
    #
    # umap_df <- phylo_umap$layout %>%
    #   as.data.frame()%>%
    #   rename(UMAP1="V1",
    #          UMAP2="V2") %>%
    #   mutate(specimen=rownames(to_plot)) %>%
    #   inner_join(df, by='specimen')
    #
    # isolate(umap_df$Division <- umap_df[,input$divison])

    isolate(toplot <- umapInput())
    # isolate(toplot$Division <- toplot[,input$division])
    isolate(ggplot(toplot, aes(x = UMAP1,
                                y = UMAP2,
                                color = toplot[,input$feature]))+
              geom_point()+
              scale_color_manual(values = my_colors[[input$feature]])+
              labs(x = "UMAP1",
                   y = "UMAP2",
                   color = "")+
              theme_bw() +
      theme(legend.position = "bottom",text = element_text(size = 20),
            legend.text = element_text(size=15)) +
      guides(fill=guide_legend(nrow=3,byrow=TRUE)))
             # shape = guide_legend(override.aes = list(size = 5))))


  })
  
  
}
