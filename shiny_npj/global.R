# Libraries ---------------------------------------------------------------

library(tidyverse)
library(GGally)
library(ggpubr)
library(reticulate)
library(umap)
library(RColorBrewer)
library(stringr)
library(reshape2)
library(rintrojs)
library(shinydashboard)
library(ggcorrplot)
library(shinyBS)
library(ggalluvial)



# Create help for the app -------------------------------------------------

add_class <- function(x, class) {
  x$attribs <- append(x$attribs, list(class = class))
  x
}

steps_filters <- tibble::tribble(
  ~element, ~intro,
  NA, "Tutorial to use filters",
  ".sample", "This filter allows observe all feature per patient or sample.",
  ".type", "You can choose between all possible outcomes.",
  ".metadata","In this tab, you can filter samples by age, project, pregnancy trimester or ethnicity.",
  ".diversity","You can select the most appropiate diversity metrics for your study.",
  ".composition","It allows to choose the type of phylotype/specie.",
  ".btn_update","When parameters and filters are changes is reqquired to update the plots click on this button."
)

steps_plots <- tibble::tribble(
  ~element, ~intro,
  NA, "This a help to understand that shows each plot",
  ".first_box", "All plots and tables are inside this boxes that can be collpased clicking on the right corner",
  ".bpType", "It shows the frequency of samples or individuals per outcome selected. Inside the bars we can observeth freqeuncy of the demographic feature selected.",
  ".PCrace","It shows the frequency by ethnicity showing the prorpotion of the selected feature.",
  ".bpProject","It shows the frequency by project showing the proportion of the selected feature",
  ".my_race","This table contains the ethnicities included in other.")


# Load main tables --------------------------------------------------------

# Metadata

metadata_all <- read.csv('../Results/metadata_all.csv')


# Diversity

diversity_all <- read.csv('../Results/diversity_all.csv')
diversity_all_long <- melt(diversity_all, 
                           id.vars = c('specimen','participant_id'),
                           variable.name = 'Metrics',
                           value.name = 'Score')

# Phylotypes

phylotypes <- read.csv('../Results/phylotypes_all.csv')
load('../Results/umap_dfs.RData') # umap_dfs

# CST

cst_alluvial <- read.csv('../Results/cst2alluvia.csv')

# Phylotypes Heatmap Taxonomy

load('../Results/heatmap_dfs.RData')
phylo_specie <- grep(pattern = 'pt_.*',
                     colnames(heatmap_dfs$specimen),
                     value = T)

# Studies information

studies <- read.delim(file = '../Results/studies.csv', sep = ';',check.names = F)

# Create final metadata ---------------------------------------------------

all(phylotypes$specimen == diversity_all$specimen) # TRUE Same order
rownames(metadata_all) <- metadata_all$specimen
metadata <- metadata_all[phylotypes$specimen,]
metadata$Trimester <- as.character(metadata$Trimester)
dim(metadata) # 3909

all(metadata$specimen == diversity_all$specimen) # TRUE
metadata %>% select(Type,Age,participant_id,project) %>% distinct() %>% count(project,Type)


# Functions

umap2plot <- function(md,umap, sampletype){
  
  to_plot <- umap %>%
    inner_join(md, by=sampletype)
  
  return(to_plot)
}


metadataSelection <- function(metadata,sampleType){
  
  if (sampleType == 'participant_id'){
    
    df <- do.call('rbind', lapply(unique(metadata$participant_id),function(my_dup){
      
      i <- which.max(metadata[metadata$participant_id == my_dup,'Trimester'])
      metadata[metadata$participant_id == my_dup,][i,]
      
    }))
    
  } else {
    df <- metadata
  }
  
  return(df)
  
}


diversitySelection <- function(diversity_all,sampleType){
  
  if (sampleType == 'participant_id'){
    
    df <- diversity_all[,colnames(diversity_all) != 'specimen'] %>% 
      group_by(participant_id) %>%
      summarise(across(everything(), mean), .groups = 'drop')
    
  } else {
    df <- diversity_all[,colnames(diversity_all) != 'participant_id']
  }
  
  return(df)
  
}

umapSelection <- function(phylotypes_umap,sampleType){
  
  if (sampleType == 'participant_id'){
    
    phylotypes_umap$participant_id <- gsub('-.*','', rownames(phylotypes_umap))
    
    df <- phylotypes_umap %>% 
      group_by(participant_id) %>%
      summarise(across(everything(), mean), .groups = 'drop')
    df <- df %>% column_to_rownames(., var = 'participant_id')
    
  } else {
    df <- phylotypes_umap
  }
  
  return(df)
  
}


phyloSelection <- function(dfs,sampleType){
  
  phylo <- heatmap_dfs[[sampleType]]
  
  return(phylo)
  
}


# Colors
my_colors_race <- brewer.pal(6,'Dark2')
names(my_colors_race) <- unique(metadata$Race)

my_colors_project <- brewer.pal(12,'Paired')
names(my_colors_project) <- unique(metadata$project)

my_colors_age <- brewer.pal(5,'Dark2')
names(my_colors_age) <- unique(metadata$Age)

my_colors_trimester <- brewer.pal(5,'Dark2')
names(my_colors_trimester) <- unique(metadata$Trimester)

my_colors_type <- c('term' = 'grey',
                    'preterm' = '#72467c',
                    'early' = 'mediumpurple')

my_colors_cst <- brewer.pal(7,'Set1')
names(my_colors_cst) <- unique(cst_alluvial$CST)

my_colors = list(
  'project' = my_colors_project,
  'Race' = my_colors_race,
  'Type' = my_colors_type,
  'Age' = my_colors_age,
  'Trimester' = my_colors_trimester,
  'CST' = my_colors_cst
)
