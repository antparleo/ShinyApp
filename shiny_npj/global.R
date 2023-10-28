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

steps_tutorial <- tibble::tribble(
  ~element, ~intro,
  NA, "Welcome to VMAP: This tutorial will you show all possible features of this app. Fristly, we begin with the filters.",
  ".sample", "This filter allows to observe all features per individual or sample since different samples were taken from the same individual.",
  ".type", "You can also choose between all possible outcomes (Term, Pre-term or early pre-term.)",
  ".metadata","In this tab, you will find different filters for age, project, pregnancy trimester or ethnicity.",
  ".diversity","Moreover, you can select your most appropiate diversity metric.",
  ".composition","It allows to choose the type of phylotype/specie that you want to observe. Try to write down the name inside the box.",
  NA, "Now, we are going to explain what represents each chart.",
  ".first_box", "First, It is worth to noting that all plots and tables are inside purple boxes that can be collpased clicking on the right corner. This will allow to hide those plots that you do not need.",
  ".bpType", "This barplot shows the frequency of samples or individuals per outcome selected. Inside the bars we can also observe the frequency of the demographic feature selected.",
  ".PCrace","Likely previous one, it shows the frequency, but by ethnicity.",
  ".bpProject","Finally, in this barplot frequency is shown by projects.",
  ".my_race","This table contains the different ethnicities included in the label 'Other'.",
  ".my_project","Here, We observed the information related to each project. You can click on the 'Accession ID' in order to read more about them.",
  ".cpDiversity","Plot represents the pairwise correlations between diversity metrics as well as its respective distribution according to the feature selected.",
  ".vpDiversity","Violin plots showing the distribution according to the feature selected.",
  ".my_text","The description of each diversity metrics can be found on this table",
  ".apCST","This chart shows an alluvial plot, where we can observe the different proportions of CST across trimesters and divided according to the feature selected.",
  ".hmphylo","It represents a heatmap of the different type of phylotypes/species depending on the trimester and the feature selected.",
  ".upPhylo","This final chart represents the complexity of microbiome data in two dimensions where points are coloured by the feature selected.",
  ".btn_update","IMPORTANT: When parameters and filters have changed, it is required to update the plots clicking on this button.",
)

# steps_plots <- tibble::tribble(
#   ~element, ~intro,
#   NA, "Now, we are going to explain what represents each chart.",
#   ".first_box", "First, It is worth to noting that all plots and tables are inside purple boxes that can be collpased clicking on the right corner",
#   ".bpType", "It shows the frequency of samples or individuals per outcome selected. Inside the bars we can observe the frquency of the demographic feature selected.",
#   ".PCrace","It shows the frequency by ethnicity showing the prorpotion of the selected feature.",
#   ".bpProject","It shows the frequency by project showing the proportion of the selected feature",
#   ".my_race","This table contains the ethnicities included in other catgory.",
#   ".my_project","Likewise, on this table all information related to the different datasets used can be observed.",
#   ".cpDiversity","Plot represents the pairwise correlations between diversity metrics as well as its respective distribution according to the feature selected.",
#   "vpDiversity","Violin plots showing the distribution according to the feature selected.",
#   ".my_text","The description of each diversity metrics can be found on this table",
#   ".apCST","This chart shows an alluvial plot, where can observe the different proportions of CST across trimesters.",
#   ".hmphylo","It represents a heatmap of the different type of phylotypes/species across time according to the feature selected.",
#   ".upPhylo","This final chart represents the complexity of microbiome data in two dimensions where points are coloured by the feature selected."
#   
#   )


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
