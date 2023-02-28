

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(GGally)
library(ggpubr)
library(reticulate)
library(umap)
library(RColorBrewer)
library(stringr)
library(reshape2)

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
phylotypes_umap <- read.csv('Pt5e_1.combined.pseudocounts.csv', row.names = 1)
all(rownames(phylotypes_umap) %in%phylotypes$specimen) # TRUE

phylotypes_umap <- phylotypes_umap[phylotypes$specimen,] # TRUE

# CST

cst_alluvial <- read.csv('../Results/cst2alluvia.csv')

# Phylotypes Heatmap Taxonomy

phylo_specie <- read.csv('../Results/phylo_specie_1e1.csv')
phylo_1e1 <- read.csv('../Results/phylotypes_1e1_heatmap.csv', row.names = 1)

index <- match(colnames(phylo_1e1),phylo_specie$phylotypes)
all(colnames(phylo_1e1) == phylo_specie[index,]$phylotypes) # TRUE

colnames(phylo_1e1) <- phylo_specie[index,]$Specie
# phylo_1e1$specimen <- rownames(phylo_1e1)

# Create final metadata ---------------------------------------------------

all(phylotypes$specimen == diversity_all$specimen) # TRUE Same order
rownames(metadata_all) <- metadata_all$specimen
metadata <- metadata_all[phylotypes$specimen,]
metadata$Trimester <- as.character(metadata$Trimester)
dim(metadata) # 3909

all(metadata$specimen == diversity_all$specimen) # TRUE
metadata %>% select(Type,Age,participant_id,project) %>% distinct() %>% count(project,Type)




# Functions

umap2plot <- function(df,phylotypes, SampleType){
  
  to_plot <- phylotypes[df[,SampleType],]
  
  phylo_umap <- umap(d = to_plot,method = 'umap-learn',
                     metric = 'braycurtis',
                     n_neighbors = 45, n_components = 2,
                     min_dist = 1, spread = 1.1,random_state = 6)
  
  umap_df <- phylo_umap$layout %>%
    as.data.frame()%>%
    rename(UMAP1="V1",
           UMAP2="V2") %>%
    mutate(!!SampleType:=rownames(to_plot)) %>%
    inner_join(df, by=SampleType)
  
  return(umap_df)
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

colnames(phylo_1e1) <- make.unique(colnames(phylo_1e1), sep = '_')


phyloSelection <- function(phylo,sampleType){
  
  if (sampleType == 'participant_id'){
    
    phylo$participant_id <- gsub('-.*','', rownames(phylo))
    
    phylo <- phylo %>%
      group_by(participant_id) %>%
      summarise(across(everything(), mean), .groups = 'drop')
    # phylo <- phylo %>% column_to_rownames(., var = 'participant_id')
    
  } else {
    phylo$specimen <- rownames(phylo)
  }
  
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
