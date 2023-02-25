

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

# Create final metadata ---------------------------------------------------

all(phylotypes$specimen == diversity_all$specimen) # TRUE Same order
rownames(metadata_all) <- metadata_all$specimen
metadata <- metadata_all[phylotypes$specimen,]
metadata$Trimester <- as.character(metadata$Trimester)
dim(metadata) # 3909

all(metadata$specimen == diversity_all$specimen) # TRUE
metadata %>% select(Type,Range,participant_id,project) %>% distinct() %>% count(project,Type)




# Functions

umap2plot <- function(df,phylotypes){
  
  to_plot <- phylotypes[df$specimen,]
  
  phylo_umap <- umap(d = to_plot,method = 'umap-learn',
                     metric = 'braycurtis',
                     n_neighbors = 45, n_components = 2,
                     min_dist = 1, spread = 1.1,random_state = 6)
  
  umap_df <- phylo_umap$layout %>%
    as.data.frame()%>%
    rename(UMAP1="V1",
           UMAP2="V2") %>%
    mutate(specimen=rownames(to_plot)) %>%
    inner_join(df, by='specimen')
  
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



# Colors
my_colors_race <- brewer.pal(6,'Dark2')
names(my_colors_race) <- unique(metadata$NIH.Racial.Category)

my_colors_project <- brewer.pal(12,'Paired')
names(my_colors_project) <- unique(metadata$project)

my_colors_age <- brewer.pal(5,'Dark2')
names(my_colors_age) <- unique(metadata$Range)

my_colors_trimester <- brewer.pal(5,'Dark2')
names(my_colors_trimester) <- unique(metadata$Trimester)

my_colors_type <- c('term' = 'grey',
                    'preterm' = '#72467c',
                    'early' = 'mediumpurple')

my_colors_cst <- brewer.pal(7,'Set1')
names(my_colors_cst) <- unique(cst_alluvial$CST)

my_colors = list(
  'project' = my_colors_project,
  'NIH.Racial.Category' = my_colors_race,
  'Type' = my_colors_type,
  'Range' = my_colors_age,
  'Trimester' = my_colors_trimester,
  'CST' = my_colors_cst
)
