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

colnames(phylo_1e1) <- paste(phylo_specie[index,]$phylotypes,
                             phylo_specie[index,]$Specie,
                             sep = ' | ')



# UMAP table --------------------------------------------------------------

## Specimen

phylo_umap_specimen <- umap(d = phylotypes_umap,method = 'umap-learn',
                   metric = 'braycurtis',
                   n_neighbors = 45, n_components = 2,
                   min_dist = 1, spread = 1.1,random_state = 6)

umap_df_specimen <- phylo_umap_specimen$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(specimen=rownames(phylotypes_umap))

save(umap_df_specimen,file = '../Results/umap_specimen.RData')

## Participant

phylotypes_umap$participant_id <- gsub('-.*','', rownames(phylotypes_umap))

phylotypes_participant <- phylotypes_umap %>% 
  group_by(participant_id) %>%
  summarise(across(everything(), mean), .groups = 'drop')

phylotypes_participant <- phylotypes_participant %>%
  column_to_rownames(., var = 'participant_id')

phylo_umap_participant <- umap(d = phylotypes_participant, method = 'umap-learn',
                            metric = 'braycurtis',
                            n_neighbors = 45, n_components = 2,
                            min_dist = 1, spread = 1.1,random_state = 6)

umap_df_participant <- phylo_umap_participant$layout %>%
  as.data.frame()%>%
  rename(UMAP1="V1",
         UMAP2="V2") %>%
  mutate(participant_id=rownames(phylotypes_participant))

save(umap_df_participant,file = '../Results/umap_participant.RData')

umap_dfs <- list('specimen' = umap_df_specimen, 'participant_id' = umap_df_participant)
save(umap_dfs,file = '../Results/umap_dfs.RData')


# Heatmap taxonomy

phylo_specie <- read.csv('../Results/phylo_specie_1e1.csv')
phylo_1e1 <- read.csv('../Results/phylotypes_1e1_heatmap.csv', row.names = 1)

index <- match(colnames(phylo_1e1),phylo_specie$phylotypes)
all(colnames(phylo_1e1) == phylo_specie[index,]$phylotypes) # TRUE

colnames(phylo_1e1) <- paste(phylo_specie[index,]$phylotypes,
                             phylo_specie[index,]$Specie,
                             sep = ' | ')

# Specimen

phylo_heatmap_specimen <- phylo_1e1
phylo_heatmap_specimen$specimen <- rownames(phylo_heatmap_specimen)


# Participant

phylo_heatmap_participant <- phylo_1e1
phylo_heatmap_participant$participant_id <- gsub('-.*','', rownames(phylo_heatmap_participant))
phylo_heatmap_participant <- phylo_heatmap_participant %>%
      group_by(participant_id) %>%
      summarise(across(everything(), mean), .groups = 'drop')

heatmap_dfs <- list('specimen' = phylo_heatmap_specimen,
                    'participant_id' = phylo_heatmap_participant)
save(heatmap_dfs, file = '../Results/heatmap_dfs.RData')
