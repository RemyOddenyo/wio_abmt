library(PCAmixdata)
library(clustrd)
library(vegan)
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(plot3D)
library(NbClust)
library(PCDimension)

rm(list=ls())

wio_mpas <- read_csv("C:/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/analysis/r/wio_abmt/wio_mpas_matrix.csv")

# Standardize all numeric columns
wio_mpas_pca  <- wio_mpas[,-c(1:2,52,70)] %>% 
  mutate(across(where(is.numeric), ~as.vector(scale(.))))

wio_mpas_pca <-  wio_mpas_pca %>% 
  mutate(across(c(STAGE, LEVEL), as.factor))
# PCA

X.quanti <- splitmix(wio_mpas_pca)$X.quanti
X.quali <- splitmix(wio_mpas_pca)$X.quali
pca<-PCAmix(X.quanti,X.quali,ndim=20,rename.level=TRUE)
pca<-PCAmix(X.quanti,X.quali,ndim=20,graph=FALSE, rename.level=TRUE)

# Dimension eigenvalue, share observed and cumulative share 

pca_eig<-pca$eig

pca_eig<-as.data.frame(pca_eig)

write_csv(pca_eig, "wio_mpa_pca_eig.csv")

variable_contri_pct<-as.data.frame(pca$quanti$contrib.pct)

write_csv(variable_contri_pct, "variable_contri_pct.csv")


# Compare with broken stick values
# bstick(length(res.pcamix$eig[, 1]), tot.inertia = sum(res.pcamix$eig[, 1]))
 
# determine optimal number of clusters and dimensions by using tuneclus factorial-kmeans

wio_mpa_clus<-tuneclus(wio_mpas_pca,
                       nclusrange = 3:20, 
                       ndimrange = 2:20, 
                       method = c("mixedFKM"), 
                       criterion = "asw", 
                       dst = "full", 
                       alpha = 0, 
                       alphak = NULL, 
                       center = TRUE, 
                       scale = TRUE, 
                       rotation = "none", 
                       nstart = 100, 
                       smartStart = NULL, 
                       seed = NULL)



# Run integrated PCAmix and K-means (e.g., seeking 2 clusters in 7 dimensions)

res.clus <- cluspcamix(data = wio_mpas_pca, 
                       method = "mixedFKM",
                       nclus = 16, 
                       ndim = 12, 
                       alpha = 0, 
                       center = TRUE, 
                       scale = TRUE, 
                       rotation = "none", 
                       nstart = 100, 
                       smartStart = NULL, 
                       seed = NULL)

# View cluster assignments
res.clus$cluster 


wio_mpas$cluster<-res.clus$cluster 

# Plot the individuals colored by their clusters
plot(res.clus)

# Extract squared loadings
sqload_df <- as.data.frame(pca$sqload)
sqload_df$Variable <- rownames(sqload_df)

# Reshape from wide to long format for ggplot
sqload_long <- sqload_df %>%
  pivot_longer(cols = starts_with("dim"), 
               names_to = "Dimension", 
               values_to = "Squared_Loading")

# Create a bar plot for Dimension 1
sqload_long %>%
  filter(Dimension == "dim 1") %>%
  ggplot(aes(x = reorder(Variable, Squared_Loading), y = Squared_Loading, fill = Squared_Loading)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Contributions (Squared Loadings) - Dimension 1 (24.01%)",
    x = "Variables (Quantitative & Qualitative)",
    y = "Squared Loading (Contribution)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 2
sqload_long %>%
  filter(Dimension == "dim 2") %>%
  ggplot(aes(x = reorder(Variable, Squared_Loading), y = Squared_Loading, fill = Squared_Loading)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Contributions (Squared Loadings) - Dimension 2 (16.26%)",
    x = "Variables (Quantitative & Qualitative)",
    y = "Squared Loading (Contribution)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 3
sqload_long %>%
  filter(Dimension == "dim 3") %>%
  ggplot(aes(x = reorder(Variable, Squared_Loading), y = Squared_Loading, fill = Squared_Loading)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Contributions (Squared Loadings) - Dimension 3 (12.3%)",
    x = "Variables (Quantitative & Qualitative)",
    y = "Squared Loading (Contribution)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 4
sqload_long %>%
  filter(Dimension == "dim 4") %>%
  ggplot(aes(x = reorder(Variable, Squared_Loading), y = Squared_Loading, fill = Squared_Loading)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Contributions (Squared Loadings) - Dimension 4 (9.43%)",
    x = "Variables (Quantitative & Qualitative)",
    y = "Squared Loading (Contribution)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Plot Dim 1 vs Dim 2 squared loadings
ggplot(sqload_df, aes(x = `dim 1`, y = `dim 2`, label = Variable)) +
  geom_text_repel(max.overlaps = Inf, box.padding=0.5, size=4)+
  geom_point(color = "steelblue", size = 3) +
  xlim(0, 1) + ylim(0, 1) +
  labs(
    title = "PCAmix Variable Contributions (Squared Loadings)",
    x = "Dimension 1 Squared Loading (24.01%)",
    y = "Dimension 2 Squared Loading (16.26%)"
  ) +
  theme_classic()

# Extract level coordinates for dimensions 1 and 2
levels_df <- as.data.frame(pca$levels$coord)
levels_df$Level <- rownames(levels_df)

# Optional: Extract percentage of variance explained for axis labels
eig <- pca$eig
pct1 <- round(eig[1, 2], 1)
pct2 <- round(eig[2, 2], 1)
pct3 <- round(eig[3, 2], 1)
pct4 <- round(eig[4, 2], 1)

# Plot with ggplot2
ggplot(levels_df, aes(x = `dim 1`, y = `dim 2`, label = Level)) +
  geom_point(color = "maroon", size = 2) +
  geom_text_repel(max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "PCAmix: Levels Component Map",
    x = paste0("Dimension 1 (", pct1, "%)"),
    y = paste0("Dimension 2 (", pct2, "%)")
  ) +
  theme_classic()

# Extract individual coordinates into a data frame
ind_coords <- as.data.frame(pca$ind$coord)

# Add metadata/labels for ggplot mapping (e.g., Label/Type)
wio_mpas_i<-wio_mpas %>% 
  mutate(index = row_number())
ind_coords$label <- wio_mpas_i$index
ind_coords$Country<-wio_mpas$COUNTRY
ind_coords$Cluster<- res.clus$cluster
ind_coords$MPA_name<-wio_mpas$MPA_NAME

ind_coords$Cluster<-as.factor(ind_coords$Cluster)

ind_coords$`Protection level` <- X.quali[, 2] # Extracting a qualitative variable for coloring
ind_coords$`Protection level`=ordered(ind_coords$`Protection level`,levels=c("Fully Protected","Lightly Protected","Minimally Protected / Incompatible","Unclassified"))
# Extract percentage of variance explained for axis labels
pc1_var <- round(pca$eig[1, 2], 1)
pc2_var <- round(pca$eig[2, 2], 1)
pc3_var <- round(pca$eig[3, 2], 1)

write_csv(ind_coords,"ind_coords_clust.csv")

# Plot 1 using ggplot2
ggplot(ind_coords, aes(x = `dim 1`, y = `dim 2`, color = `Protection level`, shape=Country)) +
  geom_point(size = 3,stroke=1) +
  # Optional: add text labels to points
  geom_text_repel(aes(label=label),max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values=c("#3cb44b","#0045C1","#f58231","#800000"))+
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 1 (", pc1_var, "%)"),
    y = paste0("Dimension 2 (", pc2_var, "%)"),
    color = "Protection level",
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes

# Plot of dim 1 vs dim2
ggplot(ind_coords, aes(x = `dim 1`, y = `dim 2`, color = `Protection level`, shape=Cluster)) +
  geom_point(size = 3,stroke=1) +
  # Optional: add text labels to points
  geom_text_repel(aes(label=label),max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values=c("#0072B2", # Blue
                              "#E69F00", # Orange
                              "#009E73", # Bluish Green
                              "#CC79A7" # Reddish Purple
                              
  ))+
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9,10,11,3,13,14,15,16))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 1 (", pc1_var, "%)"),
    y = paste0("Dimension 2 (", pc2_var, "%)")
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes


# Plot of dim 1 vs dim3
ggplot(ind_coords, aes(x = `dim 1`, y = `dim 3`, color = `Protection level`, shape=Cluster)) +
  geom_point(size = 3,stroke=1) +
  # Optional: add text labels to points
  geom_text_repel(aes(label=label),max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values=c("#0072B2", # Blue
                              "#E69F00", # Orange
                              "#009E73", # Bluish Green
                              "#CC79A7" # Reddish Purple
                              
  ))+
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9,10,11,3,13,14,15,16))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 1 (", pc1_var, "%)"),
    y = paste0("Dimension 3 (", pc3_var, "%)")
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes

# Plot of dim 2 vs dim3
ggplot(ind_coords, aes(x = `dim 2`, y = `dim 3`, color = `Protection level`, shape=Cluster)) +
  geom_point(size = 3,stroke=1) +
  # Optional: add text labels to points
  geom_text_repel(aes(label=label),max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values=c("#0072B2", # Blue
                              "#E69F00", # Orange
                              "#009E73", # Bluish Green
                              "#CC79A7" # Reddish Purple
                              
  ))+
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9,10,11,3,13,14,15,16))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 2 (", pc2_var, "%)"),
    y = paste0("Dimension 3 (", pc3_var, "%)")
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes

# 3D scatter plot with a fourth dimension mapped to color
wio_mpas_3d<-scatter3D(x = ind_coords$`dim 1`, 
          y = ind_coords$`dim 2`, 
          z = ind_coords$`dim 3`, 
          colvar = ind_coords$Cluster, # 4th dimension maps to color legend
          pch = 18, 
          ticktype = "detailed",
          labels=ind_coords$label,
          xlab="Dim 1",
          ylab="Dim 2",
          zlab="Dim 3")
