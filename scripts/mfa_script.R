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

#rm(list=ls())

wio_mpas <-read_csv("csv/wio_mpas_matrix.csv") # data with variables grouped by country e.g. hdi

# Standardize all numeric columns
wio_mpas_mfa <- wio_mpas[,-c(1:4,62,80)] %>% 
 mutate(across(where(is.numeric), ~as.vector(scale(.))))

wio_mpas_mfa <-  wio_mpas_mfa %>% 
  mutate(across(c(STAGE, LEVEL), as.factor))

colnames(wio_mpas_mfa)

index<- c(rep(1,2),rep(2,2),rep(3,14),rep(4,5),rep(5,18),rep(6,20),rep(3,13),rep(4,30) )
names <- c("age_area", 
           "stage_level", 
           "ecology", 
           "contextual_enabling_conditions",
           "threats",
           "internal_enabling_conditions"
           )

# run MFAmix
mfamix<- MFAmix(data = wio_mpas_mfa, 
                     groups = index, 
                     name.groups = names, 
                     ndim = 14, 
                     graph = FALSE,
                     rename.level=TRUE
                     )


# Extract individual factor scores (coordinates)
scores <- mfamix$ind$coord

wio_mfa_clus<-tuneclus(
  scores,
  nclusrange = 3:15, 
  ndimrange = 2:15, 
  method = c("mixedFKM"), 
  criterion = "asw",       # Average Silhouette Width ("ch" for Calinski-Harabasz)
  dst = "full", 
  alpha = 0, 
  alphak = NULL,           
  center = FALSE,          # MFA coordinates are already centered
  scale = FALSE,           # Preserve MFA group-variance weighting
  rotation = "none", 
  nstart = 100, 
  smartStart = NULL, 
  seed = NULL)

# K-Means Clustering
set.seed(123)
km <- kmeans(scores, centers = 15, nstart = 25)
clusters <- as.factor(km$cluster)

# eigenvalues

mfamix$eig

mfa_eig<-mfamix$eig

mfa_eig<-as.data.frame(mfa_eig)

write_csv(mfa_eig, "wio_mpa_mfa_eig.csv")

variable_contri_pct_mfa<-as.data.frame(res.mfamix$quanti$contrib.pct)

write.csv(variable_contri_pct_mfa, "variable_contri_pct_mfa.csv")

ind_cos2_mfa<-as.data.frame(mfamix$ind$cos2)

ind_cos2_mfa <- ind_cos2_mfa %>% 
  mutate(MPA_NAME = wio_mpas$MPA_NAME, .before = 1)

write.csv(ind_cos2_mfa, "ind_cos2_mfa.csv")

variable_quanti_mfa_cos2<-as.data.frame(res.mfamix$quanti$cos2)

write.csv(variable_quanti_mfa_cos2, "variable_quanti_mfa_cos2.csv")

# Compare with broken stick values
# bstick(length(res.mfamix$eig[, 1]), tot.inertia = sum(res.mfamix$eig[, 1]))

# determine optimal number of clusters and dimensions by using tuneclus factorial-kmeans

# Plot the individuals colored by their clusters
plot(mfamix, choice="axes")

# Extract cos2
var_quant_cos2_mfa_df <- as.data.frame(mfamix$quanti$cos2)
var_quant_cos2_mfa_df$Variable <- rownames(var_quant_cos2_mfa_df)

# Extract squared loadings
var_quant_sqload_mfa_df <- as.data.frame(mfamix$sqload)
var_quant_sqload_mfa_df$Variable <- rownames(var_quant_sqload_mfa_df)


# Reshape from wide to long format for ggplot- cos2
var_quant_cos2_mfa_long <- var_quant_cos2_mfa_df %>%
  pivot_longer(cols = starts_with("dim"), 
               names_to = "Dimension", 
               values_to = "Squared_cosine")

# Create a bar plot for Dimension 1 - cos2
var_quant_cos2_mfa_long %>%
  filter(Dimension == "dim 1") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared cosines  - Dimension 1 (20.06 %)",
    x = "Variables (Quantitative)",
    y = "Squared cosine (quality of representation)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")


# Reshape from wide to long format for ggplot
var_quant_sqload_mfa_long <- var_quant_sqload_mfa_df %>%
  pivot_longer(cols = starts_with("dim"), 
               names_to = "Dimension", 
               values_to = "Squared_cosine")

# Create a bar plot for Dimension 1 - sqload
var_quant_sqload_mfa_long %>%
  filter(Dimension == "dim 1") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared loading  - Dimension 1 (20.06 %)",
    x = "Variables",
    y = "Squared loading (variable contributions)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 2 - cos2
var_quant_cos2_mfa_long %>%
  filter(Dimension == "dim 2") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared cosines  - Dimension 2 (11.43 %)",
    x = "Variables (Quantitative)",
    y = "Squared cosine (quality of representation)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 2 - sqload
var_quant_sqload_mfa_long %>%
  filter(Dimension == "dim 2") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared loading  - Dimension 2 (11.43 %)",
    x = "Variables",
    y = "Squared loading (variable contributions)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 3 - cos2
var_quant_cos2_mfa_long %>%
  filter(Dimension == "dim 3") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared cosines  - Dimension 3 (10.03 %)",
    x = "Variables (Quantitative)",
    y = "Squared cosine (quality of representation)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 3 - sqload
var_quant_sqload_mfa_long %>%
  filter(Dimension == "dim 3") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared loading  - Dimension 3 (10.03 %)",
    x = "Variables",
    y = "Squared loading (variable contributions)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 4 - cos2
var_quant_cos2_long %>%
  filter(Dimension == "dim 4") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared cosines  - Dimension 4 (9.19 %)",
    x = "Variables (Quantitative)",
    y = "Squared cosine (quality of representation)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Create a bar plot for Dimension 4 - sqload
var_quant_sqload_mfa_long %>%
  filter(Dimension == "dim 4") %>%
  ggplot(aes(x = reorder(Variable, Squared_cosine), y = Squared_cosine, fill = Squared_cosine)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Squared loading  - Dimension 4 (9.19 %)",
    x = "Variables",
    y = "Squared loading (variable contributions)"
  ) +
  theme_minimal() +
  scale_fill_viridis_c(guide = "none")

# Plot Dim 1 vs Dim 2 squared loadings
ggplot(var_quant_cos2_mfa_df , aes(x = `dim 1`, y = `dim 2`, label = Variable)) +
  geom_text_repel(max.overlaps = Inf, box.padding=0.5, size=4)+
  geom_point(color = "steelblue", size = 3) +
  xlim(0, 1) + ylim(0, 1) +
  labs(
    title = "",
    x = "Dimension 1 Squared cosine (20.06%)",
    y = "Dimension 2 Squared cosine (31.48%)"
  ) +
  theme_classic()

# Extract level coordinates for dimensions 1 and 2
levels_mfa_df <- as.data.frame(mfamix$levels$coord)
levels_mfa_df$Level <- rownames(levels_mfa_df)

# Extract percentage of variance explained for axis labels
eig <- mfamix$eig
pct1 <- round(eig[1, 2], 1)
pct2 <- round(eig[2, 2], 1)
pct3 <- round(eig[3, 2], 1)
pct4 <- round(eig[4, 2], 1)

# levels plot for dim 1 and dim 2
ggplot(levels_mfa_df, aes(x = `dim 1`, y = `dim 2`, label = Level)) +
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
ind_coords_mfa <- as.data.frame(mfamix$ind$coord)

# levels plot for dim 1 and dim 3
ggplot(levels_mfa_df, aes(x = `dim 1`, y = `dim 3`, label = Level)) +
  geom_point(color = "maroon", size = 2) +
  geom_text_repel(max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "PCAmix: Levels Component Map",
    x = paste0("Dimension 1 (", pct1, "%)"),
    y = paste0("Dimension 3 (", pct3, "%)")
  ) +
  theme_classic()

# levels plot for dim 2 and dim 3
ggplot(levels_mfa_df, aes(x = `dim 2`, y = `dim 3`, label = Level)) +
  geom_point(color = "maroon", size = 2) +
  geom_text_repel(max.overlaps = Inf, box.padding=0.5,size = 4)+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "PCAmix: Levels Component Map",
    x = paste0("Dimension 2 (", pct2, "%)"),
    y = paste0("Dimension 3 (", pct3, "%)")
  ) +
  theme_classic()

# Extract individual coordinates into a data frame
ind_coords_mfa <- as.data.frame(mfamix$ind$coord)


# Add metadata/labels for ggplot mapping (e.g., Label/Type)
wio_mpas_mfa_i<-wio_mpas %>% 
  mutate(index = row_number())
ind_coords_mfa$label <- wio_mpas_mfa_i$index
ind_coords_mfa$Country<-wio_mpas$COUNTRY
ind_coords_mfa$Cluster<- km$cluster
ind_coords_mfa$MPA_name<-wio_mpas$MPA_NAME

ind_coords_mfa$Cluster<-as.factor(ind_coords_mfa$Cluster)

ind_coords_mfa$`Protection level` <- wio_mpas_mfa$LEVEL # Extracting a qualitative variable for coloring
ind_coords_mfa$`Protection level`=ordered(ind_coords_mfa$`Protection level`,levels=c("Fully Protected","Lightly Protected","Minimally Protected / Incompatible","Unclassified"))
# Extract percentage of variance explained for axis labels
pc1_var <- round(mfamix$eig[1, 2], 1)
pc2_var <- round(mfamix$eig[2, 2], 1)
pc3_var <- round(mfamix$eig[3, 2], 1)

write_csv(ind_coords_mfa,"ind_coords_mfa_clust.csv")

# Plot 1 using ggplot2
ggplot(ind_coords_mfa, aes(x = `dim 1`, y = `dim 2`, color = `Protection level`, shape=Country)) +
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
ggplot(ind_coords_mfa, aes(x = `dim 1`, y = `dim 2`, color = `Protection level`, shape=Cluster)) +
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
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9,10,11,3,13,14,15))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 1 (", pc1_var, "%)"),
    y = paste0("Dimension 2 (", pc2_var, "%)")
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes


# Plot of dim 1 vs dim3
ggplot(ind_coords_mfa, aes(x = `dim 1`, y = `dim 3`, color = `Protection level`, shape=Cluster)) +
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
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9,10,11,3,13,14,15))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 1 (", pc1_var, "%)"),
    y = paste0("Dimension 3 (", pc3_var, "%)")
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes

# Plot of dim 2 vs dim3
ggplot(ind_coords_mfa, aes(x = `dim 2`, y = `dim 3`, color = `Protection level`, shape=Cluster)) +
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
  scale_shape_manual(values=c(1,2,0,4,5,6,12,8,9,10,11,3,13,14,15))+
  labs(
    title = "PCAmix Individuals Plot",
    x = paste0("Dimension 2 (", pc2_var, "%)"),
    y = paste0("Dimension 3 (", pc3_var, "%)")
  ) +
  theme_classic() +
  coord_fixed() # Preserves the aspect ratio of the orthogonal axes

# 3D scatter plot with a fourth dimension mapped to color
wio_mpas_3d<-scatter3D(x = ind_coords_mfa$`dim 1`, 
                       y = ind_coords_mfa$`dim 2`, 
                       z = ind_coords_mfa$`dim 3`, 
                       colvar = ind_coords$Cluster, # 4th dimension maps to color legend
                       pch = 18, 
                       ticktype = "detailed",
                       labels=ind_coords_mfa$label,
                       xlab="Dim 1",
                       ylab="Dim 2",
                       zlab="Dim 3")
