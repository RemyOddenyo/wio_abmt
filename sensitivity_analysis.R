library(sf)
library(terra)
library(readr)
library(dplyr)
rm(list=ls())

# Load polygon and raster data 
wiompas <- st_read("/Users/48483117/OneDrive - Macquarie University/Documents/wiompas_fin.shp")

wio_bbox <- ext(-722533.6, 3953430, -5303506, -208260.7)

land<-st_read("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/gis/S2Coast_2023.shp")
land_transform<-st_transform(land, st_crs(wiompas))
land_transform<- st_crop(land_transform, wio_bbox)

######~~~~~ Chl-a

chl <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/chl_baseline_2000_2018_depthsurf_91d8_fb73_4955_U1791756259567.nc")
chl_projected<- project(chl, crs(wiompas))
chl_projected<- crop(chl_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
chl_masked <- mask(chl_projected, land_transform, inverse=TRUE)

# Define a vector of buffer distances to test
buffer_distances <- seq(0, 600000, by = 10000)

# Initialize a results data frame to store sensitivity metrics
chl_results <- data.frame(
  buffer_dist = buffer_distances,
  mean_val = NA_real_,
  sum_val = NA_real_,
  cell_count = NA_integer_
)

# Run the loop for sensitivity analysis
for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create polygon buffer using sf
  buf_wiompas <- st_buffer(wiompas, dist = dist)
  
  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extracted polygons
  extracted_chl <- extract(chl_masked, vect(buf_wiompas))
  extracted_chl[extracted_chl == -9999.900390625] <- NA 
  
  # Select values from primary data column 
  val_col <- names(extracted_chl)[2]
  vals <- extracted_chl[[val_col]]
  
  
  # Calculate summary metrics of interest
  chl_results$mean_val[i] <- mean(vals, na.rm = TRUE)
  chl_results$sd_val[i] <- sd(vals, na.rm = TRUE)
  chl_results$med_val[i] <- median(vals, na.rm = TRUE)
  chl_results$sum_val[i] <- sum(vals, na.rm = TRUE)
  chl_results$cell_count[i] <- sum(!is.na(vals))
}

# View the sensitivity analysis output
print(chl_results)

# Plot the sensitivity curve
plot(chl_results$buffer_dist, chl_results$mean_val, type = "b", pch = 19,
     xlab = "Buffer Distance (m)", ylab = "Mean Chl-a Value",
     main = "Sensitivity Analysis of Buffer Distance on Chl-a Extraction")

######~~~~~ no3
no3 <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/no3_baseline_2000_2018_depthsurf_8486_b388_df7c_U1791756169968.nc")
no3_projected<- project(no3, crs(wiompas))
no3_projected<- crop(no3_projected, wio_bbox)

#crs(land_transform, inverse=TRUE)

#Apply mask setting cells on land to NAs
no3_masked <- mask(no3_projected, land_transform, inverse=TRUE)

# Define a vector of buffer distances to test
buffer_distances <- seq(0, 600000, by = 10000)

# Initialize a results data frame to store sensitivity metrics
no3_results <- data.frame(
  buffer_dist = buffer_distances,
  mean_val = NA_real_,
  sum_val = NA_real_,
  cell_count = NA_integer_
)

# Run the loop for sensitivity analysis
for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create polygon buffer using sf
  buf_wiompas <- st_buffer(wiompas, dist = dist)
  
  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extracted polygons
  extracted_no3 <- extract(no3_masked, vect(buf_wiompas))
  extracted_no3[extracted_no3 == -9999.900390625] <- NA
  
   # Select values from primary data column 
  val_col <- names(extracted_no3)[2]
  vals <- extracted_no3[[val_col]]
  
  
  # Calculate summary metrics of interest
  no3_results$mean_val[i] <- mean(vals, na.rm = TRUE)
  no3_results$sd_val[i] <- sd(vals, na.rm = TRUE)
  no3_results$med_val[i] <- median(vals, na.rm = TRUE)
  no3_results$sum_val[i] <- sum(vals, na.rm = TRUE)
  no3_results$cell_count[i] <- sum(!is.na(vals))
}

# View the sensitivity analysis output
print(no3_results)

# Plot the sensitivity curve
plot(no3_results$buffer_dist, no3_results$mean_val, type = "b", pch = 19,
     xlab = "Buffer Distance (m)", ylab = "Mean no3 Value",
     main = "Sensitivity Analysis of Buffer Distance on no3 Extraction")



######~~~~~ o2
o2 <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/o2_baseline_2000_2018_depthsurf_c6cd_716c_f3fc_U1791756200079.nc")
o2_projected<- project(o2, crs(wiompas))
o2_projected<- crop(o2_projected, wio_bbox)

#crs(land_transform, inverse)

#Apply mask setting cells on land to NAs
o2_masked <- mask(o2_projected, land_transform, inverse=TRUE)

# Define a vector of buffer distances to test
buffer_distances <- seq(0, 600000, by = 10000)

# Initialize a results data frame to store sensitivity metrics
o2_results <- data.frame(
  buffer_dist = buffer_distances,
  mean_val = NA_real_,
  sum_val = NA_real_,
  cell_count = NA_integer_
)

# Run the loop for sensitivity analysis
for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create polygon buffer using sf
  buf_wiompas <- st_buffer(wiompas, dist = dist)
  
  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extracted polygons
  extracted_o2 <- extract(o2_masked, vect(buf_wiompas))
  extracted_o2[extracted_o2 == -9999.900390625] <- NA
  
   # Select values from primary data column 
  val_col <- names(extracted_o2)[2]
  vals <- extracted_o2[[val_col]]
  
  
  # Calculate summary metrics of interest
  o2_results$mean_val[i] <- mean(vals, na.rm = TRUE)
  o2_results$sd_val[i] <- sd(vals, na.rm = TRUE)
  o2_results$med_val[i] <- median(vals, na.rm = TRUE)
  o2_results$sum_val[i] <- sum(vals, na.rm = TRUE)
  o2_results$cell_count[i] <- sum(!is.na(vals))
}

# View the sensitivity analysis output
print(o2_results)

# Plot the sensitivity curve
plot(o2_results$buffer_dist, o2_results$mean_val, type = "b", pch = 19,
     xlab = "Buffer Distance (m)", ylab = "Mean o2 Value",
     main = "Sensitivity Analysis of Buffer Distance on o2 Extraction")


######~~~~~ phyc
phyc <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/phyc_baseline_2000_2020_depthsurf_b57c_78d1_ca7a_U1791756235321.nc")
phyc_projected<- project(phyc, crs(wiompas))
phyc_projected<- crop(phyc_projected, wio_bbox)

#crs(land_transform, inverse)

#Apply mask setting cells on land to NAs
phyc_masked <- mask(phyc_projected, land_transform, inverse=TRUE)

# Define a vector of buffer distances to test
buffer_distances <- seq(0, 600000, by = 10000)

# Initialize a results data frame to store sensitivity metrics
phyc_results <- data.frame(
  buffer_dist = buffer_distances,
  mean_val = NA_real_,
  sum_val = NA_real_,
  cell_count = NA_integer_
)

# Run the loop for sensitivity analysis
for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create polygon buffer using sf
  buf_wiompas <- st_buffer(wiompas, dist = dist)
  
  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extracted polygons
  extracted_phyc <- extract(phyc_masked, vect(buf_wiompas))
  extracted_phyc[extracted_phyc == -9999.900390625] <- NA
  
   # Select values from primary data column 
  val_col <- names(extracted_phyc)[2]
  vals <- extracted_phyc[[val_col]]
  
  
  # Calculate summary metrics of interest
  phyc_results$mean_val[i] <- mean(vals, na.rm = TRUE)
  phyc_results$sd_val[i] <- sd(vals, na.rm = TRUE)
  phyc_results$med_val[i] <- median(vals, na.rm = TRUE)
  phyc_results$sum_val[i] <- sum(vals, na.rm = TRUE)
  phyc_results$cell_count[i] <- sum(!is.na(vals))
}

# View the sensitivity analysis output
print(phyc_results)

# Plot the sensitivity curve
plot(phyc_results$buffer_dist, phyc_results$mean_val, type = "b", pch = 19,
     xlab = "Buffer Distance (m)", ylab = "Mean phyc Value",
     main = "Sensitivity Analysis of Buffer Distance on phyc Extraction")

######~~~~~ so
so <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/so_baseline_2000_2019_depthsurf_49ed_5fc4_602c_U1791756130515.nc")
so_projected<- project(so, crs(wiompas))
so_projected<- crop(so_projected, wio_bbox)

#crs(land_transform, inverse)

#Apply mask setting cells on land to NAs
so_masked <- mask(so_projected, land_transform, inverse=TRUE)

# Define a vector of buffer distances to test
buffer_distances <- seq(0, 600000, by = 10000)

# Initialize a results data frame to store sensitivity metrics
so_results <- data.frame(
  buffer_dist = buffer_distances,
  mean_val = NA_real_,
  sum_val = NA_real_,
  cell_count = NA_integer_
)

# Run the loop for sensitivity analysis
for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create polygon buffer using sf
  buf_wiompas <- st_buffer(wiompas, dist = dist)
  
  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extracted polygons
  extracted_so <- extract(so_masked, vect(buf_wiompas))
  extracted_so[extracted_so == -9999.900390625] <- NA
  
   # Select values from primary data column 
  val_col <- names(extracted_so)[2]
  vals <- extracted_so[[val_col]]
  
  
  # Calculate summary metrics of interest
  so_results$mean_val[i] <- mean(vals, na.rm = TRUE)
  so_results$sd_val[i] <- sd(vals, na.rm = TRUE)
  so_results$med_val[i] <- median(vals, na.rm = TRUE)
  so_results$sum_val[i] <- sum(vals, na.rm = TRUE)
  so_results$cell_count[i] <- sum(!is.na(vals))
}

# View the sensitivity analysis output
print(so_results)

# Plot the sensitivity curve
plot(so_results$buffer_dist, so_results$mean_val, type = "b", pch = 19,
     xlab = "Buffer Distance (m)", ylab = "Mean so Value",
     main = "Sensitivity Analysis of Buffer Distance on so Extraction")


######~~~~~ thetao
thetao <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/thetao_baseline_2000_2019_depthsurf_74ff_39fa_9adc_U1791756072928.nc")
thetao_projected<- project(thetao, crs(wiompas))
thetao_projected<- crop(thetao_projected, wio_bbox)

#crs(land_transform, inverse)

#Apply mask setting cells on land to NAs
thetao_masked <- mask(thetao_projected, land_transform, inverse=TRUE)

# Define a vector of buffer distances to test
buffer_distances <- seq(0, 600000, by = 10000)

# Initialize a results data frame to store sensitivity metrics
thetao_results <- data.frame(
  buffer_dist = buffer_distances,
  mean_val = NA_real_,
  sum_val = NA_real_,
  cell_count = NA_integer_
)

# Run the loop for sensitivity analysis
for (i in seq_along(buffer_distances)) {
  dist <- buffer_distances[i]
  
  # Create polygon buffer using sf
  buf_wiompas <- st_buffer(wiompas, dist = dist)
  
  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extracted polygons
  extracted_thetao <- extract(thetao_masked, vect(buf_wiompas))
  extracted_thetao[extracted_thetao == -9999.900390625] <- NA
  
   # Select values from primary data column 
  val_col <- names(extracted_thetao)[2]
  vals <- extracted_thetao[[val_col]]
  
  
  # Calculate summary metrics of interest
  thetao_results$mean_val[i] <- mean(vals, na.rm = TRUE)
  thetao_results$sd_val[i] <- sd(vals, na.rm = TRUE)
  thetao_results$med_val[i] <- median(vals, na.rm = TRUE)
  thetao_results$sum_val[i] <- sum(vals, na.rm = TRUE)
  thetao_results$cell_count[i] <- sum(!is.na(vals))
}

# View the sensitivity analysis output
print(thetao_results)

# Plot the sensitivity curve
plot(thetao_results$buffer_dist, thetao_results$mean_val, type = "b", pch = 19,
     xlab = "Buffer Distance (m)", ylab = "Mean thetao Value",
     main = "Sensitivity Analysis of Buffer Distance on thetao Extraction")


