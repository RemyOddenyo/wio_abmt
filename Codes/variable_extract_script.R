library(sf)
library(terra)
library(readr)
library(dplyr)
library(tidyverse)
library(psych)
library(naniar)

rm(list=ls())

# Load MPA and land polygons
wiompas <- st_read("/Users/48483117/OneDrive - Macquarie University/Documents/wiompas_fin.shp")

wiompa_df <- wiompas %>%
  st_drop_geometry()
#wiompas$ID <- seq_len(nrow(wiompas))#  add an ID column
land<-st_read("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/gis/S2Coast_2023.shp")

land_transform<-st_transform(land, st_crs(wiompas_v)) # streamline projections
wio_bbox <- ext(-722533.6, 3953430, -5303506, -208260.7) # for cropping to wio region
land_transform<- st_crop(land_transform, wio_bbox) 

# Load critical habitat and coral reef fish layers
coral_reef<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/eco/coral_reef.tif")
seagrass_bed<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/eco/seagrass_bed.tif")
mangrove<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/eco/mangrove.tif")
coral_reef_fish<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/eco/coral_reef_fish.tif")
sharks<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/eco/sharks.tif")


#Load connectivity data 
wiompa_connect <- st_read("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/gis/Global_connectivity_active_connectance_shapefile/Global_connectivity_active_connectance.shp")

# Load environmental factors

chl <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/chl_baseline_2000_2018_depthsurf_91d8_fb73_4955_U1791756259567.nc")
no3 <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/no3_baseline_2000_2018_depthsurf_8486_b388_df7c_U1791756169968.nc")
o2 <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/o2_baseline_2000_2018_depthsurf_c6cd_716c_f3fc_U1791756200079.nc")
phyc <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/phyc_baseline_2000_2020_depthsurf_b57c_78d1_ca7a_U1791756235321.nc")
so <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/so_baseline_2000_2019_depthsurf_49ed_5fc4_602c_U1791756130515.nc")
thetao <- rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/data/environmental/thetao_baseline_2000_2019_depthsurf_74ff_39fa_9adc_U1791756072928.nc")

# Load human pressures
pop_dens<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/gpw-v4-population-density-adjusted-to-2015-unwpp-country-totals-rev11_2015_30_sec_tif/gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec.tif")
travel_time<-st_read("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/gis/Gravity/Global Gravity of Coral Reefs 2.0.shp")
rdi<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/povmap-grdi-v1-geotiff/povmap-grdi-v1.tif")
gravity<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/gis/wio_gravity_of_markets_maina/wio_gravity_of_markets_maina/logGRAV_WIO_7k.tif")
artisanal_fishing<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/artisanal_fishing.tif")
coastal_tourism<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/coastal_tourism.tif")
demersal_gillnet<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/demersal_gillnet.tif")
demersal_trawl<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/demersal_trawl.tif")
dredging<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/dredging.tif")
dumping<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/dumping.tif")
floating_longline<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/floating_longline.tif")
industrial_pollution<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/industrial_pollution.tif")
nutrient_input<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/nutrient_input.tif")
ocean_acidification<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/ocean_acidification.tif")
oil_gas_industry<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/oil_gas_industry.tif")
pelagic_seine<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/pelagic_seine.tif")
piers_and_docks<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/piers_and_docks.tif")
plastic_litter<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/plastic_litter.tif")
ports<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/ports.tif")
sea_level_rise<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/sea_level_rise.tif")
temperature_rise<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/temperature_rise.tif")
weighted_longline<-rast("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/Chapter_3_governance effectiveness at different scales in the SWIO/data/wiosym/pres/weighted_longline.tif")

#se gravity#se function
se <- function(x, na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

# Define a buffer distances vector
buffer_distance <- 35000

# Create polygon buffer using sf
buf_wiompas <- st_buffer(wiompas, dist = buffer_distance)

###~~~ connectivity matrix summaries for resident and cryptobenthic spawners


wiompa_connect_projected<- st_transform(wiompa_connect, crs(wiompas)) # streamline projections
wiompa_connect_projected<- st_crop(wiompa_connect_projected, wio_bbox) # crop to wio region


wiompa_connect_joined<- st_join(buf_wiompas, wiompa_connect_projected)

wiompa_connect_joined <- wiompa_connect_joined %>%
  st_drop_geometry()

#wiompa_connect_one_node<- wiompa_connect_joined[rownames(wiompa_connect_joined)  %in% c("15","18","21",),]

wiompa_connect_summary <- wiompa_connect_joined %>%
  st_drop_geometry() %>%
  group_by(ROW_ID) %>%
  summarise(
    point_count = n(),
    mean_ConnAct = mean(ConnAct, na.rm = TRUE), # mean
    sd_ConnAct   = sd(ConnAct, na.rm = TRUE),    # standard deviation
    se_ConnAct   = se(ConnAct, na.rm = TRUE),    # standard error
    med_ConnAct   = median(ConnAct, na.rm = TRUE),    #median
    sum_ConnAct  = sum(ConnAct, na.rm = TRUE),   # sum
    min_ConnAct  = min(ConnAct, na.rm = TRUE),    # minimum value
    max_ConnAct  = max(ConnAct, na.rm = TRUE),    # maximum value
    mean_ConnInAct = mean(ConnInAct, na.rm = TRUE), # mean
    sd_ConnInAct   = sd(ConnInAct, na.rm = TRUE),    # standard deviation
    se_ConnInAct   = se(ConnInAct, na.rm = TRUE),    # standard error
    med_ConnInAct   = median(ConnInAct, na.rm = TRUE),    #median
    sum_ConnInAct  = sum(ConnInAct, na.rm = TRUE),   # sum
    min_ConnInAct  = min(ConnInAct, na.rm = TRUE),    # minimum value
    max_ConnInAct  = max(ConnInAct, na.rm = TRUE),    # maximum value
    mean_ConnOtAct = mean(ConnOtAct, na.rm = TRUE), # mean
    sd_ConnOtAct   = sd(ConnOtAct, na.rm = TRUE),    # standard deviation
    se_ConnOtAct   = se(ConnOtAct, na.rm = TRUE),    # standard error
    med_ConnOtAct   = median(ConnOtAct, na.rm = TRUE),    #median
    sum_ConnOtAct  = sum(ConnOtAct, na.rm = TRUE),   # sum
    min_ConnOtAct  = min(ConnOtAct, na.rm = TRUE),    # minimum value
    max_ConnOtAct  = max(ConnOtAct, na.rm = TRUE)    # maximum value
  )
    

wiompa_connect_summary<- as.data.frame(wiompa_connect_summary)
wiompa_connect_means<-wiompa_connect_summary %>% select(starts_with("mean_"))

###~~~critical habitat and coral reef fish summaries

######~~~~~ coral_reef

coral_reef_projected<- project(coral_reef, crs(wiompas)) # streamline projections
coral_reef_projected<- crop(coral_reef_projected, wio_bbox) # crop to wio region

#Apply mask setting cells on land to NAs
coral_reef_masked <- mask(coral_reef_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_coral_reef <- terra::extract(coral_reef_masked, vect(buf_wiompas))
extract_coral_reef[extract_coral_reef == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

coral_reef_summary <- describeBy(extract_coral_reef,extract_coral_reef$ID, mat = T)

coral_reef_summary<- coral_reef_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("coral_reef_", .x)) %>% remove_rownames()

coral_reef_summary<- coral_reef_summary [,-c(1:3)]

str(coral_reef_summary)

coral_reef_mean<- coral_reef_summary [,2]

######~~~~~ seagrass_bed

seagrass_bed_projected<- project(seagrass_bed, crs(wiompas)) # streamline projections
seagrass_bed_projected<- crop(seagrass_bed_projected, wio_bbox) # crop to wio region

#Apply mask setting cells on land to NAs
seagrass_bed_masked <- mask(seagrass_bed_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_seagrass_bed <- terra::extract(seagrass_bed_masked, vect(buf_wiompas))
extract_seagrass_bed[extract_seagrass_bed == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

seagrass_bed_summary <- describeBy(extract_seagrass_bed,extract_seagrass_bed$ID, mat = T)

seagrass_bed_summary<- seagrass_bed_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("seagrass_bed_", .x)) %>% remove_rownames()

seagrass_bed_summary<- seagrass_bed_summary [,-c(1:3)]

seagrass_bed_mean<- seagrass_bed_summary [,2]


######~~~~~ mangrove

mangrove_projected<- project(mangrove, crs(wiompas)) # streamline projections
mangrove_projected<- crop(mangrove_projected, wio_bbox) # crop to wio region

#Apply mask setting cells on land to NAs
mangrove_masked <- mask(mangrove_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_mangrove <- terra::extract(mangrove_masked, vect(buf_wiompas))
extract_mangrove[extract_mangrove == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

mangrove_summary <- describeBy(extract_mangrove,extract_mangrove$ID, mat = T)

mangrove_summary<- mangrove_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("mangrove_", .x)) %>% remove_rownames()

mangrove_summary<- mangrove_summary [,-c(1:3)]

mangrove_mean<- mangrove_summary [,2]

######~~~~~ coral_reef_fish

coral_reef_fish_projected<- project(coral_reef_fish, crs(wiompas)) # streamline projections
coral_reef_fish_projected<- crop(coral_reef_fish_projected, wio_bbox) # crop to wio region

#Apply mask setting cells on land to NAs
coral_reef_fish_masked <- mask(coral_reef_fish_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_coral_reef_fish <- terra::extract(coral_reef_fish_masked, vect(buf_wiompas))
extract_coral_reef_fish[extract_coral_reef_fish == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

coral_reef_fish_summary <- describeBy(extract_coral_reef_fish,extract_coral_reef_fish$ID, mat = T)

coral_reef_fish_summary<- coral_reef_fish_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("coral_reef_fish_", .x)) %>% remove_rownames()

coral_reef_fish_summary<- coral_reef_fish_summary [,-c(1:3)]

coral_reef_fish_mean<- coral_reef_fish_summary [,2]

######~~~~~ sharks

sharks_projected<- project(sharks, crs(wiompas)) # streamline projections
sharks_projected<- crop(sharks_projected, wio_bbox) # crop to wio region

#Apply mask setting cells on land to NAs
sharks_masked <- mask(sharks_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_sharks <- terra::extract(sharks_masked, vect(buf_wiompas))
extract_sharks[extract_sharks == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

sharks_summary <- describeBy(extract_sharks,extract_sharks$ID, mat = T)

sharks_summary<- sharks_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("sharks_", .x)) %>% remove_rownames()

sharks_summary<- sharks_summary [,-c(1:3)]

sharks_mean<- sharks_summary [,2]

###~~~ Environmental factor summaries

######~~~~~ Chl-a

chl_projected<- project(chl, crs(wiompas)) # streamline projections
chl_projected<- crop(chl_projected, wio_bbox) # crop to wio region

#Apply mask setting cells on land to NAs
chl_masked <- mask(chl_projected, land_transform, inverse=TRUE)

  # Extract or crop/mask raster values within the buffer
  # Using terra::extract() with extract polygons
  extract_chl <- terra::extract(chl_masked, vect(buf_wiompas))
  extract_chl[extract_chl == -9999.900390625] <- NA 

  # Calculate summary statistics by MPA (row ID)
  
  chl_summary <- describeBy(extract_chl,extract_chl$ID, mat = T)
  
  chl_summary<- chl_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("chl_", .x)) %>% remove_rownames()
  
  chl_summary<- chl_summary [,-c(1:3)]
  
  chl_mean<- chl_summary [,2]
  
  
# Merge polygon attributes with the extract raster values
#wiompas_df_chl <- cbind(wiompas_df,  chl_summary)
  
  
######~~~~~ no3
no3_projected<- project(no3, crs(wiompas))
no3_projected<- crop(no3_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
no3_masked <- mask(no3_projected, land_transform, inverse=TRUE)


# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_no3 <- terra::extract(no3_masked, vect(buf_wiompas))
extract_no3[extract_no3 == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

no3_summary <- describeBy(extract_no3,extract_no3$ID, mat = T)

no3_summary<- no3_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("no3_", .x)) %>% remove_rownames()

no3_summary<- no3_summary [,-c(1:3)]

no3_mean<- no3_summary [,2]


######~~~~~ o2
o2_projected<- project(o2, crs(wiompas))
o2_projected<- crop(o2_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
o2_masked <- mask(o2_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_o2 <- terra::extract(o2_masked, vect(buf_wiompas))
extract_o2[extract_o2 == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

o2_summary <- describeBy(extract_o2,extract_o2$ID, mat = T)

o2_summary<- o2_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("o2_", .x)) %>% remove_rownames()

o2_summary<- o2_summary [,-c(1:3)]

o2_mean<- o2_summary [,2]


######~~~~~ phyc
phyc_projected<- project(phyc, crs(wiompas))
phyc_projected<- crop(phyc_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
phyc_masked <- mask(phyc_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_phyc <- terra::extract(phyc_masked, vect(buf_wiompas))
extract_phyc[extract_phyc == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

phyc_summary <- describeBy(extract_phyc,extract_phyc$ID, mat = T)

phyc_summary<- phyc_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("phyc_", .x)) %>% remove_rownames()

phyc_summary<- phyc_summary [,-c(1:3)]

phyc_mean<- phyc_summary [,2]


######~~~~~ so
so_projected<- project(so, crs(wiompas))
so_projected<- crop(so_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
so_masked <- mask(so_projected, land_transform, inverse=TRUE)
# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_so <- terra::extract(so_masked, vect(buf_wiompas))
extract_so[extract_so == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

so_summary <- describeBy(extract_so,extract_so$ID, mat = T)

so_summary<- so_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("so_", .x)) %>% remove_rownames()

so_summary<- so_summary [,-c(1:3)]

so_mean<- so_summary [,2]


######~~~~~ thetao
thetao_projected<- project(thetao, crs(wiompas))
thetao_projected<- crop(thetao_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
thetao_masked <- mask(thetao_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_thetao <- terra::extract(thetao_masked, vect(buf_wiompas))
extract_thetao[extract_thetao == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

thetao_summary <- describeBy(extract_thetao,extract_thetao$ID, mat = T)

thetao_summary<- thetao_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("thetao_", .x)) %>% remove_rownames()

thetao_summary<- thetao_summary [,-c(1:3)]

thetao_mean<- thetao_summary [,2]

###~~~ Human pressure summaries

######~~~~~ pop_dens (gpw_v4_population_density_adjusted_to_2015_unwpp_country_totals_rev11_2015_30_sec)
pop_dens_projected<- project(pop_dens, crs(wiompas))
pop_dens_projected<- crop(pop_dens_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
#pop_dens_masked <- mask(pop_dens_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_pop_dens <- terra::extract(pop_dens_projected, vect(buf_wiompas))
extract_pop_dens[extract_pop_dens == -9999.900390625] <- NA 

# Calculate summary statistics by MPA (row ID)

pop_dens_summary <- describeBy(extract_pop_dens,extract_pop_dens$ID, mat = T)

pop_dens_summary<- pop_dens_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("pop_dens_", .x)) %>% remove_rownames()

pop_dens_summary<- pop_dens_summary [,-c(1:3)]

pop_dens_mean<- pop_dens_summary [,2]


###~~~ travel time to nearest population and market

travel_time_transformed<- st_transform(travel_time, crs(wiompas)) # streamline projections
travel_time_transformed<- st_crop(travel_time_transformed, wio_bbox) # crop to wio region


travel_time_summary<- st_join(buf_wiompas, travel_time_transformed)


travel_time_summary <- travel_time_summary %>%
  st_drop_geometry() %>%
  group_by(ROW_ID) %>%
  summarise(
    polygon_count = n(),
    mean_TT_pop = mean(TT_pop, na.rm = TRUE), # mean
    sd_TT_pop   = sd(TT_pop, na.rm = TRUE),    # standard deviation
    se_TT_pop   = se(TT_pop, na.rm = TRUE),    # standard error
    med_TT_pop   = median(TT_pop, na.rm = TRUE),    #median
    sum_TT_pop  = sum(TT_pop, na.rm = TRUE),   # sum
    min_TT_pop  = min(TT_pop, na.rm = TRUE),    # minimum value
    max_TT_pop  = max(TT_pop, na.rm = TRUE),    # maximum value
    mean_TT_market = mean(TT_market, na.rm = TRUE), # mean
    sd_TT_market   = sd(TT_market, na.rm = TRUE),    # standard deviation
    se_TT_market   = se(TT_market, na.rm = TRUE),    # standard error
    med_TT_market   = median(TT_market, na.rm = TRUE),    #median
    sum_TT_market  = sum(TT_market, na.rm = TRUE),   # sum
    min_TT_market  = min(TT_market, na.rm = TRUE),    # minimum value
    max_TT_market  = max(TT_market, na.rm = TRUE)    # maximum value
)


travel_time_summary<- as.data.frame(travel_time_summary)
travel_time_means<-travel_time_summary %>% select(starts_with("mean_"))


######~~~~~ gravity
gravity_projected<- project(gravity, crs(wiompas))
gravity_projected<- crop(gravity_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
gravity_masked <- mask(gravity_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_gravity <- terra::extract(gravity_masked, vect(buf_wiompas))
extract_gravity[extract_gravity == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

gravity_summary <- describeBy(extract_gravity,extract_gravity$ID, mat = T)

gravity_summary<- gravity_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("gravity_", .x)) %>% remove_rownames()

gravity_summary<- gravity_summary [,-c(1:3)]

gravity_mean<- gravity_summary [,2]

######~~~~~ relative deprivation index (RDI)
rdi_projected<- project(rdi, crs(wiompas))
rdi_projected<- crop(rdi_projected, wio_bbox)

#crs(land_transform)

#Apply mask setting cells on land to NAs
#rdi_masked <- mask(rdi_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_rdi <- terra::extract(rdi_projected, vect(buf_wiompas))
extract_rdi[extract_rdi == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

rdi_summary <- describeBy(extract_rdi,extract_rdi$ID, mat = T)

rdi_summary<- rdi_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("rdi_", .x)) %>% remove_rownames()

rdi_summary<- rdi_summary [,-c(1:3)]

rdi_mean<- rdi_summary [,2]


######~~~~~ artisanal_fishing
artisanal_fishing_projected<- project(artisanal_fishing, crs(wiompas))
artisanal_fishing_projected<- crop(artisanal_fishing_projected, wio_bbox)

#Apply mask setting cells on land to NAs
artisanal_fishing_masked <- mask(artisanal_fishing_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_artisanal_fishing <- terra::extract(artisanal_fishing_masked, vect(buf_wiompas))
extract_artisanal_fishing[extract_artisanal_fishing == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

artisanal_fishing_summary <- describeBy(extract_artisanal_fishing,extract_artisanal_fishing$ID, mat = T)

artisanal_fishing_summary<- artisanal_fishing_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("artisanal_fishing_", .x)) %>% remove_rownames()

artisanal_fishing_summary<- artisanal_fishing_summary [,-c(1:3)]

artisanal_fishing_mean<- artisanal_fishing_summary [,2]

######~~~~~ coastal_tourism
coastal_tourism_projected<- project(coastal_tourism, crs(wiompas))
coastal_tourism_projected<- crop(coastal_tourism_projected, wio_bbox)

#Apply mask setting cells on land to NAs
coastal_tourism_masked <- mask(coastal_tourism_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_coastal_tourism <- terra::extract(coastal_tourism_masked, vect(buf_wiompas))
extract_coastal_tourism[extract_coastal_tourism == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

coastal_tourism_summary <- describeBy(extract_coastal_tourism,extract_coastal_tourism$ID, mat = T)

coastal_tourism_summary<- coastal_tourism_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("coastal_tourism_", .x)) %>% remove_rownames()

coastal_tourism_summary<- coastal_tourism_summary [,-c(1:3)]

coastal_tourism_mean<- coastal_tourism_summary [,2]

######~~~~~ demersal_gillnet
demersal_gillnet_projected<- project(demersal_gillnet, crs(wiompas))
demersal_gillnet_projected<- crop(demersal_gillnet_projected, wio_bbox)

#Apply mask setting cells on land to NAs
demersal_gillnet_masked <- mask(demersal_gillnet_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_demersal_gillnet <- terra::extract(demersal_gillnet_masked, vect(buf_wiompas))
extract_demersal_gillnet[extract_demersal_gillnet == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

demersal_gillnet_summary <- describeBy(extract_demersal_gillnet,extract_demersal_gillnet$ID, mat = T)

demersal_gillnet_summary<- demersal_gillnet_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("demersal_gillnet_", .x)) %>% remove_rownames()

demersal_gillnet_summary<- demersal_gillnet_summary [,-c(1:3)]

demersal_gillnet_mean<- demersal_gillnet_summary [,2]

######~~~~~ demersal_trawl
demersal_trawl_projected<- project(demersal_trawl, crs(wiompas))
demersal_trawl_projected<- crop(demersal_trawl_projected, wio_bbox)

#Apply mask setting cells on land to NAs
demersal_trawl_masked <- mask(demersal_trawl_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_demersal_trawl <- terra::extract(demersal_trawl_masked, vect(buf_wiompas))
extract_demersal_trawl[extract_demersal_trawl == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

demersal_trawl_summary <- describeBy(extract_demersal_trawl,extract_demersal_trawl$ID, mat = T)

demersal_trawl_summary<- demersal_trawl_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("demersal_trawl_", .x)) %>% remove_rownames()

demersal_trawl_summary<- demersal_trawl_summary [,-c(1:3)]

demersal_trawl_mean<- demersal_trawl_summary [,2]

######~~~~~ dredging
dredging_projected<- project(dredging, crs(wiompas))
dredging_projected<- crop(dredging_projected, wio_bbox)

#Apply mask setting cells on land to NAs
dredging_masked <- mask(dredging_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_dredging <- terra::extract(dredging_masked, vect(buf_wiompas))
extract_dredging[extract_dredging == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

dredging_summary <- describeBy(extract_dredging,extract_dredging$ID, mat = T)

dredging_summary<- dredging_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("dredging_", .x)) %>% remove_rownames()

dredging_summary<- dredging_summary [,-c(1:3)]

dredging_mean<- dredging_summary [,2]

######~~~~~ dumping
dumping_projected<- project(dumping, crs(wiompas))
dumping_projected<- crop(dumping_projected, wio_bbox)

#Apply mask setting cells on land to NAs
dumping_masked <- mask(dumping_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_dumping <- terra::extract(dumping_masked, vect(buf_wiompas))
extract_dumping[extract_dumping == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

dumping_summary <- describeBy(extract_dumping,extract_dumping$ID, mat = T)

dumping_summary<- dumping_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("dumping_", .x)) %>% remove_rownames()

dumping_summary<- dumping_summary [,-c(1:3)]

dumping_mean<- dumping_summary [,2]

######~~~~~ floating_longline
floating_longline_projected<- project(floating_longline, crs(wiompas))
floating_longline_projected<- crop(floating_longline_projected, wio_bbox)

#Apply mask setting cells on land to NAs
floating_longline_masked <- mask(floating_longline_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_floating_longline <- terra::extract(floating_longline_masked, vect(buf_wiompas))
extract_floating_longline[extract_floating_longline == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

floating_longline_summary <- describeBy(extract_floating_longline,extract_floating_longline$ID, mat = T)

floating_longline_summary<- floating_longline_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("floating_longline_", .x)) %>% remove_rownames()

floating_longline_summary<- floating_longline_summary [,-c(1:3)]

floating_longline_mean<- floating_longline_summary [,2]

######~~~~~ industrial_pollution
industrial_pollution_projected<- project(industrial_pollution, crs(wiompas))
industrial_pollution_projected<- crop(industrial_pollution_projected, wio_bbox)

#Apply mask setting cells on land to NAs
industrial_pollution_masked <- mask(industrial_pollution_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_industrial_pollution <- terra::extract(industrial_pollution_masked, vect(buf_wiompas))
extract_industrial_pollution[extract_industrial_pollution == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

industrial_pollution_summary <- describeBy(extract_industrial_pollution,extract_industrial_pollution$ID, mat = T)

industrial_pollution_summary<- industrial_pollution_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("industrial_pollution_", .x)) %>% remove_rownames()

industrial_pollution_summary<- industrial_pollution_summary [,-c(1:3)]

industrial_pollution_mean<- industrial_pollution_summary [,2]

######~~~~~ nutrient_input
nutrient_input_projected<- project(nutrient_input, crs(wiompas))
nutrient_input_projected<- crop(nutrient_input_projected, wio_bbox)

#Apply mask setting cells on land to NAs
nutrient_input_masked <- mask(nutrient_input_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_nutrient_input <- terra::extract(nutrient_input_masked, vect(buf_wiompas))
extract_nutrient_input[extract_nutrient_input == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

nutrient_input_summary <- describeBy(extract_nutrient_input,extract_nutrient_input$ID, mat = T)

nutrient_input_summary<- nutrient_input_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("nutrient_input_", .x)) %>% remove_rownames()

nutrient_input_summary<- nutrient_input_summary [,-c(1:3)]

nutrient_input_mean<- nutrient_input_summary [,2]

######~~~~~ ocean_acidification
ocean_acidification_projected<- project(ocean_acidification, crs(wiompas))
ocean_acidification_projected<- crop(ocean_acidification_projected, wio_bbox)

#Apply mask setting cells on land to NAs
ocean_acidification_masked <- mask(ocean_acidification_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_ocean_acidification <- terra::extract(ocean_acidification_masked, vect(buf_wiompas))
extract_ocean_acidification[extract_ocean_acidification == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

ocean_acidification_summary <- describeBy(extract_ocean_acidification,extract_ocean_acidification$ID, mat = T)

ocean_acidification_summary<- ocean_acidification_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("ocean_acidification_", .x)) %>% remove_rownames()

ocean_acidification_summary<- ocean_acidification_summary [,-c(1:3)]

ocean_acidification_mean<- ocean_acidification_summary [,2]

######~~~~~ oil_gas_industry
oil_gas_industry_projected<- project(oil_gas_industry, crs(wiompas))
oil_gas_industry_projected<- crop(oil_gas_industry_projected, wio_bbox)

#Apply mask setting cells on land to NAs
oil_gas_industry_masked <- mask(oil_gas_industry_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_oil_gas_industry <- terra::extract(oil_gas_industry_masked, vect(buf_wiompas))
extract_oil_gas_industry[extract_oil_gas_industry == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

oil_gas_industry_summary <- describeBy(extract_oil_gas_industry,extract_oil_gas_industry$ID, mat = T)

oil_gas_industry_summary<- oil_gas_industry_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("oil_gas_industry_", .x)) %>% remove_rownames()

oil_gas_industry_summary<- oil_gas_industry_summary [,-c(1:3)]

oil_gas_industry_mean<- oil_gas_industry_summary [,2]

######~~~~~ pelagic_seine
pelagic_seine_projected<- project(pelagic_seine, crs(wiompas))
pelagic_seine_projected<- crop(pelagic_seine_projected, wio_bbox)

#Apply mask setting cells on land to NAs
pelagic_seine_masked <- mask(pelagic_seine_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_pelagic_seine <- terra::extract(pelagic_seine_masked, vect(buf_wiompas))
extract_pelagic_seine[extract_pelagic_seine == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

pelagic_seine_summary <- describeBy(extract_pelagic_seine,extract_pelagic_seine$ID, mat = T)

pelagic_seine_summary<- pelagic_seine_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("pelagic_seine_", .x)) %>% remove_rownames()

pelagic_seine_summary<- pelagic_seine_summary [,-c(1:3)]

pelagic_seine_mean<- pelagic_seine_summary [,2]

######~~~~~ piers_and_docks
piers_and_docks_projected<- project(piers_and_docks, crs(wiompas))
piers_and_docks_projected<- crop(piers_and_docks_projected, wio_bbox)

#Apply mask setting cells on land to NAs
piers_and_docks_masked <- mask(piers_and_docks_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_piers_and_docks <- terra::extract(piers_and_docks_masked, vect(buf_wiompas))
extract_piers_and_docks[extract_piers_and_docks == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

piers_and_docks_summary <- describeBy(extract_piers_and_docks,extract_piers_and_docks$ID, mat = T)

piers_and_docks_summary<- piers_and_docks_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("piers_and_docks_", .x)) %>% remove_rownames()

piers_and_docks_summary<- piers_and_docks_summary [,-c(1:3)]

piers_and_docks_mean<- piers_and_docks_summary [,2]


######~~~~~ plastic_litter
plastic_litter_projected<- project(plastic_litter, crs(wiompas))
plastic_litter_projected<- crop(plastic_litter_projected, wio_bbox)

#Apply mask setting cells on land to NAs
plastic_litter_masked <- mask(plastic_litter_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_plastic_litter <- terra::extract(plastic_litter_masked, vect(buf_wiompas))
extract_plastic_litter[extract_plastic_litter == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

plastic_litter_summary <- describeBy(extract_plastic_litter,extract_plastic_litter$ID, mat = T)

plastic_litter_summary<- plastic_litter_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("plastic_litter_", .x)) %>% remove_rownames()

plastic_litter_summary<- plastic_litter_summary [,-c(1:3)]

plastic_litter_mean<- plastic_litter_summary [,2]

######~~~~~ ports
ports_projected<- project(ports, crs(wiompas))
ports_projected<- crop(ports_projected, wio_bbox)

#Apply mask setting cells on land to NAs
ports_masked <- mask(ports_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_ports <- terra::extract(ports_masked, vect(buf_wiompas))
extract_ports[extract_ports == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

ports_summary <- describeBy(extract_ports,extract_ports$ID, mat = T)

ports_summary<- ports_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("ports_", .x)) %>% remove_rownames()

ports_summary<- ports_summary [,-c(1:3)]

ports_mean<- ports_summary [,2]

######~~~~~ sea_level_rise
sea_level_rise_projected<- project(sea_level_rise, crs(wiompas))
sea_level_rise_projected<- crop(sea_level_rise_projected, wio_bbox)

#Apply mask setting cells on land to NAs
sea_level_rise_masked <- mask(sea_level_rise_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_sea_level_rise <- terra::extract(sea_level_rise_masked, vect(buf_wiompas))
extract_sea_level_rise[extract_sea_level_rise == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

sea_level_rise_summary <- describeBy(extract_sea_level_rise,extract_sea_level_rise$ID, mat = T)

sea_level_rise_summary<- sea_level_rise_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("sea_level_rise_", .x)) %>% remove_rownames()

sea_level_rise_summary<- sea_level_rise_summary [,-c(1:3)]

sea_level_rise_mean<- sea_level_rise_summary [,2]

######~~~~~ temperature_rise
temperature_rise_projected<- project(temperature_rise, crs(wiompas))
temperature_rise_projected<- crop(temperature_rise_projected, wio_bbox)

#Apply mask setting cells on land to NAs
temperature_rise_masked <- mask(temperature_rise_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_temperature_rise <- terra::extract(temperature_rise_masked, vect(buf_wiompas))
extract_temperature_rise[extract_temperature_rise == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

temperature_rise_summary <- describeBy(extract_temperature_rise,extract_temperature_rise$ID, mat = T)

temperature_rise_summary<- temperature_rise_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("temperature_rise_", .x)) %>% remove_rownames()

temperature_rise_summary<- temperature_rise_summary [,-c(1:3)]

temperature_rise_mean<-temperature_rise_summary [,2]

######~~~~~ weighted_longline
weighted_longline_projected<- project(weighted_longline, crs(wiompas))
weighted_longline_projected<- crop(weighted_longline_projected, wio_bbox)

#Apply mask setting cells on land to NAs
weighted_longline_masked <- mask(weighted_longline_projected, land_transform, inverse=TRUE)

# Extract or crop/mask raster values within the buffer
# Using terra::extract() with extract polygons
extract_weighted_longline <- terra::extract(weighted_longline_masked, vect(buf_wiompas))
extract_weighted_longline[extract_weighted_longline == -9999.900390625] <- NA 


# Calculate summary statistics by MPA (row ID)

weighted_longline_summary <- describeBy(extract_weighted_longline,extract_weighted_longline$ID, mat = T)

weighted_longline_summary<- weighted_longline_summary %>%  filter(vars=="2") %>% rename_with(~ paste0("weighted_longline_", .x)) %>% remove_rownames()

weighted_longline_summary<- weighted_longline_summary [,-c(1:3)]

weighted_longline_mean<-weighted_longline_summary [,2]

# Bind variable summaries to polygon attributes


wiompas_df_var_summaries<- cbind(wiompas_df,coral_reef_summary,seagrass_bed_summary,mangrove_summary,coral_reef_fish_summary,wiompa_connect_summary, sharks_summary,  
                       chl_summary,no3_summary,o2_summary,phyc_summary,so_summary,thetao_summary,rdi_summary,pop_dens_summary,travel_time_summary,
                       gravity_summary,artisanal_fishing_summary,coastal_tourism_summary,demersal_gillnet_summary,
                       demersal_trawl_summary,dredging_summary,dumping_summary, floating_longline_summary,
                       industrial_pollution_summary,nutrient_input_summary,ocean_acidification_summary,oil_gas_industry_summary,
                       pelagic_seine_summary,piers_and_docks_summary,plastic_litter_summary,ports_summary,sea_level_rise_summary,
                       temperature_rise_summary, temperature_rise_summary, weighted_longline_summary)

# Save and inspect summaries
wiompas_df_var_summaries<-as.data.frame(wiompas_df_var_summaries)
write.csv(wiompas_df_var_summaries,"wiompas_df_var_summaries.csv")

# Bind variable means to polygon attributes

wiompas_df_var_means<- cbind(wiompas_df,coral_reef_mean,seagrass_bed_mean,mangrove_mean,coral_reef_fish_mean,wiompa_connect_means, sharks_mean,    
                                 chl_mean,no3_mean,o2_mean,phyc_mean,so_mean,thetao_mean,rdi_mean,pop_dens_mean,travel_time_means,
                                 gravity_mean,artisanal_fishing_mean,coastal_tourism_mean,demersal_gillnet_mean,
                                 demersal_trawl_mean,dredging_mean,dumping_mean, floating_longline_mean,
                                 industrial_pollution_mean,nutrient_input_mean,ocean_acidification_mean,oil_gas_industry_mean,
                                 pelagic_seine_mean,piers_and_docks_mean,plastic_litter_mean,ports_mean,sea_level_rise_mean,
                                 temperature_rise_mean,  weighted_longline_mean)

# Save and inspect means
write.csv(wiompas_df_var_means,"wiompas_df_var_means.csv")

# Bind internal and contextual enabling conditions to polygon and variable mean data frame

wiompas_cond <- read_csv("wiompa_internal_contextual_conditions.csv")
wiompas_cond_2 <- wiompas_cond [,-c(2:5,55,62,79:85,87:93,95:101,103:109,111:117,119:125,129:131)]
wiompas_df_var_means_2 <- wiompas_df_var_means [,-c(14)]
wiompas_df_var_means_cond <- left_join(wiompas_df_var_means_2, wiompas_cond_2, by = "ID")


# Update year of establishment for Parc National Mitsamiouli Ndroude from UNEP-WCMC (2026). Protected Area Profile for Parc National Mitsamiouli Ndroude from the World Database on Protected Areas, November 2026. Available at: www.protectedplanet.net
wiompas_df_var_means_cond <- wiompas_df_var_means_cond%>% 
  mutate(YEAR = ifelse(YEAR=="Not available", 2010, YEAR)) %>%
  mutate(YEAR=as.numeric(YEAR))

wiompas_df_var_means_cond<- wiompas_df_var_means_cond %>% 
  mutate(YEAR = 2018 - YEAR)  %>% rename(AGE=YEAR)



# Save and inspect full matrix
write_csv(wiompas_df_var_means_cond,"wiompas_df_var_means_cond.csv")

# evaluate NAs

### by variable
wio_mpa_df_var_NAs<-miss_var_summary(wiompas_df_var_means_cond)

wio_mpa_df_var_NAs_df<-as.data.frame(wio_mpa_df_var_NAs)

write_csv(wio_mpa_df_var_NAs_df,"wio_mpa_df_var_NAs_df.csv")

### by MPA
wio_mpa_df_case_NAs<-miss_case_summary(wiompas_df_var_means_cond)

wio_mpa_df_case_NAs_df<-as.data.frame(wio_mpa_df_case_NAs)

wiompas_NAMES<-wiompas_df_var_means_cond[,c(3,4,23)] #subset MPA names

wio_mpa_df_case_NAs_df_2 <- wio_mpa_df_case_NAs_df %>% 
  left_join(wiompas_NAMES, by = join_by(case == ROW_ID))


write_csv(wio_mpa_df_case_NAs_df_2,"wio_mpa_NAs_df.csv")


# Subset matrix in preparation for PCA analysis

wio_mpa_df<- wiompas_df_var_means_cond [,-c(5:9,11:14,16:20,23,82:95,139)]

swio_mpa_df<- wiompas_df_var_means_cond [,-c(5:9,11:14,16:20,23,28:30,40:42,82:95,139)]

wio_mpas_matrix <- na.omit(wio_mpa_df)

swio_mpas_matrix <- na.omit(swio_mpa_df)

#colnames(wio_mpa_df)


# Save and inspect matrix subsets 
write_csv(wio_mpas_matrix,"wio_mpas_matrix.csv")
write_csv(swio_mpas_matrix,"swio_mpas_matrix.csv")

# Create martix look up data frames

lookup_df <- data.frame(
  category = c("demographics", "demographics", "grades", "grades", "engagement"),
  col_name = c("student_id", "first_name", "math_score", "sci_score", "attendance_pct")
)
