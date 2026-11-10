library(sf)
library(dplyr)
library(fuzzyjoin)

# Load WIO MPA polygons
wiompa_sf <- st_read("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/analysis/r/wio_abmt/gis/WIOMPAS_All_fin/WIOMPAS_All_fin.shp")

# Select national MPAs

wiompa_sf_nat <- wiompa_sf %>% 
  filter(CATEGORY2 == "National")

wiompa_sf_nat_df <- st_drop_geometry(wiompa_sf_nat) # drop geometry

write.csv(wiompa_sf_nat_df,"wiompa_sf_nat_df.csv")

# Load WIO MPAs surveyed using METT
wiompas_survyed <- read.csv("/Users/48483117/OneDrive - Macquarie University/Remy's PhD/chapters/chapter_3_governance effectiveness at different scales in the SWIO/analysis/r/wio_abmt/gis/wiompas_surveyed.csv")

# Perform regex join based on a regular expression in one column matching the other
regex_wiompas<-regex_inner_join(wiompa_sf_nat, wiompas_survyed, by = c("MPA_NAME" = "MPA_NAME_WIOMETT_trunk"), ignore_case=TRUE)

# Drop rows with WIO survey duplicates 
wiompas_clean<-regex_wiompas[-c(2, 6, 31, 46,56,58,87,88,97,99,100),] # MPAs maintained and fishing reserves dropped 

# Subset surveyed MPAs without shape files version 1

wiompas_no_sf<-wiompas_survyed %>% anti_join(wiompas_clean , by = "ID")

# Perform string join on WIO MPA polygons and survey MPA subset without shapefiles 
string_wiompas_with_sf<-stringdist_join(
  wiompa_sf_nat, wiompas_no_sf, 
  by = c("MPA_NAME" = "MPA_NAME_WIOMETT_trunk"), 
  method = "qgram",      # Splits strings into q-character blocks (grams) and counts absolute differences in their frequencies.
  ignore_case = TRUE,     # Match regardless of capitalization
  distance_col = "score"  # Creates a column showing the match distance
)

# Subset surveyed MPAs without shape files version 2
wiompas_no_sf_2<-wiompas_no_sf %>% anti_join(string_wiompas_with_sf , by = "ID")

# Combine MPA polygons from regex and string joins to one multypolygon without dissolving boundaries
wiompas_clean<-st_as_sf(wiompas_clean)
string_wiompas_with_sf<-st_as_sf(string_wiompas_with_sf)

wiompas_fin <- bind_rows(wiompas_clean,string_wiompas_with_sf)

wiompas_fin <- wiompas_fin [,-c(22,23,26)] #drop truncated name, country, and score columns

wiompas_fin <- wiompas_fin %>%    # rename columns
  rename(NAME_METT = MPA_NAME_WIOMETT,
         STAGE  = establishment_stage,
         LEVEL = protection_level)

str(wiompas_fin)

plot(st_geometry(wiompas_fin), col = "lightblue", border = "darkgray")

st_write(wiompas_fin, "/Users/48483117/OneDrive - Macquarie University/Documents/wiompas_fin.shp")


