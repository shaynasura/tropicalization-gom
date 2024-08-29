## playing around to make sure things work before committing changes to other files


library(geosphere)

# source custom functions
source("R_scripts/functions/fish_calculate_gCOB.R")
source("R_scripts/functions/test_gCOBs_over_time.R")
source("R_scripts/functions/combine_and_modify_models.R")
source("R_scripts/functions/filter_gCOB_dataframes.R")
source("R_scripts/functions/plot_lat_long_gCOB_data.R")
source("R_scripts/functions/calculate_gCOB_movement.R")





# subset the data for the target years and months
years_to_include <- c(2010:2022)
months_to_include <- c(10,11,12)

bio_data_clean_yr2010_2022_m101112 <- bio_data_clean_test %>% ## CHANGE THIS BACK TO bio_data_clean AFTER TESTING
  filter(YR %in% years_to_include) %>% 
  mutate(MONTH = month(dmy(MO_DAY_YR))) %>% 
  filter(MONTH %in% months_to_include)




## species list for 2010 - 2022 fall months (10,11,12) data only
species_df_yr2010_2022_m101112 <- bio_data_clean_yr2010_2022_m101112 %>% 
  select(new_TAXON, RANK) %>% 
  filter(RANK == "SPECIES") %>% 
  distinct(new_TAXON, .keep_all = TRUE) %>% 
  arrange(new_TAXON)

# dim(species_df_yr2010_2022_m101112) # 500 unique taxonomic names
species_vec_yr2010_2022_m101112 <- species_df_yr2010_2022_m101112$new_TAXON
num_species <- length(species_vec_yr2010_2022_m101112) # 500 unique taxonomic names




# Step 1 - use fish_calculate_gCOB function
# Applying the gCOB calculate function to each unique species for the dataset only containing 2010 - 2022 data
# create an empty list to store cg dataframe
gCOBs_yr2010_2022_m101112 <- list()

# Apply the fish_calculate_gCOB function to each unique species and combine results into a list
result_list_yr2010_2022_m101112 <- purrr::map(species_vec_yr2010_2022_m101112, ~fish_calculate_gCOB(data = bio_data_clean_yr2010_2022_m101112,
                                                                                                    output_name = "gCOBs_yr2010_2022_m101112",
                                                                                                    taxonomic_column = "new_TAXON",
                                                                                                    common_name_column = "new_TAXON",
                                                                                                    species_name = .))


# Step 2 - filter to only include fish species with 5+ gCOB observations
# Filter the list of data frames
gCOBs_yr2010_2022_m101112 <- filter_gCOB_dataframes(gCOBs_yr2010_2022_m101112, min_years = 5)

# get updated list of fish species names
species_vec_yr2010_2022_m101112 <- gsub("_gCOB", "", names(gCOBs_yr2010_2022_m101112))

# get number of fish species in filtered dataset
filtered_num_species <- length(species_vec_yr2010_2022_m101112)


### NEW BELOW HERE ####
# Step 3 - calculate change in gCOBs over time in terms of bearing and distance

#initiate empty list
movement_results <- list()

gCOBs_data <- get("gCOBs_yr2010_2022_m101112", envir = .GlobalEnv)
movement_results <- calculate_all_gCOB_movements(gCOBs_data)




################### WIND ROSE PLOT CODE FOR SHOWING DISTANCE ALONG PETALS ############################

# join climate zone information to gCOB_trends
climate_gCOB_movements <- movement_results %>%
  rename(species_name = species) %>%
  left_join(species_climate_df, join_by(species_name)) %>% 
  rename(species = species_name)


# Create the wind rose plot
ggplot(data = climate_gCOB_movements, aes(x = factor(round(bearing,0)), y = distance,
                                          fill = climate_zone,
                                          group = climate_zone)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(start = -pi/16) +  # Adjust start for orientation
  scale_fill_manual(values = c("tropical" = "coral", "temperate" = "skyblue", "subtropical" = "lightgreen", "deep-water" = "navy")) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(size = 12, vjust = 0.5),
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        legend.position = "right") +
  labs(fill = "Climate Distribution") +
  ggtitle("Wind Rose Plot with Distance and Climate Distribution") +
  facet_wrap(~climate_zone, scales = "free_y")

#################################################################################################################








### THIS CHUNK OF CODE WORKS FOR MAKING THE PLOTS ####

# Bin the bearing values and aggregate data
binned_climate_gCOBs <- climate_gCOB_movements %>% 
  mutate(bearing_bins = cut(bearing, breaks = seq(0, 360, length.out = 37), include.lowest = TRUE)) %>% 
  group_by(climate_zone, bearing_bins) %>% 
  summarize(num_species = n(), .groups = 'drop') %>% 
  ungroup()

# Extract labels for bins based on their midpoints
bin_labels <- seq(0, 360, by = 10) # Midpoints of 36 bins (0-10, 10-20, ..., 350-360)

# sequence of labels that are not every 10 degrees
less_bin_labels <- c("0", rep("",8), "90", rep("",8), "180", rep("",8), "270", rep("",8))

# Ensure the correct order of bearing bins
binned_climate_gCOBs <- binned_climate_gCOBs %>%
  mutate(bearing_bins = factor(bearing_bins,
                               levels = levels(bearing_bins),
                               labels = bin_labels[1:36]))


# make plot
ggplot(data = binned_climate_gCOBs, aes(x = bearing_bins, y = num_species,
                                        fill = climate_zone,
                                        group = climate_zone)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "x", start = -pi/32, direction = 1) +   # adjust start for orientation
  scale_fill_manual(values = c("tropical" = "coral", "temperate" = "skyblue", "subtropical" = "lightgreen", "deep-water" = "navy")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(drop = FALSE,
    # limits = as.character(bin_labels[1:36]),
    # limits = levels(bearing_bins),
                   # labels = bin_labels[1:36],    # Apply bin midpoints as labels
                   labels = less_bin_labels,    # less labels
                   expand = c(0,0.5)
                   ) +  
  geom_text(aes(label = num_species), position = position_stack(vjust = 0.9), size = 3) +  # Add labels to petals
  # theme_minimal() +
  # theme(axis.title = element_blank(),
  #       axis.text.x = element_text(size = 12, vjust = 0.5),
  #       axis.text.y = element_blank(),
  #       panel.grid = element_blank(),
  #       legend.position = "right") +
  labs(fill = "Climate Distribution") +
  ggtitle("Wind Rose Plot with Species Counts and Climate Distribution") +
  facet_wrap(~climate_zone, scales = "free_y")

#################################################################################################################








