#'
#'
#'

library(geosphere)
library(dplyr)

# Function to calculate bearing and distance moved for gCOBs for all species in a list
calculate_all_gCOB_movements <- function(gCOBs_data) {
  # Initialize an empty list to store results for each species
  results_list <- list()
  
  # Loop over each species in the gCOBs_data list
  for (species_name in names(gCOBs_data)) {
    # Remove "_gCOB" suffix from species name
    clean_species_name <- sub("_gCOB$", "", species_name)
    
    # Retrieve gCOB data for the species
    species_gCOBs <- gCOBs_data[[species_name]]
    
    # Ensure that there are at least two years of gCOB data
    if (nrow(species_gCOBs) >= 2) {
      # Get the earliest and latest years with gCOB data
      earliest <- species_gCOBs %>% filter(!is.na(latitude) & !is.na(longitude)) %>% slice(1)
      latest <- species_gCOBs %>% filter(!is.na(latitude) & !is.na(longitude)) %>% slice(n())
      
      # Coordinates of the earliest and latest years
      start_coords <- c(earliest$longitude, earliest$latitude)
      end_coords <- c(latest$longitude, latest$latitude)
      
      # Define ellipsoid parameters for NAD83 (GRS 80)
      NAD83_ellipsoid <- c(a = 6378137.0, f = 1/298.257222101)
      
      # Calculate bearing from earliest to latest gCOB
      bearing_move <- bearing(start_coords, end_coords, a = NAD83_ellipsoid["a"], f = NAD83_ellipsoid["f"])
      
      #Check if bearing is negative, and if so,  normalize it by adding 360
      bearing_move <- ifelse(bearing_move < 0, bearing_move + 360, bearing_move)
      
      # Calculate distance from earliest to latest gCOB
      distance_move <- distGeo(start_coords, end_coords, a = NAD83_ellipsoid["a"], f = NAD83_ellipsoid["f"])
      
      # Store the results in the list with the species name
      results_list[[species_name]] <- data.frame(species = clean_species_name, bearing = bearing_move, distance = distance_move)
    } else {
      # If there are not enough data points, return NA for bearing and distance
      results_list[[species_name]] <- data.frame(species = clean_species_name, bearing = NA, distance = NA)
    }
  }
  
  # Combine all data frames in the list into one data frame
  results_df <- do.call(rbind, results_list)
  
  # Return the combined data frame
  return(results_df)

  
  # # Return the complete list of results
  # return(results_list)
}

# Example usage:
# gCOBs_data <- get("gCOBs_all_data", envir = .GlobalEnv)
# movement_results <- calculate_all_gCOB_movements(gCOBs_data)
# print(movement_results)







# ## PREVIOUS VERSION BELOW HERE FOR JUST A SINGLE SPECIES
# 
# # Function to calculate bearing and distance moved for gCOBs
# calculate_gCOB_movement <- function(species_name, gCOBs_data) {
#   # Retrieve gCOB data for the specified species
#   species_gCOBs <- gCOBs_data[[paste(species_name, "_gCOB", sep = "")]]
#   
#   # Ensure that there are at least two years of gCOB data
#   if (nrow(species_gCOBs) < 2) {
#     stop(paste("Not enough data for species:", species_name))
#   }
#   
#   # Get the earliest and latest years with gCOB data
#   earliest <- species_gCOBs %>% filter(!is.na(latitude) & !is.na(longitude)) %>% slice(1)
#   latest <- species_gCOBs %>% filter(!is.na(latitude) & !is.na(longitude)) %>% slice(n())
#   
#   # Coordinates of the earliest and latest years
#   start_coords <- c(earliest$longitude, earliest$latitude)
#   end_coords <- c(latest$longitude, latest$latitude)
#   
#   # Define ellipsoid parameters for NAD83 (GRS 80)
#   NAD83_ellipsoid <- c(a = 6378137.0, f = 1/298.257222101)
#   
#   # Calculate bearing from earliest to latest gCOB
#   bearing_move <- bearing(start_coords, end_coords, a = NAD83_ellipsoid["a"], f = NAD83_ellipsoid["f"])
#   
#   # Calculate distance from earliest to latest gCOB
#   distance_move <- distGeo(start_coords, end_coords, a = NAD83_ellipsoid["a"], f = NAD83_ellipsoid["f"])
#   
#   # Return a list with the bearing and distance moved
#   return(list(bearing = bearing_move, distance = distance_move))
# }
# 
# # Example usage:
# # gCOBs_data <- get("gCOBs_all_data", envir = .GlobalEnv)
# # result <- calculate_gCOB_movement(species_name = "ExampleFish", gCOBs_data = gCOBs_data)
# # print(result)
# 
