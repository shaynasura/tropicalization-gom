



library(geosphere)
library(dplyr)

# Function to calculate bearing and distance moved for gCOBs for all species in a list, year to year
calculate_annual_gCOB_movements <- function(gCOBs_data) {
  # Initialize an empty list to store results for each species
  results_list <- list()
  
  # Loop over each species in the gCOBs_data list
  for (species_name in names(gCOBs_data)) {
    # Remove "_gCOB" suffix from species name
    clean_species_name <- sub("_gCOB$", "", species_name)
    
    # Retrieve gCOB data for the species and sort by year
    species_gCOBs <- gCOBs_data[[species_name]] %>% 
      filter(!is.na(latitude) & !is.na(longitude)) %>%
      arrange(year)
    
    # Ensure that there are at least two years of gCOB data
    if (nrow(species_gCOBs) >= 2) {
      # Loop over consecutive years to calculate bearing and distance
      species_results <- data.frame()
      for (i in 1:(nrow(species_gCOBs) - 1)) {
        # Get the coordinates of two consecutive years
        start_coords <- c(species_gCOBs$longitude[i], species_gCOBs$latitude[i])
        end_coords <- c(species_gCOBs$longitude[i + 1], species_gCOBs$latitude[i + 1])
        
        # Define ellipsoid parameters for NAD83 (GRS 80)
        NAD83_ellipsoid <- c(a = 6378137.0, f = 1/298.257222101)
        
        # Calculate bearing between consecutive years
        bearing_move <- bearing(start_coords, end_coords, a = NAD83_ellipsoid["a"], f = NAD83_ellipsoid["f"])
        # Normalize bearing if it's negative
        bearing_move <- ifelse(bearing_move < 0, bearing_move + 360, bearing_move)
        
        # Calculate distance between consecutive years
        distance_move <- distGeo(start_coords, end_coords, a = NAD83_ellipsoid["a"], f = NAD83_ellipsoid["f"])
        
        # Create year pair string
        year_pair <- paste0(species_gCOBs$year[i], "-", species_gCOBs$year[i + 1])
        
        # Store results for the species
        species_results <- rbind(species_results, data.frame(
          species = clean_species_name,
          year_pair = year_pair,
          bearing = bearing_move,
          distance = distance_move
        ))
      }
      # Add species-specific results to the main results list
      results_list[[species_name]] <- species_results
    } else {
      # If there are not enough data points, return NA for bearing and distance
      results_list[[species_name]] <- data.frame(
        species = clean_species_name, 
        year_pair = NA, 
        bearing = NA, 
        distance = NA
      )
    }
  }
  
  # Combine all data frames in the list into one data frame
  results_df <- do.call(rbind, results_list)
  
  # Return the combined data frame
  return(results_df)
}

# Example usage:
# gCOBs_data <- get("gCOBs_all_data", envir = .GlobalEnv)
# annual_movement_results <- calculate_annual_gCOB_movements(gCOBs_data)
# print(annual_movement_results)
