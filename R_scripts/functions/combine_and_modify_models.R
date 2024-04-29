#'
#'
#'
#'




# modify list of models into a single combined dataframe
combine_and_modify_models <- function(model_list, output_name, pattern) {
  # Create an empty list to store modified models
  model_combined <- list()
  
  # Loop through each model in the list
  for (i in seq_along(model_list)) {
    # Determine the pattern based on the input value
    if (pattern == "latitude") {
      species_name <- gsub("_latitude_lm_model", "", names(model_list)[i])
    } else if (pattern == "longitude") {
      species_name <- gsub("_longitude_lm_model", "", names(model_list)[i])
    } else {
      stop("Invalid pattern. Please specify 'latitude' or 'longitude'.")
    }
    
    # Add the species name as a new column to the model output
    model_with_species <- model_list[[i]] %>%
      mutate(species = species_name)
    
    # Store the modified model in the combined list
    model_combined[[i]] <- model_with_species
  }
  
  # Combine all the modified models into a single data frame
  combined_df <- bind_rows(model_combined)
  
  # Reorder columns so species is first
  combined_df <- combined_df %>%
    select(species, everything()) %>% 
    rename(species_name = species) %>% 
    left_join(species_climate_df, join_by(species_name)) %>% 
    rename(species = species_name)
  
  
  # Write the combined data frame to a file
  write.csv(combined_df, file = paste0("output/", output_name, ".csv"), row.names = FALSE)
  
  # Return the combined data frame
  return(combined_df)
}




# Example usage:
# combined_df <- combine_and_modify_models(model_list = lat_models_yr2010_2022_m678,
#                                          output_name = "output_combined_models", pattern = "latitude")
# 
# lat_models_df_yr2010_2022_m678_filtered <- combine_and_modify_models(model_list = lat_models_filtered_yr2010_2022_m678,
#                                                                      output_name = "yr2010_2022_m678_filtered_lat_models_df", pattern = "latitude)



