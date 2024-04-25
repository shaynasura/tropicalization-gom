#'
#'
#'
#'




# filter the list of gCOB dataframes to only keep those that have calculated gCOB values for a minimum number of years (e.g. 5)
filter_gCOB_dataframes <- function(df_list, min_years = 5) {
  # Initialize an empty list to store filtered data frames
  filtered_list <- list()
  
  # Loop through each data frame in the list
  for (i in seq_along(df_list)) {
    df <- df_list[[i]]  # Extract the current data frame
    # Calculate the number of unique years for each species
    unique_years <- length(unique(df$year))
    
    # Check if the number of unique years is at least min_years
    if (unique_years >= min_years) {
      # If yes, add the data frame to the filtered list with its original name
      filtered_list[[names(df_list)[i]]] <- df
    }
  }
  
  # Return the filtered list of data frames
  return(filtered_list)
}



# # Example usage
# # Filter the list of data frames
# gCOBs_filtered_yr2010_2022_m678 <- filter_gCOB_dataframes(gCOBs_yr2010_2022_m678, min_years = 5)