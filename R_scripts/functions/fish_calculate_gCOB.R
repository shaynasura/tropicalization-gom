#' Calculate geographic center of biomass (gCOB) from biomass values of fish species caught in SEAMAP trawls
#'
#'
#'
#'


# function to calculate gCOBs for fish species
fish_calculate_gCOB <- function(data = bio_data_clean_all,
                                output_name = "gCOBs_all_data",
                                taxonomic_column = "new_TAXON",
                                # taxonomic_column = "TAXONOMIC",
                                common_name_column = "common_name",
                                species_name)
{
  species_data <- data %>% 
    filter(if_any(all_of(taxonomic_column), ~ toupper(.) == toupper(species_name)))
  
  # Check if there is data for the specified species
  if (nrow(species_data) == 0) {
    stop(paste("No data found for species:", species_name))
  }
  
  # determine the specific years where fish appeared in trawl(s)
  unique_years <- unique(species_data$YR)
  
  # initiate empty data frame to store data in
  cg <- data.frame(year = unique_years,
                   latitude = NA,
                   longitude = NA)
  
  # for loop to calculate gCOB latitude and longitude for each year when fish appeared in trawl(s)
  for(i in 1:length(unique_years)) {
    filtered_data <- filter(species_data, YR == unique_years[i])
    
    # Check if the filtered dataset is not empty
    if(nrow(filtered_data) > 0) {
      cg$latitude[i] <- sum(filtered_data$DECSLAT * filtered_data$SELECT_BGS) / sum(filtered_data$SELECT_BGS)
      cg$longitude[i] <- sum(filtered_data$DECSLON * filtered_data$SELECT_BGS) / sum(filtered_data$SELECT_BGS)
    } else {
      # If the filtered dataset is empty, assign NA to latitude and longitude
      cg$latitude[i] <- NA
      cg$longitude[i] <- NA
    }}
  
  # Order the cg dataframe by time / years
  cg <- cg %>% arrange(year)
  
  # Get the existing list from the global environment
  global_species_gCOBs <- get(output_name, envir = .GlobalEnv)
  
  # Assign the cg dataframe with gCOB values the name of the fish species and add to the global_species_gCOBs list
  global_species_gCOBs[[paste(species_name, "_gCOB", sep = "")]] <- cg
  
  # Assign the modified list back to the global environment
  assign(output_name, global_species_gCOBs, envir = .GlobalEnv) 
  
  # Return the cg dataframe
  return(cg)
  
}


## Example use: 
# must create an empty list to store cg dataframe
# gCOBs_all_data <- list()
# gCOBs_yr2010_2022_m678 <- list()
