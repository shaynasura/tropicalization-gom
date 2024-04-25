#'
#'
#'


# function to run linear models on gCOB latitude and longitude values over time to test for shifts
test_gCOBs_over_time <- function(species_name,
                                 input_gCOBs,
                                 output_df_name,
                                 latitude_models_output_name,
                                 longitude_models_output_name)
{
  species <- tolower(species_name)
  
  # Find the index corresponding to the species name (case-insensitive)
  index <- which(grepl(species, tolower(names(input_gCOBs))))
  
  if (length(index) == 0) {
    # If no match is found, print an error message
    print("Species not found in the input_gCOBs list.")
  } else {
    # Access the result for the first matching species
    species_result <- input_gCOBs[[index[1]]]
  }
  
  
  internal_gCOB_trends <- data.frame(species = NA,
                                     latitude_year_coef = NA,
                                     latitude_year_SE = NA,
                                     latitude_pvalue = NA,
                                     latitude_rsquared = NA,
                                     latitude_rsquared_adj = NA,
                                     longitude_year_coef = NA,
                                     longitude_year_SE = NA,
                                     longitude_pvalue = NA,
                                     longitude_rsquared = NA,
                                     longitude_rsquared_adj = NA)
  
  # put species name in first column
  internal_gCOB_trends$species <- toupper(species_name)
  
  
  # Check if there are any non-NA values in the data frame
  if (all(is.na(species_result$latitude)) || all(is.na(species_result$longitude))) {
    # If there are only NAs in the data frame, assign NA to all values
    internal_gCOB_trends$latitude_year_coef <- NA
    internal_gCOB_trends$latitude_year_SE <- NA
    internal_gCOB_trends$latitude_pvalue <- NA
    internal_gCOB_trends$latitude_rsquared <- NA
    internal_gCOB_trends$latitude_rsquared_adj <- NA
    internal_gCOB_trends$longitude_year_coef <-NA
    internal_gCOB_trends$longitude_year_SE <- NA
    internal_gCOB_trends$longitude_pvalue <- NA
    internal_gCOB_trends$longitude_rsquared <- NA
    internal_gCOB_trends$longitude_rsquared_adj <- NA
  } else {
    
    # fit linear models testing for trends in latitude and longitude of gCOBs over time
    lat_model <- lm(latitude ~ year, data = species_result)
    long_model <- lm(longitude ~ year, data = species_result)
    
    # Check if the models produced valid results
    if(is.na(coef(lat_model)[["year"]]) == TRUE) {
      # If the coefficient for the year predictor is NA (meaning only one data point), assign NA to all values
      internal_gCOB_trends$latitude_year_coef <- NA
      internal_gCOB_trends$latitude_year_SE <- NA
      internal_gCOB_trends$latitude_pvalue <- NA
      internal_gCOB_trends$latitude_rsquared <- NA
      internal_gCOB_trends$latitude_rsquared_adj <- NA
    } else {
      # Pull out important result values from the linear model summary
      internal_gCOB_trends$latitude_year_coef <- lat_model$coefficients[["year"]]
      internal_gCOB_trends$latitude_year_SE <- coef(summary(lat_model))[["year","Std. Error"]]
      internal_gCOB_trends$latitude_pvalue <- coef(summary(lat_model))["year", "Pr(>|t|)"]
      internal_gCOB_trends$latitude_rsquared <- summary(lat_model)$r.squared
      internal_gCOB_trends$latitude_rsquared_adj <- summary(lat_model)$adj.r.squared
    }
    
    if(is.na(coef(long_model)[["year"]]) == TRUE) {
      # If the coefficient for the year predictor is NA (meaning only one data point), assign NA to all values
      internal_gCOB_trends$longitude_year_coef <-NA
      internal_gCOB_trends$longitude_year_SE <- NA
      internal_gCOB_trends$longitude_pvalue <- NA
      internal_gCOB_trends$longitude_rsquared <- NA
      internal_gCOB_trends$longitude_rsquared_adj <- NA
    } else {
      # Pull out important result values from the linear model summary
      internal_gCOB_trends$longitude_year_coef <- long_model$coefficients[["year"]]
      internal_gCOB_trends$longitude_year_SE <- coef(summary(long_model))[["year","Std. Error"]]
      internal_gCOB_trends$longitude_pvalue <- coef(summary(long_model))["year", "Pr(>|t|)"]
      internal_gCOB_trends$longitude_rsquared <- summary(long_model)$r.squared
      internal_gCOB_trends$longitude_rsquared_adj <- summary(long_model)$adj.r.squared
    }
  }
  
  # Get the existing dataframe from the global environment
  global_gCOB_trends <- get(output_df_name, envir = .GlobalEnv) 
  
  # Assign the cg dataframe with gCOB values the name of the fish species and add to the global_gCOB_trends dataframe
  global_gCOB_trends <- rbind(global_gCOB_trends, internal_gCOB_trends)
  
  # Assign the modified dataframe back to the global environment
  assign(output_df_name, global_gCOB_trends, envir = .GlobalEnv)
  
  
  ### get tidy output tables of model results
  tidy_latitude <- lat_model %>% tidy(conf.int = TRUE)
  tidy_longitude <- long_model %>% tidy(conf.int = TRUE)
  
  # Get the existing lists from the global environment
  global_species_latitude_models <- get(latitude_models_output_name, envir = .GlobalEnv)
  global_species_longitude_models <- get(longitude_models_output_name, envir = .GlobalEnv)
  
  # Assign the tidy tibbles with linear model outputs the name of the fish species and add to the global_species_models lists
  global_species_latitude_models[[paste(species_name, "_latitude_lm_model", sep = "")]] <- tidy_latitude
  global_species_longitude_models[[paste(species_name, "_longitude_lm_model", sep = "")]] <- tidy_longitude
  
  # Assign the modified lists back to the global environment
  assign(latitude_models_output_name, global_species_latitude_models, envir = .GlobalEnv) 
  assign(longitude_models_output_name, global_species_longitude_models, envir = .GlobalEnv)
  
  
  return(internal_gCOB_trends)
}
