


# Function to create a map/animation for a given fish species
fish_species_animation <- function(
    data,
    taxonomic_column,
    common_name_column,
    species_name,
    low_color = "#470887",  # Default low color
    high_color = "#F4D6FF"  # Default high color
) {
  # Subset data for the specified species
  species_data <- data %>% 
    filter(if_any(all_of(taxonomic_column), ~ toupper(.) == toupper(species_name)))
  
  # Check if there is data for the specified species
  if (nrow(species_data) == 0) {
    stop(paste("No data found for species:", species_name))
  }
  
  # Create survey years data frame
  survey_years <- data.frame("YR" = sort(unique(data$YR)))
  
  # Join survey years to the species data
  species_data <- right_join(species_data, survey_years, by = "YR")
  
  # Define color gradient for the species
  species_color_gradient <- scale_fill_gradient(
    low = low_color,
    high = high_color,
    limits = range(survey_years$YR),
    breaks = seq(min(survey_years$YR), max(survey_years$YR), by = 4),
    labels = seq(min(survey_years$YR), max(survey_years$YR), by = 4),
    name = "Year"
  )
  
  # # Create base map
  # base_map <- ggplot()  # Add your base map specifications here
  
  # # Create ggplot object for the map
  # plot <- base_map +
  #   geom_point(data = species_data,
  #              na.rm = FALSE,
  #              aes(x = DECSLON,
  #                  y = DECSLAT,
  #                  group = YR,
  #                  fill = YR),
  #              size = 2.5,
  #              shape = 21,
  #              color = "black",
  #              stroke = 0.5) +
  #   species_color_gradient
  
  
  # Create ggplot object for the map
  plot <- ggplot() +
    geom_sf(data = world_base_map_sf, color = "black", fill = "white") +
    geom_sf(data = usa_state_borders_sf, color = "black", fill = "white") +
    geom_sf(data = species_data, 
            aes(geometry = geometry, group = YR, fill = YR),
            size = 2.5,
            shape = 21,
            color = "black",
            stroke = 0.5) +
    coord_sf(xlim = c(-97.7, -79.9), ylim = c(24, 31), crs = st_crs(4269)) +
    theme(panel.background = element_rect(fill = c("#CFEFFF")),
          panel.border = element_rect(fill = NA, color = "black")) +
    labs(x = "Longitude", y = "Latitude") +
    species_color_gradient
  
  # Create animation
  animation <- plot +
    transition_time(YR) +
    ggtitle(
      label = NULL,  # No main title, just a subtitle
      subtitle = bquote(
        .(toTitleCase(species_data[[common_name_column]][1])) ~
          "(" * italic(.(paste0(substring(species_name, 1, 1),
                                tolower(substring(species_name, 2, regexpr(" ", species_name))),
                                "",
                                tolower(substring(species_name, regexpr(" ", species_name) + 1))))) * ")" ~
          "Presence" ~ "\n" ~
          .(paste0("\nYear: {frame_time}\nFrame {frame} of {nframes}"))
      )
    ) +
    # ggtitle(
    #   label = bquote(
    #     .(toTitleCase(species_data[[common_name_column]][1])) ~ 
    #       "(" * italic(.(paste0(substring(species_name, 1, 1), 
    #                             tolower(substring(species_name, 2, regexpr(" ", species_name))), 
    #                             "", 
    #                             tolower(substring(species_name, regexpr(" ", species_name) + 1))))) * ")" ~ 
    #       "Presence"),
    #   subtitle = (paste0("\nYear: {frame_time}\nFrame {frame} of {nframes}"))
    # ) +
    shadow_mark(alpha = 0.6,
                color = "darkgray",
                size = 1.5)  +
    theme(text = element_text(family = "Times",
                              size = 14),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(size = 12,
                                    face = "bold"),
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12,
                                      face = "bold",
                                      hjust = 0.5),
          legend.text = element_text(size = 10))
  
  num_years <- max(data$YR - min(data$YR)) + 1
  animation <- animate(animation,
                       nframes = num_years,
                       fps = 2,
                       duration = (num_years + 5)/2,
                       end_pause = 5,
                       height = 4,
                       width = 6,
                       units = "in",
                       res = 300)
  
  # Assign animation object to a variable with a name that includes the species name
  assign(paste("animation_", species_name, sep = ""), animation, envir = .GlobalEnv)
  
  # Return the animation object
  return(animation)
}

