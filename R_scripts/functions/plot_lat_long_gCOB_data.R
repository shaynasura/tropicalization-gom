#'
#'
#'
#'


plot_lat_long_gCOB_data <- function(lat_data, long_data, title_prefix) {
  
  # Extract unique names from input data frame names
  lat_unique_name <- str_extract(deparse(substitute(lat_data)), "(?<=lat_models_df_).*")
  long_unique_name <- str_extract(deparse(substitute(long_data)), "(?<=long_models_df_).*")
  
  # lat_unique_name <- str_extract(deparse(substitute(lat_data)), "(?<=lat_models_df_)[^_]+$")
  # long_unique_name <- str_extract(deparse(substitute(long_data)), "(?<=long_models_df_)[^_]+$")
  
  # Plotting latitude data
  lat_data_to_plot <- lat_data %>% 
    filter(term != "(Intercept)") %>% 
    group_by(climate_zone) %>% 
    mutate(species = factor(species, levels = unique(species[order(estimate)]))) %>% 
    ungroup()
  
  lat_plot <- ggplot(data = lat_data_to_plot,
                     aes(x = estimate, y = species, color = species)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Estimate (95% CI) of effect of year on latitude gCOB",
         y = "Species",
         title = paste0(title_prefix, " all latitudinal trends in gCOBs")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 7, face = "italic")) +
    facet_wrap(~climate_zone, scales = "free_y") +
    scale_y_discrete(labels = function(x) str_to_sentence(x))
  
  # Saving the latitude plot as a PDF
  ggsave(paste0("plots/all_lat_shifts_", lat_unique_name, ".pdf"), plot = lat_plot, width = 6.5, height = 11)
  
  # Assigning the latitude plot to a figure number and making it accessible in the environment
  assign(paste0("Fig_1_", lat_unique_name), lat_plot, envir = .GlobalEnv)
  
  # Plotting longitude data
  long_data_to_plot <- long_data %>% 
    filter(term != "(Intercept)") %>% 
    group_by(climate_zone) %>% 
    mutate(species = factor(species, levels = unique(species[order(estimate)]))) %>% 
    ungroup()
  
  long_plot <- ggplot(data = long_data_to_plot,
                      aes(x = estimate, y = species, color = species)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Estimate (95% CI) of effect of year on longitude gCOB",
         y = "Species",
         title = paste0(title_prefix, " all longitudinal trends in gCOBs")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 7, face = "italic")) +
    facet_wrap(~climate_zone, scales = "free_y") +
    scale_y_discrete(labels = function(x) str_to_sentence(x))
  
  # Saving the longitude plot as a PDF
  ggsave(paste0("plots/all_long_shifts_", long_unique_name, ".pdf"), plot = long_plot, width = 6.5, height = 11)
  
  # Assigning the longitude plot to a figure number and making it accessible in the environment
  assign(paste0("Fig_2_", long_unique_name), long_plot, envir = .GlobalEnv)
  
  # Plotting significant latitude data
  signif_lat_data_to_plot <- lat_data %>% 
    filter(term != "(Intercept)") %>% 
    mutate(signif_95CI = ifelse(conf.low < 0 & conf.high > 0, "not significant", "significant")) %>% 
    mutate(signif_pvalue = ifelse(p.value < 0.05, "significant", "not significant")) %>% 
    filter(signif_95CI == "significant") %>% 
    group_by(climate_zone) %>% 
    mutate(species = factor(species, levels = unique(species[order(estimate)]))) %>% 
    ungroup()
  
  signif_lat_plot <- ggplot(data = signif_lat_data_to_plot,
                            aes(x = estimate, y = species, color = species)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Estimate (95% CI) of effect of year on latitude gCOB",
         y = "Species",
         title = paste0(title_prefix, " significant latitudinal shifts in gCOBs")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 7, face = "italic")) +
    facet_wrap(~climate_zone, scales = "free_y") +
    scale_y_discrete(labels = function(x) str_to_sentence(x))
  
  # Saving the significant latitude plot as a PDF
  ggsave(paste0("plots/signif_lat_shifts_", lat_unique_name, ".pdf"), plot = signif_lat_plot, width = 7.5, height = 6)
  
  # Assigning the significant latitude plot to a figure number and making it accessible in the environment
  assign(paste0("Fig_3_", lat_unique_name), signif_lat_plot, envir = .GlobalEnv)
  
  # Plotting significant longitude data
  signif_long_data_to_plot <- long_data %>% 
    filter(term != "(Intercept)") %>% 
    mutate(signif_95CI = ifelse(conf.low < 0 & conf.high > 0, "not significant", "significant")) %>% 
    mutate(signif_pvalue = ifelse(p.value < 0.05, "significant", "not significant")) %>% 
    filter(signif_95CI == "significant") %>% 
    group_by(climate_zone) %>% 
    mutate(species = factor(species, levels = unique(species[order(estimate)]))) %>% 
    ungroup()
  
  signif_long_plot <- ggplot(data = signif_long_data_to_plot,
                             aes(x = estimate, y = species, color = species)) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    labs(x = "Estimate (95% CI) of effect of year on longitude gCOB",
         y = "Species",
         title = paste0(title_prefix, " significant longitudinal shifts in gCOBs")) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.y = element_text(size = 7, face = "italic")) +
    facet_wrap(~climate_zone, scales = "free_y") +
    scale_y_discrete(labels = function(x) str_to_sentence(x))
  
  # Saving the significant longitude plot as a PDF
  ggsave(paste0("plots/signif_long_shifts_", long_unique_name, ".pdf"), plot = signif_long_plot, width = 9, height = 6)
  
  # Assigning the significant longitude plot to a figure number and making it accessible in the environment
  assign(paste0("Fig_4_", long_unique_name), signif_long_plot, envir = .GlobalEnv)
  
}




# Example usage
# plot_lat_long_gCOB_data(lat_data = lat_models_df_yr2010_2022_m678,
#                         long_data = long_models_df_yr2010_2022_m678,
#                         title_prefix = "2010-2022 June, July, & August")
#
# should produce four figures
# Fig_1_yr2010_2022_m678
# Fig_2_yr2010_2022_m678
# Fig_3_yr2010_2022_m678
# Fig_4_yr2010_2022_m678



