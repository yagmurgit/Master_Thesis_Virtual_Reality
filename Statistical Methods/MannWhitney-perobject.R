# Load required libraries
library(dplyr)
library(ggplot2)
library(tidyr)  

load_data <- function(file_path) {
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  return(df)
}


reshape_data <- function(df) {
  
  df_long <- df %>%
    pivot_longer(cols = starts_with("Object") & ends_with("Errors"),  # Include only error columns
                 names_to = "Object", 
                 values_to = "Value") %>%
    mutate(Object = gsub("Object", "", Object)) %>%  # Clean Object column names
    mutate(Object = factor(Object))  
  
  # Add numbering for objects based on the condition and set order
  df_long <- df_long %>%
    mutate(Object = case_when(
      Complexity == "SF" ~ paste0("Object ", as.numeric(Object)),  # SF: Objects 1 to 5
      Complexity == "MF" ~ paste0("Object ", as.numeric(Object) + 5)  # MF: Objects 6 to 10
    )) %>%
    mutate(Object = factor(Object, levels = c("Object 1", "Object 2", "Object 3", "Object 4", "Object 5",
                                              "Object 6", "Object 7", "Object 8", "Object 9", "Object 10")))  # Set ordered levels
  
  return(df_long)
}


plot_SF_APA <- function(df_long) {
  message("Generating APA plot for Single Floor (SF)...")
  
  ggplot(df_long %>% filter(Complexity == "SF"), 
         aes(x = Learning_Environment, y = Value, fill = Object)) +
    geom_bar(stat = "summary", fun = "mean", position = position_dodge(0.9), 
             color = "black", width = 0.7) +  # Bar plot for mean values
    geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, 
                  position = position_dodge(0.9), linewidth = 0.5) +  # Error bars
    labs(x = "Learning Environment", 
         y = "Average Number of Errors", 
         fill = "", 
         title = "Single Floor (SF) - Average Number of Errors per Object") +  # Add title
    theme_classic() +  # Classic theme
    theme(
      text = element_text(size = 16, family = "serif"),  # Serif font
      axis.text.x = element_text(angle = 0, hjust = 0.5),  # Align x-axis labels
      legend.text = element_text(size = 14),  # Adjust legend text size
      legend.title = element_text(size = 16)  # Adjust legend title size
    )
}


plot_MF_APA <- function(df_long) {
  message("Generating APA plot for Multi-Floor (MF)...")
  
  ggplot(df_long %>% filter(Complexity == "MF"), 
         aes(x = Learning_Environment, y = Value, fill = Object)) +
    geom_bar(stat = "summary", fun = "mean", position = position_dodge(0.9), 
             color = "black", width = 0.7) +  # Bar plot for mean values
    geom_errorbar(stat = "summary", fun.data = "mean_se", width = 0.2, 
                  position = position_dodge(0.9), linewidth = 0.5) +  # Error bars
    labs(x = "Learning Environment", 
         y = "Average Number of Errors", 
         fill = "", 
         title = "Multi-Floor (MF) - Average Number of Errors per Object") +  # Add title
    theme_classic() +  # Classic theme
    theme(
      text = element_text(size = 16, family = "serif"),  # Serif font
      axis.text.x = element_text(angle = 0, hjust = 0.5),  # Align x-axis labels
      legend.text = element_text(size = 14),  # Adjust legend text size
      legend.title = element_text(size = 16)  # Adjust legend title size
    )
}


conduct_mann_whitney_test <- function(df_long) {
  
  mw_results <- df_long %>%
    filter(Complexity %in% c("SF", "MF")) %>%  # Filter for SF and MF conditions
    group_by(Object, Complexity) %>%  # Group by Object and Complexity
    filter(n_distinct(Learning_Environment) == 2) %>%  # Ensure two groups: R and VR
    summarise(
      mw_test = list(suppressWarnings(wilcox.test(Value ~ Learning_Environment))),  # Mann-Whitney test
      W_statistic = mw_test[[1]]$statistic,  # Extract U (W statistic)
      p_value = mw_test[[1]]$p.value,        # Extract p-value
      .groups = 'drop'
    )
  
  return(mw_results)
}


run_analysis <- function() {
  # Request file path from user
  file_path <- readline(prompt = "Please enter the full path to the CSV file: ")
  
  
  df <- load_data(file_path)
  df_long <- reshape_data(df)
  
  
  message("=== Single Floor (SF) Plot ===")
  print(plot_SF_APA(df_long))
  
 
  message("=== Multi-Floor (MF) Plot ===")
  print(plot_MF_APA(df_long))
  
 
  mw_results <- conduct_mann_whitney_test(df_long)
  message("=== Mann-Whitney U Test Results ===")
  print(mw_results)
  
  message("Analysis completed.")
}

# Execute the analysis
run_analysis()
