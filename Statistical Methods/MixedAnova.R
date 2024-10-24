
if (!require(dplyr)) install.packages("dplyr", dependencies = TRUE)
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
if (!require(car)) install.packages("car", dependencies = TRUE)


library(dplyr)
library(ggplot2)
library(car)


load_data <- function(file_path) {
  df <- read.csv(file_path, sep = ",", stringsAsFactors = FALSE)
  return(df)
}

# convert factors and transform variables
prepare_data <- function(df) {
  df$Learning_Environment <- factor(df$Learning_Environment, levels = c("R", "VR"))
  df$Complexity <- factor(df$Complexity, levels = c("SF", "MF"))
  
  # Square root transformation of No_of_errors
  df <- df %>% mutate(Sqrt_No_of_errors = sqrt(No_of_errors))
  
  return(df)
}


perform_anova <- function(df) {
  anova_result <- aov(Sqrt_No_of_errors ~ Complexity * Learning_Environment + Error(Subject_ID / Complexity), data = df)
  return(summary(anova_result))
}

# Q-Q plots for normality check
create_qq_plot <- function(df) {
  ggplot(df, aes(sample = Sqrt_No_of_errors)) +
    geom_qq() +
    geom_qq_line() +
    facet_grid(Complexity ~ Learning_Environment) +
    theme_minimal() +
    labs(title = "Q-Q Plots for Square Root Transformed Data") +
    theme(plot.title = element_text(hjust = 0.5))
}

#Levene's test for homogeneity of variances
levene_test <- function(df) {
  leveneTest(Sqrt_No_of_errors ~ Complexity * Learning_Environment, data = df)
}

# Shapiro-Wilk normality test
perform_shapiro_test <- function(df) {
  df %>%
    group_by(Complexity, Learning_Environment) %>%
    summarise(Shapiro_Wilk_p = shapiro.test(Sqrt_No_of_errors)$p.value)
}


calculate_summary <- function(df) {
  df %>%
    group_by(Complexity, Learning_Environment) %>%
    summarise(
      Mean_No_of_errors = mean(No_of_errors, na.rm = TRUE),
      SEM = sd(No_of_errors, na.rm = TRUE) / sqrt(n())
    )
}


create_bar_chart <- function(df_summary) {
  ggplot(df_summary, aes(x = Complexity, y = Mean_No_of_errors, fill = Learning_Environment)) +
    geom_bar(stat = "identity", color = "black", width = 0.7, position = position_dodge(0.8)) +
    geom_errorbar(aes(ymin = Mean_No_of_errors - SEM, ymax = Mean_No_of_errors + SEM), width = 0.2, position = position_dodge(0.8)) +
    geom_text(aes(label = round(Mean_No_of_errors, 1)), vjust = -0.5, position = position_dodge(0.8), size = 5) +
    geom_text(aes(label = paste0(round(Mean_No_of_errors, 1), " ± ", round(SEM, 2))), vjust = -1.5, position = position_dodge(0.8), size = 4) +
    scale_fill_manual(values = c("R" = "#1f77b4", "VR" = "#ff7f0e")) +
    labs(x = "Complexity", y = "Average Number of Errors") +
    theme_classic() +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.position = "right",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
}


run_analysis <- function() {
  file_path <- readline(prompt = "Enter the CSV file path: ")
  
  df <- load_data(file_path)
  df <- prepare_data(df)
  
  cat("ANOVA Results:\n")
  print(perform_anova(df))
  
  cat("\nLevene's Test Results:\n")
  print(levene_test(df))
  
  cat("\nShapiro-Wilk Test Results:\n")
  print(perform_shapiro_test(df))
  
  cat("\nQ-Q Plot:\n")
  print(create_qq_plot(df))
  
  df_summary <- calculate_summary(df)
  
  cat("\nBar Chart with Error Bars:\n")
  print(create_bar_chart(df_summary))
}

# Run the analysis
run_analysis()
