library(dplyr)
library(ggplot2)
library(car)  
library(rstatix)  

file_path <- "path/to/your/file" 
data <- read.csv(file_path, sep = ",", stringsAsFactors = FALSE)

data$Learning_Environment <- factor(data$Learning_Environment, levels = c("R", "VR"))
data$Complexity <- factor(data$Complexity, levels = c("SF", "MF"))

analyze_conditions <- function(data) {
  results <- list()
  
  for (floor_type in c("SF", "MF")) {
    dataset <- data %>% filter(Complexity == floor_type)
    cat(paste("Analyzing", floor_type, "conditions\n"))
    
    # Plot histograms
    ggplot(dataset, aes(x = Boxes_picked)) + 
      geom_histogram(binwidth = 2, fill = "blue", color = "black") + 
      facet_wrap(~Learning_Environment, scales = "free") + 
      ggtitle(paste("Histograms of Boxes Picked for", floor_type, "Conditions")) +
      theme_minimal() 
    ggsave(paste0("histogram_", floor_type, ".png"), width = 6, height = 4)  # Save the histogram plot
    
    #Shapiro-Wilk test
    shapiro_results <- dataset %>% 
      group_by(Learning_Environment) %>% 
      shapiro_test(Boxes_picked)
    print(shapiro_results)
    
    # Levene's test for homogeneity of variances
    levene_result <- leveneTest(Boxes_picked ~ Learning_Environment, data = dataset)
    print(levene_result)
    
    
    if (n_distinct(dataset$Learning_Environment) == 2) {
      if (all(shapiro_results$p > 0.05) && levene_result$`Pr(>F)`[1] > 0.05) {
        cat("Conditions for t-test are met. Running Student's t-test.\n")
        t_test_result <- t.test(Boxes_picked ~ Learning_Environment, data = dataset)
        print(t_test_result)
      } else {
        cat("Running Mann-Whitney U Test (Wilcoxon rank-sum test).\n")
        mann_whitney_result <- wilcox_test(Boxes_picked ~ Learning_Environment, data = dataset)
        print(mann_whitney_result)
      }
    } else {
      if (all(shapiro_results$p > 0.05) && levene_result$`Pr(>F)`[1] > 0.05) {
        cat("Conditions for ANOVA are met. Running One-Way ANOVA.\n")
        anova_result <- aov(Boxes_picked ~ Learning_Environment, data = dataset)
        print(summary(anova_result))
      }
    }
    
    
    for (env in unique(dataset$Learning_Environment)) {
      env_data <- dataset %>% filter(Learning_Environment == env)
      lm_model <- lm(No_of_errors ~ Boxes_picked, data = env_data)
      cat(paste("Linear regression results for", env, "in", floor_type, "conditions:\n"))
      print(summary(lm_model))
      
   
      plot <- ggplot(env_data, aes(x = Boxes_picked, y = No_of_errors)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE, color = "black") +
        labs(title = paste("Linear Regression for", env, "in", floor_type, "Conditions"),
             x = "Boxes Picked",
             y = "Number of Errors") +
        theme_minimal()
      
      
      print(plot)
      
      
      ggsave(paste0("linear_regression_", env, "_", floor_type, ".png"), plot = plot, width = 6, height = 4)
    }
    
    results[[floor_type]] <- dataset
  }
  
  return(results)
}

analyze_conditions(data)


plot_mean_boxes <- function(data) {
  summary_stats <- data %>%
    group_by(Complexity, Learning_Environment) %>%
    summarise(
      Mean = mean(Boxes_picked),
      SD = sd(Boxes_picked),
      SE = SD / sqrt(n()),  
      .groups = 'drop'
    )
  
  ggplot(summary_stats, aes(x = Learning_Environment, y = Mean, fill = Learning_Environment)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(0.6)) +
    labs(
      title = "Average Boxes Picked for Different Conditions",
      x = "Learning Environment",
      y = "Average Number of Boxes Picked"
    ) +
    facet_wrap(~ Complexity) +  
    theme_minimal(base_size = 15) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.title.x = element_text(size = 12, face = "plain"),
      axis.title.y = element_text(size = 12, face = "plain"),
      axis.text.x = element_text(size = 12, face = "plain"),
      axis.text.y = element_text(size = 12, face = "plain")
    ) +
    scale_fill_manual(values = c("#1f77b4", "#ff7f0e"))
}

plot_mean_boxes(data)
