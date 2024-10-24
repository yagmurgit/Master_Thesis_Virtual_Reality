library(dplyr)     
library(ggplot2)   
library(car)       

file_path <- "path/to/your/file"
data <- read.csv(file_path, sep = ",", stringsAsFactors = FALSE)
data$Learning_Environment <- factor(data$Learning_Environment, levels = c("R", "VR"))
data$Complexity <- factor(data$Complexity, levels = c("SF", "MF"))
sf_data <- data %>% filter(Complexity == "SF")
mf_data <- data %>% filter(Complexity == "MF")
sf_stats <- sf_data %>%
  group_by(Learning_Environment) %>%
  summarise(
    Mean_Distance = mean(Distance, na.rm = TRUE),  # Added na.rm = TRUE to handle any NA values
    Std_Deviation = sd(Distance, na.rm = TRUE)
  )
mf_stats <- mf_data %>%
  group_by(Learning_Environment) %>%
  summarise(
    Mean_Distance = mean(Distance, na.rm = TRUE),
    Std_Deviation = sd(Distance, na.rm = TRUE)
  )
cat("Summary Statistics for Single-Floor Conditions:\n")
print(sf_stats)
cat("\nSummary Statistics for Multi-Floor Conditions:\n")
print(mf_stats)
summary_stats <- bind_rows(
  sf_stats %>% mutate(Complexity = "Single-Floor"),
  mf_stats %>% mutate(Complexity = "Multi-Floor")
)
# Plot for Single-Floor
ggplot(summary_stats %>% filter(Complexity == "Single-Floor"), 
       aes(x = Learning_Environment, y = Mean_Distance, fill = Learning_Environment)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Mean_Distance - Std_Deviation, 
                    ymax = Mean_Distance + Std_Deviation),
                width = 0.2, size = 0.8) +
  labs(title = "Mean Total Distance for Single-Floor",
       x = "Learning Environment", 
       y = "Mean Total Distance (meters)") +
  scale_fill_manual(values = c("VR" = "#1f77b4", "R" = "#ff7f0e")) +
  theme_minimal() +
  theme(text = element_text(size = 14))
# Plot for Multi-Floor
ggplot(summary_stats %>% filter(Complexity == "Multi-Floor"), 
       aes(x = Learning_Environment, y = Mean_Distance, fill = Learning_Environment)) +
  geom_bar(stat = "identity", width = 0.6, color = "black") +
  geom_errorbar(aes(ymin = Mean_Distance - Std_Deviation, 
                    ymax = Mean_Distance + Std_Deviation),
                width = 0.2, size = 0.8) +
  labs(title = "Mean Total Distance for Multi-Floor",
       x = "Learning Environment", 
       y = "Mean Total Distance (meters)") +
  scale_fill_manual(values = c("VR" = "#1f77b4", "R" = "#ff7f0e")) +
  theme_minimal() +
  theme(text = element_text(size = 14))
# Mann-Whitney U Test for Single-Floor
mann_whitney_result_sf <- wilcox.test(Distance ~ Learning_Environment, data = sf_data)
cat("\nMann-Whitney U Test Result for Single-Floor:\n")
print(mann_whitney_result_sf)
# Shapiro-Wilk normality test for Single-Floor
shapiro_vr_sf <- shapiro.test(sf_data$Distance[sf_data$Learning_Environment == "VR"])
shapiro_r_sf <- shapiro.test(sf_data$Distance[sf_data$Learning_Environment == "R"])
cat("\nShapiro-Wilk Normality Test for Single-Floor VR Group:\n")
print(shapiro_vr_sf)
cat("\nShapiro-Wilk Normality Test for Single-Floor R Group:\n")
print(shapiro_r_sf)
# Shapiro-Wilk normality test for Multi-Floor
shapiro_vr_mf <- shapiro.test(mf_data$Distance[mf_data$Learning_Environment == "VR"])
shapiro_r_mf <- shapiro.test(mf_data$Distance[mf_data$Learning_Environment == "R"])
cat("\nShapiro-Wilk Normality Test for Multi-Floor VR Group:\n")
print(shapiro_vr_mf)
cat("\nShapiro-Wilk Normality Test for Multi-Floor R Group:\n")
print(shapiro_r_mf)
# Levene's Test for Homogeneity of Variances for Single-Floor
levene_test_result_sf <- leveneTest(Distance ~ Learning_Environment, data = sf_data)
cat("\nLevene's Test for Homogeneity of Variances for Single-Floor:\n")
print(levene_test_result_sf)
#Independent Samples t-test for Multi-Floor
t_test_result_mf <- t.test(Distance ~ Learning_Environment, data = mf_data, var.equal = TRUE)
cat("\nIndependent Samples t-Test Result for Multi-Floor:\n")
print(t_test_result_mf)
