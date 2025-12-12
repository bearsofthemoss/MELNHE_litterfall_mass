# Load required libraries
library(mice)
library(VIM)
library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Load your data
lit.all <- read.csv(here::here("data", "MELNHE Litterfall mass_Nov2025.csv"))
litterfall_data <- lit.all

# Calculate litterfall per unit area
litterfall_data$Litterfall_gha <- as.numeric(litterfall_data$Total_Mass) / litterfall_data$Basket_Area

# Study design parameters
n_years <- 24  # 2004 to 2025
n_stands <- 13
n_plots <- 4
n_seasons <- 3
n_replicates <- 5
seasons <- c("Fall", "Spring", "Summer")

print("=== DATASET STRUCTURE ===")
print(paste("Total observations:", nrow(litterfall_data)))
print(paste("Years:", min(litterfall_data$Lityear, na.rm = TRUE), "to", 
            max(litterfall_data$Lityear, na.rm = TRUE)))
print(paste("Number of stands:", length(unique(litterfall_data$Stand))))
print(paste("Number of plots per stand:", length(unique(litterfall_data$Plot))))
print(paste("Seasons:", paste(unique(litterfall_data$Season), collapse = ", ")))
print(paste("Treatments:", paste(unique(litterfall_data$Treatment), collapse = ", ")))

# No zeros allowed for Fall - convert to NA for imputation
fall_zeros <- which(litterfall_data$Season == "Fall" & 
                      !is.na(litterfall_data$Litterfall_gha) & 
                      litterfall_data$Litterfall_gha == 0)
if(length(fall_zeros) > 0) {
  litterfall_data$Litterfall_gha[fall_zeros] <- NA
  print(paste("\nConverted", length(fall_zeros), "fall zero values to NA for imputation"))
}

# Create additional variables for imputation
litterfall_data$YearCentered <- litterfall_data$Lityear - median(litterfall_data$Lityear, na.rm = TRUE)
litterfall_data$StandFactor <- as.factor(litterfall_data$Stand)
litterfall_data$PlotFactor <- as.factor(litterfall_data$Plot)
litterfall_data$SeasonFactor <- as.factor(litterfall_data$Season)
litterfall_data$TreatmentFactor <- as.factor(litterfall_data$Treatment)
litterfall_data$Stand_Plot <- interaction(litterfall_data$Stand, litterfall_data$Plot)

print("\n=== MISSING DATA OVERVIEW ===")
print(paste("Missing values (NA):", sum(is.na(litterfall_data$Litterfall_gha))))
print(paste("True zeros:", sum(litterfall_data$Litterfall_gha == 0, na.rm = TRUE)))
print(paste("Complete observations:", sum(!is.na(litterfall_data$Litterfall_gha) & 
                                            litterfall_data$Litterfall_gha > 0)))
print(paste("Percent missing:", round(100 * sum(is.na(litterfall_data$Litterfall_gha)) / 
                                        nrow(litterfall_data), 1), "%"))

# Detailed missing data analysis by season
print("\n=== MISSING DATA PATTERNS BY SEASON ===")
missing_by_season <- litterfall_data %>%
  group_by(Season) %>%
  summarise(
    Total_obs = n(),
    Missing_NA = sum(is.na(Litterfall_gha)),
    True_zeros = sum(Litterfall_gha == 0, na.rm = TRUE),
    Complete_obs = sum(!is.na(Litterfall_gha) & Litterfall_gha > 0),
    Percent_missing = round(100 * Missing_NA / Total_obs, 1),
    Percent_zeros = round(100 * True_zeros / Total_obs, 1),
    Mean_litterfall = round(mean(Litterfall_gha, na.rm = TRUE), 1),
    SD_litterfall = round(sd(Litterfall_gha, na.rm = TRUE), 1),
    .groups = 'drop'
  )

print(missing_by_season)

# Missing data by stand
print("\n=== MISSING DATA PATTERNS BY STAND ===")
missing_by_stand <- litterfall_data %>%
  group_by(Stand) %>%
  summarise(
    Total_obs = n(),
    Missing_NA = sum(is.na(Litterfall_gha)),
    Percent_missing = round(100 * Missing_NA / Total_obs, 1),
    Mean_litterfall = round(mean(Litterfall_gha, na.rm = TRUE), 1),
    .groups = 'drop'
  ) %>%
  arrange(desc(Percent_missing))

print(missing_by_stand)

# Missing data by stand and season
print("\n=== MISSING DATA BY STAND AND SEASON ===")
missing_by_stand_season <- litterfall_data %>%
  group_by(Stand, Season) %>%
  summarise(
    Total_obs = n(),
    Missing_NA = sum(is.na(Litterfall_gha)),
    Percent_missing = round(100 * Missing_NA / Total_obs, 1),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = Season, values_from = c(Missing_NA, Percent_missing), 
              names_sep = "_") %>%
  arrange(desc(Percent_missing_Fall + Percent_missing_Spring + Percent_missing_Summer))

print("Stands ranked by total missing data:")
print(missing_by_stand_season)

# Create visualization of missing patterns
print("\n=== VISUALIZING MISSING PATTERNS ===")

# Missing data heatmap by stand and season
missing_plot_data <- litterfall_data %>%
  group_by(Stand, Season) %>%
  summarise(
    Percent_missing = 100 * sum(is.na(Litterfall_gha)) / n(),
    .groups = 'drop'
  )

missing_heatmap <- ggplot(missing_plot_data, aes(x = factor(Stand), y = Season, fill = Percent_missing)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(low = "lightblue", mid = "yellow", high = "red", 
                       midpoint = 15, name = "% Missing") +
  geom_text(aes(label = round(Percent_missing, 0)), size = 3) +
  labs(
    title = "Missing Data Patterns by Stand and Season",
    subtitle = "Numbers show percentage of missing observations",
    x = "Stand",
    y = "Season"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(missing_heatmap)

# Time series of missing data
missing_by_year <- litterfall_data %>%
  group_by(Lityear, Season) %>%
  summarise(
    Percent_missing = 100 * sum(is.na(Litterfall_gha)) / n(),
    .groups = 'drop'
  )

missing_time_plot <- ggplot(missing_by_year, aes(x = Lityear, y = Percent_missing, color = Season)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "Missing Data Rates Over Time",
    subtitle = "Shows if data collection improved or worsened over time",
    x = "Litterfall Year",
    y = "Percent Missing (%)",
    color = "Season"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(missing_time_plot)

# Identify most problematic stand-season combinations
print("\n=== MOST PROBLEMATIC STAND-SEASON COMBINATIONS ===")
problematic_combinations <- litterfall_data %>%
  group_by(Stand, Season) %>%
  summarise(
    Total_obs = n(),
    Missing_count = sum(is.na(Litterfall_gha)),
    Percent_missing = round(100 * Missing_count / Total_obs, 1),
    Years_affected = length(unique(Lityear[is.na(Litterfall_gha)])),
    .groups = 'drop'
  ) %>%
  filter(Percent_missing > 20) %>%
  arrange(desc(Percent_missing))

if(nrow(problematic_combinations) > 0) {
  print("Stand-Season combinations with >20% missing data:")
  print(problematic_combinations)
} else {
  print("No stand-season combinations have >20% missing data")
}

# Statistical tests for missing data patterns
print("\n=== STATISTICAL TESTS FOR MISSING DATA PATTERNS ===")

# Chi-square test for association between season and missingness
missing_table <- table(litterfall_data$Season, is.na(litterfall_data$Litterfall_gha))
colnames(missing_table) <- c("Complete", "Missing")
print("Contingency table - Season vs Missing:")
print(missing_table)

chi_test <- chisq.test(missing_table)
print(paste("Chi-square test p-value:", round(chi_test$p.value, 4)))
if(chi_test$p.value < 0.05) {
  print("SIGNIFICANT difference in missing rates between seasons")
} else {
  print("No significant difference in missing rates between seasons")
}

# Visualize missing data patterns
VIM::aggr(litterfall_data[c("Litterfall_gha", "Lityear", "Season", "Stand", "Treatment")], 
          col = c('navyblue', 'red'), 
          numbers = TRUE, 
          sortVars = TRUE,
          labels = c("Litterfall", "Year", "Season", "Stand", "Treatment"))

# Prepare data for MICE imputation
print("\n=== PREPARING DATA FOR MICE IMPUTATION ===")

# Select variables for imputation
imputation_data <- litterfall_data %>%
  select(Litterfall_gha, Lityear, YearCentered, Stand, Plot, Season, Treatment,
         StandFactor, PlotFactor, SeasonFactor, TreatmentFactor, Stand_Plot)

# Remove any rows where all key variables are missing
imputation_data <- imputation_data[!is.na(imputation_data$Lityear), ]

print(paste("Observations for imputation:", nrow(imputation_data)))
print(paste("Variables for imputation:", ncol(imputation_data)))

# Initialize mice to see the default methods
print("\n=== SETTING UP MICE IMPUTATION ===")
init_mice <- mice(imputation_data, maxit = 0)
print("Default imputation methods:")
print(init_mice$method)

# Customize imputation methods
methods <- init_mice$method
methods["Litterfall_gha"] <- "pmm"  # Predictive mean matching

# Customize predictor matrix
pred_matrix <- init_mice$predictorMatrix

# Don't use these as predictors (redundant or not meaningful)
pred_matrix[, "YearCentered"] <- 0
pred_matrix["Lityear", "YearCentered"] <- 0
pred_matrix["YearCentered", "Lityear"] <- 0

# Stand_Plot is interaction term, use sparingly
pred_matrix["Stand_Plot", ] <- 0

print("Predictor matrix customized")

# Perform multiple imputation
print("\n=== PERFORMING MICE IMPUTATION ===")
print("This may take a minute with real data...")

mice_result <- mice(imputation_data, 
                    method = methods,
                    predictorMatrix = pred_matrix,
                    m = 5,           # Create 5 imputed datasets
                    maxit = 10,      # 10 iterations
                    seed = 123,
                    printFlag = FALSE)

print("MICE imputation completed!")
print(mice_result)

# Check convergence
print("\n=== CHECKING CONVERGENCE ===")
plot(mice_result, main = "Convergence Plot - Check for stable chains")

# Examine the imputed values
print("\n=== EXAMINING IMPUTED VALUES ===")
densityplot(mice_result, ~Litterfall_gha, main = "Distribution: Observed (blue) vs Imputed (red)")
stripplot(mice_result, Litterfall_gha ~ .imp, pch = 20, cex = 1.2,
          main = "Imputed Values by Dataset")

print("\n=== CROSS-VALIDATION OF IMPUTATION ACCURACY ===")

# Function to perform cross-validation
validate_mice_imputation <- function(data, prop_holdout = 0.1, n_folds = 5) {
  
  # Only use complete cases for validation
  complete_data <- data[!is.na(data$Litterfall_gha), ]
  
  print(paste("Using", nrow(complete_data), "complete observations for cross-validation"))
  
  validation_results <- data.frame()
  
  for(fold in 1:n_folds) {
    set.seed(fold * 100)
    
    # Stratify by season
    holdout_indices <- c()
    for(season in unique(complete_data$Season)) {
      season_data <- complete_data[complete_data$Season == season, ]
      season_holdout <- sample(nrow(season_data), 
                               size = max(1, round(nrow(season_data) * prop_holdout)))
      holdout_indices <- c(holdout_indices, which(complete_data$Season == season)[season_holdout])
    }
    
    # Create validation dataset
    validation_data <- complete_data
    true_values <- validation_data$Litterfall_gha[holdout_indices]
    validation_data$Litterfall_gha[holdout_indices] <- NA
    
    # Store information
    holdout_info <- validation_data[holdout_indices, c("Season", "Stand", "Plot", "Lityear", "Treatment")]
    holdout_info$True_value <- true_values
    holdout_info$Fold <- fold
    
    # Run MICE on validation data
    mice_val <- mice(validation_data[c("Litterfall_gha", "YearCentered", "StandFactor", 
                                       "PlotFactor", "SeasonFactor", "TreatmentFactor")], 
                     m = 3, maxit = 10, printFlag = FALSE, seed = fold * 111)
    
    # Get imputed values
    for(imp in 1:3) {
      complete_val_data <- complete(mice_val, imp)
      imputed_values <- complete_val_data$Litterfall_gha[holdout_indices]
      
      fold_results <- holdout_info
      fold_results$Imputed_value <- imputed_values
      fold_results$Imputation <- imp
      fold_results$Absolute_error <- abs(imputed_values - true_values)
      fold_results$Relative_error <- abs(imputed_values - true_values) / true_values * 100
      fold_results$Squared_error <- (imputed_values - true_values)^2
      
      validation_results <- rbind(validation_results, fold_results)
    }
    
    print(paste("Completed fold", fold, "of", n_folds))
  }
  
  return(validation_results)
}

# Run cross-validation
print("Running cross-validation...")
cv_results <- validate_mice_imputation(imputation_data, prop_holdout = 0.15, n_folds = 5)

# Analyze results
print("\n=== CROSS-VALIDATION RESULTS ===")

overall_metrics <- cv_results %>%
  summarise(
    N_predictions = n(),
    Mean_Absolute_Error = round(mean(Absolute_error, na.rm = TRUE), 2),
    Median_Absolute_Error = round(median(Absolute_error, na.rm = TRUE), 2),
    RMSE = round(sqrt(mean(Squared_error, na.rm = TRUE)), 2),
    Mean_Relative_Error_Pct = round(mean(Relative_error[Relative_error < 200], na.rm = TRUE), 1),
    Median_Relative_Error_Pct = round(median(Relative_error, na.rm = TRUE), 1)
  )

print("Overall imputation accuracy:")
print(overall_metrics)

# Accuracy by season
season_metrics <- cv_results %>%
  group_by(Season) %>%
  summarise(
    N_predictions = n(),
    Mean_True_Value = round(mean(True_value, na.rm = TRUE), 2),
    Mean_Imputed_Value = round(mean(Imputed_value, na.rm = TRUE), 2),
    Mean_Absolute_Error = round(mean(Absolute_error, na.rm = TRUE), 2),
    Median_Absolute_Error = round(median(Absolute_error, na.rm = TRUE), 2),
    RMSE = round(sqrt(mean(Squared_error, na.rm = TRUE)), 2),
    Mean_Relative_Error_Pct = round(mean(Relative_error[Relative_error < 200], na.rm = TRUE), 1),
    Median_Relative_Error_Pct = round(median(Relative_error, na.rm = TRUE), 1),
    .groups = 'drop'
  )

print("\nAccuracy by season:")
print(season_metrics)

# Visualizations
validation_scatter <- ggplot(cv_results, aes(x = True_value, y = Imputed_value, color = Season)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", size = 1) +
  facet_wrap(~Season) +
  labs(
    title = "Cross-Validation: True vs Imputed Values",
    subtitle = "Points near diagonal indicate good accuracy",
    x = "True Value (g/m²)",
    y = "Imputed Value (g/m²)",
    color = "Season"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(validation_scatter)

error_boxplot <- ggplot(cv_results, aes(x = Season, y = Relative_error, fill = Season)) +
  geom_boxplot(alpha = 0.7) +
  coord_cartesian(ylim = c(0, 100)) +
  geom_hline(yintercept = c(10, 20, 50), linetype = "dashed", alpha = 0.5) +
  labs(
    title = "Distribution of Relative Errors by Season",
    x = "Season",
    y = "Relative Error (%)",
    fill = "Season"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(error_boxplot)

# Bias analysis
bias_analysis <- cv_results %>%
  group_by(Season) %>%
  summarise(
    Mean_Bias = round(mean(Imputed_value - True_value, na.rm = TRUE), 2),
    Median_Bias = round(median(Imputed_value - True_value, na.rm = TRUE), 2),
    .groups = 'drop'
  )

print("\nBias analysis (positive = over-prediction, negative = under-prediction):")
print(bias_analysis)

# Analyze temporal trends with imputed data
print("\n=== ANALYZING TEMPORAL TRENDS WITH IMPUTED DATA ===")

trend_models <- with(mice_result, lm(Litterfall_gha ~ YearCentered + SeasonFactor + 
                                       TreatmentFactor + StandFactor))

pooled_trend <- pool(trend_models)
summary_trend <- summary(pooled_trend)

print("Pooled trend analysis results:")
print(summary_trend[summary_trend$term == "YearCentered", ])

year_coef <- summary_trend[summary_trend$term == "YearCentered", ]
print(paste("\nAnnual trend in litterfall:", round(year_coef$estimate, 3), "±", 
            round(year_coef$std.error, 3), "g/m² per year"))
 print(paste("P-value for trend:", round(year_coef$p.value, 4)))

if(year_coef$p.value < 0.05) {
  if(year_coef$estimate > 0) {
    print("SIGNIFICANT INCREASING trend in litterfall over time")
  } else {
    print("SIGNIFICANT DECREASING trend in litterfall over time")
  }
} else {
  print("No significant temporal trend detected")
}

# Create visualization
complete_data1 <- complete(mice_result, 1)

annual_means <- complete_data1 %>%
  group_by(Lityear, Season) %>%
  summarise(
    Mean_Litterfall = mean(Litterfall_gha, na.rm = TRUE),
    SE = sd(Litterfall_gha, na.rm = TRUE) / sqrt(n()),
    .groups = 'drop'
  )

trend_plot <- ggplot(annual_means, aes(x = Lityear, y = Mean_Litterfall, color = Season)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.3) +
  facet_wrap(~Season, scales = "free_y") +
  labs(
    title = "Litterfall Trends Over Time by Season",
    subtitle = "Based on MICE-imputed data",
    x = "Litterfall Year",
    y = "Litterfall (g/m²)",
    color = "Season"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

print(trend_plot)

print("\n=== ANALYSIS COMPLETE ===")
print("Summary:")
print(paste("- Total observations:", nrow(litterfall_data)))
print(paste("- Missing rate:", round(100 * sum(is.na(litterfall_data$Litterfall_gha)) / nrow(litterfall_data), 1), "%"))
print(paste("- Cross-validation relative error:", round(overall_metrics$Median_Relative_Error_Pct, 1), "%"))
print(paste("- Annual trend:", round(year_coef$estimate, 3), "g/m²/year, p =", round(year_coef$p.value, 4)))

