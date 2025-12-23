

# ============================================================================
# MICE Cross-Validation for Ecological Data - Base R Style
# ============================================================================
#####
lit <- read.csv(here::here("data","MELNHE Litterfall mass_Nov2025.csv"))


## How predictable is the seasonal litterfall in 13 stands?

lit$Total_Mass <- as.numeric(lit$Total_Mass_g_m2)
sealit <- lit[ ,c(2,4,6,7,12,13)]
sealit$plot_id <- paste(sealit$Lityear, sealit$Stand, sealit$Plot, sealit$Basket)

# Deal with duplicated Center basket
sealit <- sealit[-7309,]
sealit <- tidyr::spread( sealit,  "Season","Total_Mass")


head(sealit)


library(mice)
library(dplyr)
library(tidyr)
library(ggplot2)
# ============================================================================
# Simple MICE Imputation Example - Step by Step
# ============================================================================

# Step 1: Aggregate to plot level
# ----------------------------------------------------------------------------
plot_data <- sealit %>%
  group_by(Stand, Lityear, Plot) %>%
  summarise(
    Summer = mean(Summer, na.rm = TRUE),
    Fall = mean(Fall, na.rm = TRUE),
    Spring = mean(Spring, na.rm = TRUE),
    n_baskets = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Summer = ifelse(is.nan(Summer), NA, Summer),
    Fall = ifelse(is.nan(Fall), NA, Fall),
    Spring = ifelse(is.nan(Spring), NA, Spring)
  )

# Convert to data frame
plot_data <- as.data.frame(plot_data)

# Create unique plot ID
plot_data$plot_id <- paste(plot_data$Lityear, plot_data$Stand, plot_data$Plot, sep = "_")




plot_data <- plot_data[plot_data$Fall>0, ]
library(mice)
library(dplyr)
library(tidyr)
library(ggplot2)

# ============================================================================
# MICE Cross-Validation with STAND-SPECIFIC Imputation
# ============================================================================

# Parameters
n_folds <- 5
prop_mask <- 0.2  # Proportion of observed values to mask
m_imputations <- 5
maxit <- 10
seasons <- c("Summer", "Fall", "Spring")

# Only use rows with at least one observed season value
complete_data <- plot_data[!is.na(plot_data$Summer) | 
                             !is.na(plot_data$Fall) | 
                             !is.na(plot_data$Spring), ]

# Create folds stratified by Stand
set.seed(123)
# Create folds within each stand to maintain stand representation
complete_data$fold <- NA
unique_stands <- unique(complete_data$Stand)

for (stand in unique_stands) {
  stand_rows <- which(complete_data$Stand == stand)
  n_stand_rows <- length(stand_rows)
  complete_data$fold[stand_rows] <- sample(rep(1:n_folds, length.out = n_stand_rows))
}

# Storage for all results
all_comparisons <- data.frame(
  fold = numeric(),
  season = character(),
  actual = numeric(),
  imputed = numeric(),
  stand = character(),
  year = numeric(),
  plot = character(),
  plot_id = character(),
  stringsAsFactors = FALSE
)

cat("Starting", n_folds, "fold cross-validation with STAND-SPECIFIC imputation...\n")
cat("Total stands:", length(unique_stands), "\n")

# ============================================================================
# Main Cross-Validation Loop
# ============================================================================

for (fold in 1:n_folds) {
  cat("\n=== Fold", fold, "of", n_folds, "===\n")
  
  # Split data into train and test
  test_fold <- complete_data[complete_data$fold == fold, ]
  train_fold <- complete_data[complete_data$fold != fold, ]
  
  # Create a copy of test_fold to mask values
  test_masked <- test_fold
  
  # Store which values we masked
  masked_info <- list()
  
  # Create artificial missingness in test fold
  for (season in seasons) {
    observed_idx <- which(!is.na(test_fold[[season]]))
    n_to_mask <- floor(length(observed_idx) * prop_mask)
    
    if (n_to_mask > 0) {
      mask_idx <- sample(observed_idx, n_to_mask)
      
      masked_info[[season]] <- data.frame(
        idx = mask_idx,
        actual_value = test_fold[[season]][mask_idx],
        stand = test_fold$Stand[mask_idx],
        year = test_fold$Lityear[mask_idx],
        plot = test_fold$Plot[mask_idx],
        plot_id = test_fold$plot_id[mask_idx],
        stringsAsFactors = FALSE
      )
      
      test_masked[[season]][mask_idx] <- NA
      cat("  Masked", n_to_mask, season, "values across stands\n")
    } else {
      masked_info[[season]] <- data.frame()
    }
  }
  
  # Combine train and masked test
  combined_data <- rbind(train_fold, test_masked)
  
  # ===========================================================================
  # STAND-SPECIFIC IMPUTATION
  # ===========================================================================
  # Instead of imputing all data at once, impute each stand separately
  # This respects stand-specific patterns and prevents cross-stand borrowing
  
  # Create storage for imputed data
  imputed_combined <- combined_data
  imputed_combined[, seasons] <- NA  # Will fill in stand by stand
  
  stands_in_combined <- unique(combined_data$Stand)
  
  cat("  Imputing", length(stands_in_combined), "stands separately...\n")
  
  for (stand in stands_in_combined) {
    stand_idx <- which(combined_data$Stand == stand)
    stand_data <- combined_data[stand_idx, ]
    
    # Check if this stand has any missing values to impute
    has_missing <- any(is.na(stand_data$Summer)) | 
      any(is.na(stand_data$Fall)) | 
      any(is.na(stand_data$Spring))
    
    if (!has_missing) {
      # No missing values in this stand, just copy the data
      imputed_combined[stand_idx, seasons] <- stand_data[, seasons]
      next
    }
    
    # Check if stand has enough data for imputation (need at least 3 rows)
    if (nrow(stand_data) < 3) {
      cat("    Stand", stand, "has too few observations (", nrow(stand_data), 
          "), using overall mean imputation\n")
      # Fall back to simple mean imputation for this stand
      for (season in seasons) {
        if (any(is.na(stand_data[[season]]))) {
          season_mean <- mean(stand_data[[season]], na.rm = TRUE)
          if (is.nan(season_mean)) {
            # If no values in this stand for this season, use grand mean
            season_mean <- mean(combined_data[[season]], na.rm = TRUE)
          }
          stand_data[[season]][is.na(stand_data[[season]])] <- season_mean
        }
      }
      imputed_combined[stand_idx, seasons] <- stand_data[, seasons]
      next
    }
    
    # Prepare data for MICE - use Lityear, Plot, and seasons
    mice_data <- stand_data[, c("Lityear", "Plot", "Summer", "Fall", "Spring")]
    
    # Run MICE imputation for this stand
    tryCatch({
      imp <- mice(mice_data, 
                  m = m_imputations, 
                  maxit = maxit, 
                  method = "pmm",
                  seed = 123 + fold + as.numeric(factor(stand)),
                  printFlag = FALSE)
      
      # Average across imputations
      imputed_sum <- mice_data
      imputed_sum[, seasons] <- 0
      
      for (i in 1:m_imputations) {
        completed_data <- complete(imp, action = i)
        imputed_sum[, seasons] <- imputed_sum[, seasons] + completed_data[, seasons]
      }
      
      imputed_avg <- imputed_sum[, seasons] / m_imputations
      imputed_combined[stand_idx, seasons] <- imputed_avg
      
    }, error = function(e) {
      cat("    ERROR in stand", stand, "- using mean imputation:", e$message, "\n")
      # Fall back to mean imputation
      for (season in seasons) {
        if (any(is.na(stand_data[[season]]))) {
          season_mean <- mean(stand_data[[season]], na.rm = TRUE)
          if (is.nan(season_mean)) {
            season_mean <- mean(combined_data[[season]], na.rm = TRUE)
          }
          stand_data[[season]][is.na(stand_data[[season]])] <- season_mean
        }
      }
      imputed_combined[stand_idx, seasons] <- stand_data[, seasons]
    })
  }
  
  cat("  Stand-specific imputation complete\n")
  
  # Extract just the test portion
  test_start <- nrow(train_fold) + 1
  test_end <- nrow(combined_data)
  imputed_test <- imputed_combined[test_start:test_end, ]
  
  # Compare imputed vs actual for each season
  fold_total <- 0
  for (season in seasons) {
    if (nrow(masked_info[[season]]) > 0) {
      
      local_idx <- masked_info[[season]]$idx
      imputed_values_for_masked <- imputed_test[[season]][local_idx]
      
      comparison <- data.frame(
        fold = fold,
        season = season,
        actual = masked_info[[season]]$actual_value,
        imputed = imputed_values_for_masked,
        stand = masked_info[[season]]$stand,
        year = masked_info[[season]]$year,
        plot = masked_info[[season]]$plot,
        plot_id = masked_info[[season]]$plot_id,
        stringsAsFactors = FALSE
      )
      
      all_comparisons <- rbind(all_comparisons, comparison)
      fold_total <- fold_total + nrow(comparison)
    }
  }
  
  cat("  Fold", fold, "completed: tested", fold_total, "imputations\n")
}

cat("\n============================================\n")
cat("Cross-validation complete!\n")
cat("Total imputations tested:", nrow(all_comparisons), "\n")
cat("============================================\n\n")

# ============================================================================
# Calculate Performance Metrics
# ============================================================================

# Overall metrics
overall_rmse <- sqrt(mean((all_comparisons$actual - all_comparisons$imputed)^2, na.rm = TRUE))
overall_mae <- mean(abs(all_comparisons$actual - all_comparisons$imputed), na.rm = TRUE)
overall_r2 <- cor(all_comparisons$actual, all_comparisons$imputed, use = "complete.obs")^2
overall_bias <- mean(all_comparisons$imputed - all_comparisons$actual, na.rm = TRUE)

cat("OVERALL PERFORMANCE METRICS:\n")
cat("  RMSE:      ", round(overall_rmse, 3), "\n")
cat("  MAE:       ", round(overall_mae, 3), "\n")
cat("  R-squared: ", round(overall_r2, 3), "\n")
cat("  Bias:      ", round(overall_bias, 3), "\n\n")

# Metrics by season
cat("METRICS BY SEASON:\n")
metrics_by_season <- data.frame(
  season = character(),
  n = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  R_squared = numeric(),
  Bias = numeric(),
  Mean_actual = numeric(),
  Mean_imputed = numeric(),
  stringsAsFactors = FALSE
)


for (i  in c(1:3)) {
  
  season_data <- all_comparisons[all_comparisons$season == seasons[i], ]
  
  if (nrow(season_data) > 0) {
    metrics_by_season <- rbind(metrics_by_season, data.frame(
      season = seasons[i],
      n = nrow(season_data),
      RMSE = sqrt(mean((season_data$actual - season_data$imputed)^2, na.rm = TRUE)),
      MAE = mean(abs(season_data$actual - season_data$imputed), na.rm = TRUE),
      R_squared = cor(season_data$actual, season_data$imputed, use = "complete.obs")^2,
      Bias = mean(season_data$imputed - season_data$actual, na.rm = TRUE),
      Mean_actual = mean(season_data$actual, na.rm = TRUE),
      Mean_imputed = mean(season_data$imputed, na.rm = TRUE)
    ))
  }
}

print(metrics_by_season)
cat("\n")

# Metrics by stand
cat("METRICS BY STAND:\n")
unique_stands_in_results <- unique(all_comparisons$stand)
metrics_by_stand <- data.frame(
  stand = character(),
  n = numeric(),
  RMSE = numeric(),
  MAE = numeric(),
  R_squared = numeric(),
  stringsAsFactors = FALSE
)

for (stand in unique_stands_in_results) {
  stand_data <- all_comparisons[all_comparisons$stand == stand, ]
  
  if (nrow(stand_data) > 1) {
    r2_val <- tryCatch(
      cor(stand_data$actual, stand_data$imputed, use = "complete.obs")^2,
      error = function(e) NA
    )
    
    metrics_by_stand <- rbind(metrics_by_stand, data.frame(
      stand = stand,
      n = nrow(stand_data),
      RMSE = sqrt(mean((stand_data$actual - stand_data$imputed)^2, na.rm = TRUE)),
      MAE = mean(abs(stand_data$actual - stand_data$imputed), na.rm = TRUE),
      R_squared = r2_val
    ))
  }
}

metrics_by_stand <- metrics_by_stand[order(-metrics_by_stand$RMSE), ]
print(metrics_by_stand)
cat("\n")

# ============================================================================
# Create Diagnostic Plots
# ============================================================================

# 1. Scatter plot: Actual vs Imputed by season
p1 <- ggplot(all_comparisons, aes(x = actual, y = imputed, color = season)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~season) +
  labs(title = "Cross-Validation: Actual vs Imputed (Stand-Specific Imputation)",
       subtitle = paste("RMSE =", round(overall_rmse, 2),
                        " | R² =", round(overall_r2, 3)),
       x = "Actual Value",
       y = "Imputed Value") +
  theme_bw() +
  theme(legend.position = "none")

print(p1)

# 2. Residual plot
all_comparisons$residual <- all_comparisons$imputed - all_comparisons$actual

p2 <- ggplot(all_comparisons, aes(x = actual, y = residual, color = season)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~season, scales = "free") +
  labs(title = "Residual Plot (Stand-Specific Imputation)",
       x = "Actual Value",
       y = "Residual (Imputed - Actual)") +
  theme_bw() +
  theme(legend.position = "none")

print(p2)

# 3. Distribution comparison
comp_long <- rbind(
  data.frame(season = all_comparisons$season, type = "actual", value = all_comparisons$actual),
  data.frame(season = all_comparisons$season, type = "imputed", value = all_comparisons$imputed)
)

p3 <- ggplot(comp_long, aes(x = value, fill = type)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~season, scales = "free") +
  labs(title = "Distribution: Actual vs Imputed (Stand-Specific)",
       x = "Value",
       y = "Density",
       fill = "Type") +
  theme_bw()

print(p3)

# 4. Metrics by season barplot
metrics_long <- rbind(
  data.frame(season = metrics_by_season$season, metric = "RMSE", value = metrics_by_season$RMSE),
  data.frame(season = metrics_by_season$season, metric = "MAE", value = metrics_by_season$MAE),
  data.frame(season = metrics_by_season$season, metric = "Bias", value = metrics_by_season$Bias)
)

p4 <- ggplot(metrics_long, aes(x = season, y = value, fill = season)) +
  geom_col() +
  facet_wrap(~metric, scales = "free_y") +
  labs(title = "Performance Metrics by Season (Stand-Specific)",
       x = "Season",
       y = "Value") +
  theme_bw() +
  theme(legend.position = "none")

print(p4)

# 5. Additional: Actual vs Imputed colored by Stand
p5 <- ggplot(all_comparisons, aes(x = actual, y = imputed, color = stand)) +
  geom_point(alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_wrap(~season, scales = "free") +
  labs(title = "Actual vs Imputed by Stand",
       x = "Actual Value",
       y = "Imputed Value",
       color = "Stand") +
  theme_bw()

print(p5)



#############################



Take 2???
  
  
  library(mice)
library(dplyr)

# ============================================================================
# Stand-Specific MICE Imputation for Complete Dataset
# ============================================================================

# Parameters
m_imputations <- 5
maxit <- 10
seasons <- c("Summer", "Fall", "Spring")

cat("Starting stand-specific imputation for full dataset...\n")
cat("Original data dimensions:", nrow(plot_data), "rows\n")

# Count missing values before imputation
cat("\nMissing values before imputation:\n")
for (season in seasons) {
  n_missing <- sum(is.na(plot_data[[season]]))
  pct_missing <- round(100 * n_missing / nrow(plot_data), 1)
  cat("  ", season, ":", n_missing, "(", pct_missing, "%)\n")
}

# Create a copy for imputation
imputed_data <- plot_data

# Get unique stands
unique_stands <- unique(plot_data$Stand)
cat("\nImputing", length(unique_stands), "stands separately...\n\n")

# Track imputation summary
imputation_summary <- data.frame(
  stand = character(),
  n_rows = numeric(),
  method = character(),
  summer_imputed = numeric(),
  fall_imputed = numeric(),
  spring_imputed = numeric(),
  stringsAsFactors = FALSE
)

# ============================================================================
# Loop through each stand and impute separately
# ============================================================================

for (stand in unique_stands) {
  cat("Processing Stand:", stand, "\n")
  
  # Extract data for this stand
  stand_idx <- which(plot_data$Stand == stand)
  stand_data <- plot_data[stand_idx, ]
  
  # Check if this stand has any missing values
  has_missing <- any(is.na(stand_data$Summer)) | 
    any(is.na(stand_data$Fall)) | 
    any(is.na(stand_data$Spring))
  
  if (!has_missing) {
    cat("  No missing values - skipping\n\n")
    imputation_summary <- rbind(imputation_summary, data.frame(
      stand = stand,
      n_rows = nrow(stand_data),
      method = "none_needed",
      summer_imputed = 0,
      fall_imputed = 0,
      spring_imputed = 0
    ))
    next
  }
  
  # Count missing values for this stand
  n_missing_summer <- sum(is.na(stand_data$Summer))
  n_missing_fall <- sum(is.na(stand_data$Fall))
  n_missing_spring <- sum(is.na(stand_data$Spring))
  
  cat("  Missing values: Summer =", n_missing_summer, 
      "| Fall =", n_missing_fall, 
      "| Spring =", n_missing_spring, "\n")
  
  # Check if stand has enough data for MICE (need at least 3 rows)
  if (nrow(stand_data) < 3) {
    cat("  Too few observations (", nrow(stand_data), 
        ") - using mean imputation\n\n")
    
    # Mean imputation
    for (season in seasons) {
      if (any(is.na(stand_data[[season]]))) {
        season_mean <- mean(stand_data[[season]], na.rm = TRUE)
        if (is.nan(season_mean)) {
          # If no values in this stand for this season, use grand mean
          season_mean <- mean(plot_data[[season]], na.rm = TRUE)
          cat("    Using grand mean for", season, ":", round(season_mean, 2), "\n")
        }
        stand_data[[season]][is.na(stand_data[[season]])] <- season_mean
      }
    }
    
    imputed_data[stand_idx, seasons] <- stand_data[, seasons]
    
    imputation_summary <- rbind(imputation_summary, data.frame(
      stand = stand,
      n_rows = nrow(stand_data),
      method = "mean",
      summer_imputed = n_missing_summer,
      fall_imputed = n_missing_fall,
      spring_imputed = n_missing_spring
    ))
    next
  }
  
  # Prepare data for MICE
  mice_data <- stand_data[, c("Lityear", "Plot", "Summer", "Fall", "Spring")]
  
  # Run MICE imputation
  tryCatch({
    cat("  Running MICE...\n")
    
    imp <- mice(mice_data, 
                m = m_imputations, 
                maxit = maxit, 
                method = "pmm",
                seed = 123 + as.numeric(factor(stand)),
                printFlag = FALSE)
    
    # Average across multiple imputations
    imputed_sum <- mice_data
    imputed_sum[, seasons] <- 0
    
    for (i in 1:m_imputations) {
      completed_data <- complete(imp, action = i)
      imputed_sum[, seasons] <- imputed_sum[, seasons] + completed_data[, seasons]
    }
    
    # Calculate average
    imputed_avg <- imputed_sum[, seasons] / m_imputations
    
    # Store imputed values back
    imputed_data[stand_idx, seasons] <- imputed_avg
    
    cat("  MICE complete\n\n")
    
    imputation_summary <- rbind(imputation_summary, data.frame(
      stand = stand,
      n_rows = nrow(stand_data),
      method = "MICE",
      summer_imputed = n_missing_summer,
      fall_imputed = n_missing_fall,
      spring_imputed = n_missing_spring
    ))
    
  }, error = function(e) {
    cat("  ERROR in MICE - falling back to mean imputation\n")
    cat("  Error message:", e$message, "\n\n")
    
    # Fall back to mean imputation
    for (season in seasons) {
      if (any(is.na(stand_data[[season]]))) {
        season_mean <- mean(stand_data[[season]], na.rm = TRUE)
        if (is.nan(season_mean)) {
          season_mean <- mean(plot_data[[season]], na.rm = TRUE)
        }
        stand_data[[season]][is.na(stand_data[[season]])] <- season_mean
      }
    }
    
    imputed_data[stand_idx, seasons] <- stand_data[, seasons]
    
    imputation_summary <<- rbind(imputation_summary, data.frame(
      stand = stand,
      n_rows = nrow(stand_data),
      method = "mean_fallback",
      summer_imputed = n_missing_summer,
      fall_imputed = n_missing_fall,
      spring_imputed = n_missing_spring
    ))
  })
}

# ============================================================================
# Summary and Verification
# ============================================================================

cat("============================================\n")
cat("IMPUTATION COMPLETE\n")
cat("============================================\n\n")

cat("Missing values after imputation:\n")
for (season in seasons) {
  n_missing <- sum(is.na(imputed_data[[season]]))
  cat("  ", season, ":", n_missing, "\n")
}
cat("\n")

# Summary by method
cat("Imputation methods used:\n")
method_summary <- table(imputation_summary$method)
print(method_summary)
cat("\n")

# Total values imputed
cat("Total values imputed by season:\n")
cat("  Summer:", sum(imputation_summary$summer_imputed), "\n")
cat("  Fall:  ", sum(imputation_summary$fall_imputed), "\n")
cat("  Spring:", sum(imputation_summary$spring_imputed), "\n")
cat("\n")

# Show imputation summary table
cat("Imputation summary by stand:\n")
print(imputation_summary)

# ============================================================================
# Save the imputed dataset
# ============================================================================

# The complete imputed dataset is now in: imputed_data

# You can save it:
# write.csv(imputed_data, "imputed_plot_data.csv", row.names = FALSE)

#Or compare original vs imputed:
comparison <- data.frame(
  plot_id = plot_data$plot_id,
  original_summer = plot_data$Summer,
  imputed_summer = imputed_data$Summer,
  original_fall = plot_data$Fall,
  imputed_fall = imputed_data$Fall,
  original_spring = plot_data$Spring,
  imputed_spring = imputed_data$Spring
)

comparison
# # Show only rows where imputation occurred
# imputed_rows <- comparison[is.na(comparison$original_summer) | 
#                            is.na(comparison$original_fall) | 
#                            is.na(comparison$original_spring), ]
# print(head(imputed_rows, 20))

imputed_data$total <- imputed_data$Fall+imputed_data$Summer+imputed_data$Spring


ggplot(imputed_data, aes(x= Lityear, y= total))+geom_point()+
  geom_line(aes(group=Plot))+facet_wrap(~Stand)+
  geom_smooth(method="lm")

anova(lm( total ~ Lityear+Stand, data=imputed_data))
