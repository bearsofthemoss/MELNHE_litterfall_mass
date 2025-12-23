library(ggplot2)
library(here)


#####
lit <- read.csv(here::here("data","MELNHE Litterfall mass_Nov2025.csv"))

lit$Total_Mass <- as.numeric(lit$Total_Mass_g_m2)
lit <- lit[lit$Season=="Fall",]

# Load your data

# ===== STEP 1: Create basket ID and identify species columns =====
lit$basket_id <- paste(lit$Stand, lit$Plot, lit$Basket, sep = "_")

# Identify species columns (adjust pattern if needed)
# Assuming species columns are uppercase codes like ACPE, ACRU, ACSA3, ASSP2
species_cols <- grep("^[A-Z]{2,4}[0-9]*$", names(lit), value = TRUE)

# Or manually specify if the pattern doesn't work:
# species_cols <- c("ACPE", "ACRU", "ACSA3", "ASSP2", ...)
for(col in species_cols) {
  lit[[col]] <- as.numeric(lit[[col]])
}

# ===== STEP 2: Calculate sorted mass and determine basket type =====
# Calculate total sorted mass (sum of all species)
lit$sorted_mass <- rowSums(lit[, species_cols], na.rm = TRUE)

# Check if basket was sorted (has at least one non-NA species value)
lit$is_sorted <- rowSums(!is.na(lit[, species_cols])) > 0

# Check if basket was weighed whole
lit$is_whole <- !is.na(lit$Total_Mass) & !lit$is_sorted

# Count species richness
lit$species_richness <- rowSums(lit[, species_cols] > 0, na.rm = TRUE)

# Quick check
table(lit$is_sorted)
table(lit$is_whole)


# ===== STEP 3: Calculate expected ratios for each basket =====
# Get unique basket IDs
unique_baskets <- unique(lit$basket_id)

# Create empty vectors to store results
basket_id_vec <- character()
n_sorted_vec <- numeric()
n_whole_vec <- numeric()
mean_sorted_vec <- numeric()
sd_sorted_vec <- numeric()
mean_whole_vec <- numeric()
sd_whole_vec <- numeric()
expected_ratio_vec <- numeric()

# Loop through each basket
for(b in unique_baskets) {
  basket_data <- lit[lit$basket_id == b, ]
  
  n_sorted <- sum(basket_data$is_sorted, na.rm = TRUE)
  n_whole <- sum(basket_data$is_whole, na.rm = TRUE)
  
  # Only calculate ratios if we have both types
  if(n_sorted > 0 & n_whole > 0) {
    mean_sorted <- mean(basket_data$sorted_mass[basket_data$is_sorted], na.rm = TRUE)
    sd_sorted <- sd(basket_data$sorted_mass[basket_data$is_sorted], na.rm = TRUE)
    mean_whole <- mean(basket_data$Total_Mass[basket_data$is_whole], na.rm = TRUE)
    sd_whole <- sd(basket_data$Total_Mass[basket_data$is_whole], na.rm = TRUE)
    expected_ratio <- mean_sorted / mean_whole
    
    basket_id_vec <- c(basket_id_vec, b)
    n_sorted_vec <- c(n_sorted_vec, n_sorted)
    n_whole_vec <- c(n_whole_vec, n_whole)
    mean_sorted_vec <- c(mean_sorted_vec, mean_sorted)
    sd_sorted_vec <- c(sd_sorted_vec, sd_sorted)
    mean_whole_vec <- c(mean_whole_vec, mean_whole)
    sd_whole_vec <- c(sd_whole_vec, sd_whole)
    expected_ratio_vec <- c(expected_ratio_vec, expected_ratio)
  }
}

# Create summary dataframe
basket_ratios <- data.frame(
  basket_id = basket_id_vec,
  n_sorted = n_sorted_vec,
  n_whole = n_whole_vec,
  mean_sorted = mean_sorted_vec,
  sd_sorted = sd_sorted_vec,
  mean_whole = mean_whole_vec,
  sd_whole = sd_whole_vec,
  expected_ratio = expected_ratio_vec
)

# View baskets with ratio data
head(basket_ratios)
summary(basket_ratios$expected_ratio)


# ===== STEP 4: Merge ratio data back to main dataset =====
lit <- merge(lit, basket_ratios, by = "basket_id", all.x = TRUE)


# ===== STEP 5: Calculate risk scores for sorted baskets =====
lit$expected_sorted <- NA
lit$deviation_pct <- NA
lit$z_score <- NA
lit$risk_flag <- ""

for(i in 1:nrow(lit)) {
  if(lit$is_sorted[i] & !is.na(lit$expected_ratio[i])) {
    # Expected sorted mass based on this basket's historical ratio
    lit$expected_sorted[i] <- lit$expected_ratio[i] * lit$mean_whole[i]
    
    # Percent deviation from expected
    lit$deviation_pct[i] <- ((lit$sorted_mass[i] - lit$expected_sorted[i]) / lit$expected_sorted[i]) * 100
    
    # Z-score (how many standard deviations below expected)
    if(!is.na(lit$sd_sorted[i]) & lit$sd_sorted[i] > 0) {
      lit$z_score[i] <- (lit$sorted_mass[i] - lit$mean_sorted[i]) / lit$sd_sorted[i]
    }
    
    # Flag suspicious baskets
    if(!is.na(lit$deviation_pct[i]) & lit$deviation_pct[i] < -30) {
      lit$risk_flag[i] <- "HIGH RISK: >30% below expected"
    } else if(!is.na(lit$deviation_pct[i]) & lit$deviation_pct[i] < -20) {
      lit$risk_flag[i] <- "MEDIUM RISK: 20-30% below expected"
    } else if(!is.na(lit$z_score[i]) & lit$z_score[i] < -2) {
      lit$risk_flag[i] <- "CAUTION: >2 SD below basket mean"
    }
  }
}


# ===== STEP 6: Create summary of flagged baskets =====
flagged <- lit[lit$risk_flag != "" & lit$is_sorted, 
               c("Lityear", "Stand", "Plot", "Basket", "basket_id", 
                 "sorted_mass", "expected_sorted", "deviation_pct", 
                 "z_score", "species_richness", "risk_flag")]

flagged <- flagged[order(flagged$deviation_pct), ]

print("=== FLAGGED BASKETS (potentially missing species) ===")

# Export to CSV
write.csv(flagged, "flagged_baskets.csv", row.names = FALSE)


# ===== STEP 7: Visualizations =====

# Plot 1: Time series for each basket (faceted)
sorted_data <- lit[lit$is_sorted, ]

# Select a few baskets to visualize (or do all if not too many)
baskets_to_plot <- unique(sorted_data$basket_id)[1:min(12, length(unique(sorted_data$basket_id)))]
plot_data <- sorted_data[sorted_data$basket_id %in% baskets_to_plot, ]

ggplot(plot_data, aes(x = Lityear, y = sorted_mass)) +
  geom_line() +
  geom_point(aes(color = risk_flag != ""), size = 3) +
  geom_hline(aes(yintercept = mean_sorted), linetype = "dashed", color = "blue") +
  facet_wrap(~basket_id, scales = "free_y") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     labels = c("Normal", "Flagged"),
                     name = "") +
  labs(title = "Sorted Mass Over Time by Basket",
       subtitle = "Dashed line = basket mean, Red points = flagged years",
       x = "Year", y = "Sorted Mass (g)") +
  theme_bw()

ggsave("basket_timeseries.png", width = 12, height = 8)


# Plot 2: Deviation from expected for all sorted baskets
ggplot(lit[lit$is_sorted & !is.na(lit$deviation_pct), ], 
       aes(x = Lityear, y = deviation_pct, color = basket_id)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_hline(yintercept = c(-30, -20, 0), linetype = c("solid", "dashed", "solid"), 
             color = c("red", "orange", "black")) +
  labs(title = "Deviation from Expected Sorted Mass",
       subtitle = "Red line = -30% (high risk), Orange line = -20% (medium risk)",
       x = "Year", y = "% Deviation from Expected") +
  theme_bw() +
  theme(legend.position = "none")

ggsave("deviation_scatter.png", width = 10, height = 6)


# Plot 3: Species richness over time
ggplot(lit[lit$is_sorted, ], aes(x = Lityear, y = species_richness)) +
  geom_jitter(aes(color = risk_flag != ""), width = 0.2, height = 0, size = 2, alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "blue") +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black"),
                     labels = c("Normal", "Flagged"),
                     name = "") +
  labs(title = "Species Richness Over Time",
       subtitle = "Red points = flagged baskets",
       x = "Year", y = "Number of Species Detected") +
  theme_bw()

ggsave("species_richness.png", width = 10, height = 6)


# ===== STEP 8: Interactive review of specific baskets =====
# Create a function-free way to look at specific baskets
review_basket <- "1_1_1"  # Change this to review different baskets

basket_review <- lit[lit$basket_id == review_basket, 
                     c("Lityear", "basket_id", "is_sorted", "is_whole", 
                       "sorted_mass", "Total_Mass", "species_richness", 
                       "expected_sorted", "deviation_pct", "risk_flag")]
basket_review <- basket_review[order(basket_review$Lityear), ]

print(paste("=== REVIEW OF BASKET:", review_basket, "==="))
print(basket_review)

# Plot this specific basket
basket_plot_data <- lit[lit$basket_id == review_basket, ]

plot(basket_plot_data$Lityear, basket_plot_data$sorted_mass, 
     type = "b", pch = 19, col = "black",
     main = paste("Basket", review_basket, "- Sorted Mass Over Time"),
     xlab = "Year", ylab = "Sorted Mass (g)",
     ylim = c(0, max(basket_plot_data$sorted_mass, na.rm = TRUE) * 1.2))

# Add flagged points in red
flagged_years <- basket_plot_data$risk_flag != "" & !is.na(basket_plot_data$risk_flag)
points(basket_plot_data$Lityear[flagged_years], 
       basket_plot_data$sorted_mass[flagged_years],
       pch = 19, col = "red", cex = 1.5)

# Add mean line
if(!is.na(basket_plot_data$mean_sorted[1])) {
  abline(h = basket_plot_data$mean_sorted[1], lty = 2, col = "blue")
  legend("topright", legend = c("Observed", "Flagged", "Mean"), 
         col = c("black", "red", "blue"), pch = c(19, 19, NA), lty = c(NA, NA, 2))
}


print("=== ANALYSIS COMPLETE ===")
print(paste("Total baskets:", length(unique(lit$basket_id))))
print(paste("Baskets with ratio data:", nrow(basket_ratios)))
print(paste("Flagged observations:", nrow(flagged)))
