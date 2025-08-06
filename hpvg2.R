# Paired Categorical Analysis: Pre/Post Advertisement Study
# Q1: Yes/No responses, Q2: Yes/No/Maybe responses
# Including demographic breakdowns by age group and race

# Load required libraries
library(tidyverse)
library(ggplot2)
library(exact2x2)
library(coin)
library(vcd)
library(knitr)
library(DescTools)
library(viridis)
library(patchwork)

# Set seed for reproducibility

hpvg <- read.csv("D:/Users/bears/Downloads/HPV_GARD Study (Responses).csv")

hpvg$participant <- seq(1:dim(hpvg)[1])

hpvg[hpvg$Ethnicity=="African American/Black", "race"] <- "Black"

hpvg[hpvg$Ethnicity=="White/Caucasian", "race"] <- "White"

hpvg[is.na(hpvg$race), "race"] <- "Other"



# Generate sample data (replace this section with your actual data loading)
names(hpvg)

data <- hpvg[ , c("participant","age_group","race","q1_pre", "q1_post","q2_pre","q2_post")]

# Convert to factors with proper ordering
data$q1_pre <- factor(data$q1_pre, levels = c("No", "Yes"))
data$q1_post <- factor(data$q1_post, levels = c("No", "Yes"))
data$q2_pre <- factor(data$q2_pre, levels = c("No", "Maybe", "Yes"))
data$q2_post <- factor(data$q2_post, levels = c("No", "Maybe", "Yes"))

# Create change variables
data$q1_change <- ifelse(data$q1_pre == "No" & data$q1_post == "Yes", "No_to_Yes",
                         ifelse(data$q1_pre == "Yes" & data$q1_post == "No", "Yes_to_No", "No_Change"))

data$q2_change <- paste(data$q2_pre, "to", data$q2_post, sep = "_")

# Display first few rows
cat("Sample of the data:\n")
print(head(data))

cat("DESCRIPTIVE STATISTICS\n")

# Overall response distributions
cat("Question 1 (Yes/No) - Response Distribution:\n")
cat("Pre-advertisement:\n")
print(table(data$q1_pre))
print(prop.table(table(data$q1_pre)))

cat("\nPost-advertisement:\n")
print(table(data$q1_post))
print(prop.table(table(data$q1_post)))

cat("\nQuestion 2 (Yes/No/Maybe) - Response Distribution:\n")
cat("Pre-advertisement:\n")
print(table(data$q2_pre))
print(prop.table(table(data$q2_pre)))

cat("\nPost-advertisement:\n")
print(table(data$q2_post))
print(prop.table(table(data$q2_post)))

# Demographic breakdown
cat("\nDemographic Distribution:\n")
print(table(data$age_group))
print(table(data$race))

cat("\n" + rep("=", 60) + "\n")
cat("PAIRED CATEGORICAL ANALYSES\n")
cat(rep("=", 60) + "\n")

# Question 1: McNemar's Test for paired binary data
cat("QUESTION 1 ANALYSIS (Yes/No):\n")
cat("-" * 30 + "\n")

# Create contingency table for Q1
q1_table <- table(data$q1_pre, data$q1_post)
cat("Contingency Table (Pre vs Post):\n")
print(q1_table)

# McNemar's test
q1_mcnemar <- mcnemar.test(q1_table, correct = TRUE)
print(q1_mcnemar)

# Calculate effect size (odds ratio for discordant pairs)
if (q1_table[1,2] > 0 || q1_table[2,1] > 0) {
  q1_or <- q1_table[1,2] / q1_table[2,1]
  cat(sprintf("Odds Ratio (No->Yes / Yes->No): %.3f\n", q1_or))
}

# Show change patterns
cat("\nChange Patterns for Q1:\n")
change_table_q1 <- table(data$q1_change)
print(change_table_q1)
print(prop.table(change_table_q1))

# Question 2: Stuart-Maxwell test for paired ordinal data
cat("\nQUESTION 2 ANALYSIS (Yes/No/Maybe):\n")
cat("-" * 30 + "\n")

# Create contingency table for Q2
q2_table <- table(data$q2_pre, data$q2_post)
cat("Contingency Table (Pre vs Post):\n")
print(q2_table)

# Stuart-Maxwell test (extension of McNemar's test for >2 categories)
q2_symmetry <- symmetry_test(q2_post ~ q2_pre, data = data)
print(q2_symmetry)

# Marginal homogeneity test
cat("\nMarginal Homogeneity Test for Q2:\n")
q2_mh <- mh_test(q2_post ~ q2_pre, data = data)
print(q2_mh)

# Show change patterns
cat("\nChange Patterns for Q2:\n")
change_table_q2 <- table(data$q2_change)
print(change_table_q2)

cat("\n" + rep("=", 60) + "\n")
cat("DEMOGRAPHIC SUBGROUP ANALYSES\n")
cat(rep("=", 60) + "\n")

# Function to perform subgroup analysis
perform_subgroup_analysis <- function(data, group_var, group_name) {
  cat(sprintf("\n%s SUBGROUP ANALYSIS:\n", toupper(group_name)))
  cat(rep("-", 40) + "\n")
  
  groups <- unique(data[[group_var]])
  
  for (group in groups) {
    subset_data <- data[data[[group_var]] == group, ]
    n_group <- nrow(subset_data)
    
    cat(sprintf("\n%s (n = %d):\n", group, n_group))
    
    if (n_group >= 10) {  # Minimum sample size for categorical tests
      
      # Question 1 analysis
      cat("  Question 1 (Yes/No):\n")
      q1_sub_table <- table(subset_data$q1_pre, subset_data$q1_post)
      
      # Check if McNemar's test is appropriate (need discordant pairs)
      if (sum(q1_sub_table[1,2] + q1_sub_table[2,1]) >= 5) {
        q1_sub_test <- mcnemar.test(q1_sub_table, correct = TRUE)
        cat(sprintf("    McNemar's χ² = %.3f, p = %.3f\n", 
                    q1_sub_test$statistic, q1_sub_test$p.value))
      } else {
        cat("    Insufficient discordant pairs for McNemar's test\n")
      }
      
      # Show proportions
      cat("    Pre: ", sprintf("%.1f%% Yes", 100 * mean(subset_data$q1_pre == "Yes")), "\n")
      cat("    Post:", sprintf("%.1f%% Yes", 100 * mean(subset_data$q1_post == "Yes")), "\n")
      
      # Question 2 analysis
      cat("  Question 2 (Yes/No/Maybe):\n")
      q2_sub_table <- table(subset_data$q2_pre, subset_data$q2_post)
      
      # Symmetry test
      if (min(dim(q2_sub_table)) >= 2 && sum(q2_sub_table) >= 20) {
        q2_sub_test <- tryCatch({
          symmetry_test(q2_post ~ q2_pre, data = subset_data)
        }, error = function(e) NULL)
        
        if (!is.null(q2_sub_test)) {
          cat(sprintf("    Symmetry test p = %.3f\n", pvalue(q2_sub_test)))
        } else {
          cat("    Unable to perform symmetry test\n")
        }
      } else {
        cat("    Insufficient data for symmetry test\n")
      }
      
      # Show proportions for Q2
      q2_props_pre <- prop.table(table(subset_data$q2_pre))
      q2_props_post <- prop.table(table(subset_data$q2_post))
      cat("    Pre proportions: ", paste(sprintf("%.1f%%", 100*q2_props_pre), names(q2_props_pre), collapse=", "), "\n")
      cat("    Post proportions:", paste(sprintf("%.1f%%", 100*q2_props_post), names(q2_props_post), collapse=", "), "\n")
      
    } else {
      cat("  Sample size too small for reliable analysis\n")
    }
  }
}

# Analyze by age group
perform_subgroup_analysis(data, "age_group", "age group")

# Analyze by race
perform_subgroup_analysis(data, "race", "race")

cat("\n" + rep("=", 60) + "\n")
cat("DETAILED CHANGE ANALYSIS\n")
cat(rep("=", 60) + "\n")

# Detailed change patterns by demographics
cat("Question 1 Change Patterns by Age Group:\n")
q1_age_change <- table(data$age_group, data$q1_change)
print(q1_age_change)
print(prop.table(q1_age_change, margin = 1))

cat("\nQuestion 1 Change Patterns by Race:\n")
q1_race_change <- table(data$race, data$q1_change)
print(q1_race_change)
print(prop.table(q1_race_change, margin = 1))

# Chi-square tests for demographic differences in change patterns
cat("\nTesting for demographic differences in change patterns:\n")
cat("Q1 changes by age group:\n")
chisq_age_q1 <- chisq.test(q1_age_change)
print(chisq_age_q1)

cat("\nQ1 changes by race:\n")
chisq_race_q1 <- chisq.test(q1_race_change)
print(chisq_race_q1)

cat("\n" + rep("=", 60) + "\n")
cat("ADVANCED GGPLOT2 VISUALIZATIONS\n")
cat(rep("=", 60) + "\n")

# Create enhanced change variables for visualization
data <- data %>%
  mutate(
    # Q1 positive change indicator
    q1_positive_change = case_when(
      q1_pre == "No" & q1_post == "Yes" ~ "Changed to Yes",
      q1_pre == "Yes" & q1_post == "Yes" ~ "Stayed Yes",
      q1_pre == "No" & q1_post == "No" ~ "Stayed No",
      q1_pre == "Yes" & q1_post == "No" ~ "Changed to No"
    ),
    
    # Q2 positive change indicator
    q2_positive_change = case_when(
      q2_pre %in% c("No", "Maybe") & q2_post == "Yes" ~ "Changed to Yes",
      q2_pre == "Yes" & q2_post == "Yes" ~ "Stayed Yes",
      q2_pre == "No" & q2_post == "Maybe" ~ "No to Maybe",
      q2_pre == "Maybe" & q2_post == "No" ~ "Maybe to No",
      q2_pre == "Yes" & q2_post %in% c("No", "Maybe") ~ "Yes to Lower",
      TRUE ~ "No Change"
    ),
    
    # Binary indicators for positive movement
    q1_moved_to_yes = (q1_pre == "No" & q1_post == "Yes"),
    q2_moved_to_yes = (q2_pre %in% c("No", "Maybe") & q2_post == "Yes")
  )

# 1. Question 1: Age vs Change to Yes
cat("Creating Q1 visualizations...\n")

p1_age <- data %>%
  group_by(age_group, q1_positive_change) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(age_group) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  ) %>%
  ggplot(aes(x = age_group, y = percentage, fill = q1_positive_change)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_viridis_d(name = "Q1 Change Pattern") +
  labs(title = "Q1 Response Changes by Age Group",
       subtitle = "Percentage distribution of change patterns",
       x = "Age Group", 
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# 2. Question 1: Race vs Change to Yes
p1_race <- data %>%
  group_by(race, q1_positive_change) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(race) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  ) %>%
  ggplot(aes(x = race, y = percentage, fill = q1_positive_change)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_fill_viridis_d(name = "Q1 Change Pattern") +
  labs(title = "Q1 Response Changes by Race/Ethnicity",
       subtitle = "Percentage distribution of change patterns",
       x = "Race/Ethnicity", 
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# 3. Question 2: Age vs Change to Yes
cat("Creating Q2 visualizations...\n")

p2_age <- data %>%
  group_by(age_group, q2_positive_change) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(age_group) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  ) %>%
  ggplot(aes(x = age_group, y = percentage, fill = q2_positive_change)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 2.5) +
  scale_fill_viridis_d(name = "Q2 Change Pattern") +
  labs(title = "Q2 Response Changes by Age Group",
       subtitle = "Percentage distribution of change patterns",
       x = "Age Group", 
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# 4. Question 2: Race vs Change to Yes
p2_race <- data %>%
  group_by(race, q2_positive_change) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(race) %>%
  mutate(
    total = sum(count),
    percentage = count / total * 100
  ) %>%
  ggplot(aes(x = race, y = percentage, fill = q2_positive_change)) +
  geom_col(position = "stack", alpha = 0.8) +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 2.5) +
  scale_fill_viridis_d(name = "Q2 Change Pattern") +
  labs(title = "Q2 Response Changes by Race/Ethnicity",
       subtitle = "Percentage distribution of change patterns",
       x = "Race/Ethnicity", 
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# 5. Focused analysis: Who moved to "Yes"?
cat("Creating focused 'moved to Yes' analysis...\n")

# Calculate rates of positive change by demographics
positive_change_summary <- data %>%
  group_by(age_group, race) %>%
  summarise(
    n = n(),
    q1_yes_rate = mean(q1_moved_to_yes) * 100,
    q2_yes_rate = mean(q2_moved_to_yes) * 100,
    .groups = 'drop'
  )

# Heatmap showing rates of positive change
p3_heatmap <- positive_change_summary %>%
  pivot_longer(cols = c(q1_yes_rate, q2_yes_rate), 
               names_to = "question", values_to = "yes_rate") %>%
  mutate(question = ifelse(question == "q1_yes_rate", "Q1: No→Yes", "Q2: No/Maybe→Yes")) %>%
  ggplot(aes(x = age_group, y = race, fill = yes_rate)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = paste0(round(yes_rate, 1), "%")), 
            color = "white", fontface = "bold") +
  scale_fill_viridis_c(name = "% Moving\nto Yes", 
                       trans = "sqrt",
                       labels = function(x) paste0(x, "%")) +
  facet_wrap(~question) +
  labs(title = "Rate of Positive Change by Demographics",
       subtitle = "Percentage who moved toward 'Yes' response",
       x = "Age Group", 
       y = "Race/Ethnicity") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Comparative bar chart: Direct comparison of positive change rates
p4_comparison <- data %>%
  group_by(age_group) %>%
  summarise(
    Q1_rate = mean(q1_moved_to_yes) * 100,
    Q2_rate = mean(q2_moved_to_yes) * 100,
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(Q1_rate, Q2_rate), 
               names_to = "Question", values_to = "Rate") %>%
  mutate(Question = ifelse(Question == "Q1_rate", "Q1 (No→Yes)", "Q2 (No/Maybe→Yes)")) %>%
  ggplot(aes(x = age_group, y = Rate, fill = Question)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Rate, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("#440154FF", "#31688EFF")) +
  labs(title = "Positive Change Rates by Age Group",
       subtitle = "Comparison between Q1 and Q2",
       x = "Age Group", 
       y = "Percentage Moving to 'Yes'") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

p5_comparison <- data %>%
  group_by(race) %>%
  summarise(
    Q1_rate = mean(q1_moved_to_yes) * 100,
    Q2_rate = mean(q2_moved_to_yes) * 100,
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = c(Q1_rate, Q2_rate), 
               names_to = "Question", values_to = "Rate") %>%
  mutate(Question = ifelse(Question == "Q1_rate", "Q1 (No→Yes)", "Q2 (No/Maybe→Yes)")) %>%
  ggplot(aes(x = race, y = Rate, fill = Question)) +
  geom_col(position = "dodge", alpha = 0.8) +
  geom_text(aes(label = paste0(round(Rate, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_fill_manual(values = c("#440154FF", "#31688EFF")) +
  labs(title = "Positive Change Rates by Race/Ethnicity",
       subtitle = "Comparison between Q1 and Q2",
       x = "Race/Ethnicity", 
       y = "Percentage Moving to 'Yes'") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold"))

# Display all plots
print(p1_age)
print(p1_race)
print(p2_age)
print(p2_race)
print(p3_heatmap)
print(p4_comparison)
print(p5_comparison)

# 7. Statistical summary table for positive changes
cat("\nStatistical Summary: Rates of Positive Change\n")
cat(rep("-", 50) + "\n")

summary_stats <- data %>%
  group_by(age_group, race) %>%
  summarise(
    n = n(),
    q1_to_yes_n = sum(q1_moved_to_yes),
    q1_to_yes_pct = round(mean(q1_moved_to_yes) * 100, 1),
    q2_to_yes_n = sum(q2_moved_to_yes),
    q2_to_yes_pct = round(mean(q2_moved_to_yes) * 100, 1),
    .groups = 'drop'
  )

print(summary_stats)

# Overall rates by demographics
cat("\nOverall Rates by Age Group:\n")
age_overall <- data %>%
  group_by(age_group) %>%
  summarise(
    n = n(),
    q1_to_yes_pct = round(mean(q1_moved_to_yes) * 100, 1),
    q2_to_yes_pct = round(mean(q2_moved_to_yes) * 100, 1),
    .groups = 'drop'
  )
print(age_overall)

cat("\nOverall Rates by Race/Ethnicity:\n")
race_overall <- data %>%
  group_by(race) %>%
  summarise(
    n = n(),
    q1_to_yes_pct = round(mean(q1_moved_to_yes) * 100, 1),
    q2_to_yes_pct = round(mean(q2_moved_to_yes) * 100, 1),
    .groups = 'drop'
  )
print(race_overall)

cat("\n" + rep("=", 60) + "\n")
cat("BASIC VISUALIZATIONS (for reference)\n")
cat(rep("=", 60) + "\n")

# Create basic visualizations
par(mfrow = c(2, 2))

# Q1 pre/post comparison
q1_comparison <- data.frame(
  Time = rep(c("Pre", "Post"), each = 2),
  Response = rep(c("No", "Yes"), 2),
  Count = c(sum(data$q1_pre == "No"), sum(data$q1_pre == "Yes"),
            sum(data$q1_post == "No"), sum(data$q1_post == "Yes"))
)

barplot(matrix(q1_comparison$Count, nrow = 2), 
        beside = TRUE, col = c("lightcoral", "lightblue"),
        names.arg = c("Pre", "Post"), 
        main = "Q1: Yes/No Responses",
        legend.text = c("No", "Yes"), 
        ylim = c(0, max(q1_comparison$Count) * 1.2))

# Q2 pre/post comparison
q2_pre_counts <- table(data$q2_pre)
q2_post_counts <- table(data$q2_post)
q2_matrix <- rbind(q2_pre_counts, q2_post_counts)

barplot(q2_matrix, beside = TRUE, col = c("lightcoral", "lightblue"),
        main = "Q2: Yes/No/Maybe Responses",
        legend.text = c("Pre", "Post"), 
        ylim = c(0, max(q2_matrix) * 1.2))

# Change patterns for Q1
change_counts_q1 <- table(data$q1_change)
barplot(change_counts_q1, main = "Q1 Change Patterns",
        col = c("gray", "lightgreen", "salmon"),
        las = 2)

# Mosaic plot for Q1 transitions
mosaicplot(q1_table, main = "Q1: Pre vs Post Transitions",
           xlab = "Pre-Advertisement", ylab = "Post-Advertisement")

par(mfrow = c(1, 1))

# ALTERNATIVE APPROACH: Treating Q2 as ordinal scores
cat("\n" + rep("=", 60) + "\n")
cat("QUESTION 2: ORDINAL SCORING APPROACH FOR PAIRED T-TESTS\n")
cat(rep("=", 60) + "\n")

cat("Converting Q2 responses to ordinal scores for paired t-test analysis:\n")
cat("No = 1, Maybe = 2, Yes = 3\n\n")

# Convert Q2 responses to numeric scores
data$q2_pre_score <- as.numeric(factor(data$q2_pre, levels = c("No", "Maybe", "Yes")))
data$q2_post_score <- as.numeric(factor(data$q2_post, levels = c("No", "Maybe", "Yes")))
data$q2_diff_score <- data$q2_post_score - data$q2_pre_score

# Function to perform paired t-test analysis by race
analyze_q2_by_race <- function(race_group) {
  subset_data <- data[data$race == race_group, ]
  n_group <- nrow(subset_data)
  
  cat(sprintf("%s PARTICIPANTS (n = %d):\n", toupper(race_group), n_group))
  cat(rep("-", 40) + "\n")
  
  if (n_group >= 10) {
    # Descriptive statistics
    cat(sprintf("Pre-ad mean score: %.2f (SD = %.2f)\n", 
                mean(subset_data$q2_pre_score), sd(subset_data$q2_pre_score)))
    cat(sprintf("Post-ad mean score: %.2f (SD = %.2f)\n", 
                mean(subset_data$q2_post_score), sd(subset_data$q2_post_score)))
    cat(sprintf("Mean difference: %.2f (SD = %.2f)\n", 
                mean(subset_data$q2_diff_score), sd(subset_data$q2_diff_score)))
    
    # Paired t-test
    t_test_result <- t.test(subset_data$q2_post_score, subset_data$q2_pre_score, 
                            paired = TRUE)
    print(t_test_result)
    
    # Effect size
    effect_size <- cohen.d(subset_data$q2_post_score, subset_data$q2_pre_score, 
                           paired = TRUE)
    cat(sprintf("Cohen's d: %.3f (%s effect)\n", 
                effect_size$estimate,
                ifelse(abs(effect_size$estimate) < 0.2, "negligible",
                       ifelse(abs(effect_size$estimate) < 0.5, "small",
                              ifelse(abs(effect_size$estimate) < 0.8, "medium", "large")))))
    
    # Normality check for difference scores
    shapiro_result <- shapiro.test(subset_data$q2_diff_score)
    cat(sprintf("Normality test (Shapiro-Wilk): W = %.3f, p = %.3f\n", 
                shapiro_result$statistic, shapiro_result$p.value))
    
    if (shapiro_result$p.value < 0.05) {
      cat("WARNING: Difference scores not normally distributed - consider non-parametric test\n")
      
      # Wilcoxon signed-rank test as alternative
      wilcox_result <- wilcox.test(subset_data$q2_post_score, subset_data$q2_pre_score, 
                                   paired = TRUE)
      cat("Non-parametric alternative (Wilcoxon signed-rank test):\n")
      print(wilcox_result)
    }
    
    # Response distribution
    cat("\nResponse distributions:\n")
    pre_dist <- table(subset_data$q2_pre)
    post_dist <- table(subset_data$q2_post)
    cat("Pre-ad: ", paste(names(pre_dist), pre_dist, sep="=", collapse=", "), "\n")
    cat("Post-ad:", paste(names(post_dist), post_dist, sep="=", collapse=", "), "\n")
    
  } else {
    cat("Sample size too small for reliable analysis\n")
  }
  cat("\n")
}

# Analyze each racial group
race_groups <- unique(data$race)
for (race in race_groups) {
  analyze_q2_by_race(race)
}

# Summary comparison table
cat("SUMMARY COMPARISON TABLE - Q2 Ordinal Analysis:\n")
cat(rep("-", 60) + "\n")

q2_race_summary <- data %>%
  group_by(race) %>%
  summarise(
    n = n(),
    pre_mean = round(mean(q2_pre_score), 2),
    pre_sd = round(sd(q2_pre_score), 2),
    post_mean = round(mean(q2_post_score), 2),
    post_sd = round(sd(q2_post_score), 2),
    mean_diff = round(mean(q2_diff_score), 2),
    diff_sd = round(sd(q2_diff_score), 2),
    .groups = 'drop'
  )

print(q2_race_summary)

# Perform t-tests for each group and collect p-values
cat("\nSTATISTICAL SIGNIFICANCE SUMMARY:\n")
cat(rep("-", 40) + "\n")

race_results <- data.frame(
  Race = character(),
  n = numeric(),
  t_statistic = numeric(),
  p_value = numeric(),
  cohens_d = numeric(),
  significant = character(),
  stringsAsFactors = FALSE
)

for (race in race_groups) {
  subset_data <- data[data$race == race, ]
  n_group <- nrow(subset_data)
  
  if (n_group >= 10) {
    t_test <- t.test(subset_data$q2_post_score, subset_data$q2_pre_score, paired = TRUE)
    effect <- DescTools::CohenD(subset_data$q2_post_score, subset_data$q2_pre_score)
    
    race_results <- rbind(race_results, data.frame(
      Race = race,
      n = n_group,
      t_statistic = round(t_test$statistic, 3),
      p_value = round(t_test$p.value, 4),
      cohens_d = round(effect$estimate, 3),
      significant = ifelse(t_test$p.value < 0.05, "Yes*", "No")
    ))
  }
}

print(race_results)
cat("* Statistically significant at α = 0.05\n")

# Visualization of Q2 scores by race
cat("\nCreating visualization of Q2 ordinal scores by race...\n")

# Box plot comparison
p_q2_scores <- data %>%
  select(race, q2_pre_score, q2_post_score) %>%
  pivot_longer(cols = c(q2_pre_score, q2_post_score), 
               names_to = "time", values_to = "score") %>%
  mutate(time = ifelse(time == "q2_pre_score", "Pre-Ad", "Post-Ad")) %>%
  ggplot(aes(x = race, y = score, fill = time)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_point(position = position_jitterdodge(dodge.width = 0.8, jitter.width = 0.2), 
             alpha = 0.5, size = 1) +
  scale_y_continuous(breaks = 1:3, labels = c("No", "Maybe", "Yes")) +
  scale_fill_manual(values = c("lightcoral", "lightblue"), name = "Time Point") +
  labs(title = "Q2 Response Scores by Race/Ethnicity",
       subtitle = "Ordinal scoring: No=1, Maybe=2, Yes=3",
       x = "Race/Ethnicity", 
       y = "Response Score") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_q2_scores)

# Mean difference plot
p_q2_diff <- data %>%
  group_by(race) %>%
  summarise(
    mean_diff = mean(q2_diff_score),
    se_diff = sd(q2_diff_score) / sqrt(n()),
    .groups = 'drop'
  ) %>%
  ggplot(aes(x = race, y = mean_diff)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_diff - 1.96*se_diff, ymax = mean_diff + 1.96*se_diff),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Mean Q2 Score Change by Race/Ethnicity",
       subtitle = "Error bars show 95% confidence intervals",
       x = "Race/Ethnicity", 
       y = "Mean Change in Score (Post - Pre)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(p_q2_diff)

cat("\nAnalysis complete. Key findings:\n")
cat("1. McNemar's test for Q1 (binary) paired comparisons\n")
cat("2. Stuart-Maxwell/Symmetry tests for Q2 (ordinal) paired comparisons\n")
cat("3. Ordinal scoring approach for Q2 enables paired t-tests by race\n")
cat("4. Demographic subgroup analyses with appropriate sample size checks\n")
cat("5. Change pattern analysis showing specific transitions\n")
cat("6. Advanced ggplot2 visualizations for demographic comparisons\n")
cat("7. Statistical significance testing within each racial group\n")
cat("\nNote: For Q2, both categorical and ordinal approaches are provided.\n")
cat("The ordinal approach treats responses as equally-spaced intervals.\n")