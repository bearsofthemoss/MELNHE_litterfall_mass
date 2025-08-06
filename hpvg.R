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
set.seed(123)


hpvg <- read.csv("D:/Users/bears/Downloads/HPV_GARD Study (Responses).csv")

hpvg$participant <- seq(1:dim(hpvg)[1])

table(hpvg$Ethnicity)

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


# Question 1: McNemar's Test for paired binary data
cat("QUESTION 1 ANALYSIS (Yes/No):\n")

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

# Question 2: 



cat("DETAILED CHANGE ANALYSIS\n")

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
