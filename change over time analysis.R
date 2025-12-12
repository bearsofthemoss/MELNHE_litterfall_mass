
## Start analysis


### Treatment effect on litterfall mass

library(dplyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(ggplot2)

# Read data

  sorted_imputed <- read.csv(here::here("data", "MELNHE Litterfall mass_Nov2025.csv"))

  sorted_imputed$Age[sorted_imputed$Stand=="C1"]<-"Young forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C2"]<-"Young forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C3"]<-"Young forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C4"]<-"Mid-aged forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C5"]<-"Mid-aged forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C6"]<-"Mid-aged forest" 
  sorted_imputed$Age[sorted_imputed$Stand=="C7"]<-"Mature forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C8"]<-"Mature forest"
  sorted_imputed$Age[sorted_imputed$Stand=="C9"]<-"Mature forest"
  sorted_imputed$Age[sorted_imputed$Stand=="JBM"]<-"Mid-aged forest"
  sorted_imputed$Age[sorted_imputed$Stand=="HBM"]<-"Mid-aged forest"
  sorted_imputed$Age[sorted_imputed$Stand=="JBO"]<-"Mature forest"
  sorted_imputed$Age[sorted_imputed$Stand=="HBO"]<-"Mature forest"
  
  sorted_imputed$Total_Mass_g_m2 <- as.numeric(sorted_imputed$Leaf_Mass) / sorted_imputed$Basket_Area
  
  library(dplyr)
  library(ggplot2)
  
  
  sorted_imputed <- sorted_imputed %>%
    group_by(Stand, Age, Year, Season, Treatment) %>%
    summarise(
      Annual_Mass = sum(Total_Mass_g_m2, na.rm = TRUE),
      n_collections = n(),
      .groups = 'drop'
    )
  
  
  sorted_imputed <- sorted_imputed[sorted_imputed$Annual_Mass >0, ]
  
  
  sorted_imputed <- sorted_imputed %>%
    group_by(Stand, Age, Year, Treatment) %>%
    summarise(
      Annual_Mass = mean(Annual_Mass, na.rm = TRUE),
      n_collections = n(),
      .groups = 'drop'
    )
  
  
  # Create the plot
  ggplot(sorted_imputed, aes(x = Year, y = Annual_Mass, color = Treatment, group = Treatment)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    facet_wrap(~Stand) +
    scale_color_manual(values = c("Control" = "black", 
                                  "P" = "red", 
                                  "N" = "blue", 
                                  "NP" = "purple")) +
    labs(title = "Litterfall Mass by Stand and Treatment Over Time",
         x = "Year",
         y = "Average Litterfall Mass (g/m²)") +
    theme_bw() +
    theme(legend.position = "bottom")
  