library(ggplot2)
library(tidyr)
#Import data
lit.all2 <- read.csv(here::here("data","MELNHE Litterfall EDI Data - December.csv"))



table(lit.all2$Stand, lit.all2$Lityear)

#Isolate fall data
lit.fall.sort2<-subset(lit.all2, Season == "Fall" & Sorted == "Y")


names(lit.fall.sort2)
#Convert to long form
lgf2<- gather(lit.fall.sort2[,c(1:35)], "SP","mass",c(14:35))
lgf2$staplo<-paste(lgf2$Stand, lgf2$Plot)
lgf2$Year<-as.factor(lgf2$Year)
lgf2$mass<-as.numeric(lgf2$mass)



# deal with species

lgf2[lgf2$SP== "PRSE2", "SP"] <- "PRPE2"
lgf2[lgf2$SP== "PIRU", "SP"] <- "TSCA"
lgf2[lgf2$SP== "BEPO", "SP"] <- "BEPA"
lgf2[lgf2$SP== "POGR4", "SP"] <- "PO_sp"
lgf2[lgf2$SP== "POTR5", "SP"] <- "PO_sp"
lgf2[lgf2$SP== "BE_sp", "SP"] <- "BEPA"


lgf2[lgf2$SP== "TSCA", "SP"] <- "Unknown"

uncommon_sp <- c("ACSP2","ACPE", "FRAM2","QURU","SOAM3","TIAM","PO_sp")


common_sp <- c("BEAL2","BEPA", "FAGR2","PRPE2","ACRU","ACSA3","VILA11")


lgf2[lgf2$SP %in% common_sp , "Common"] <- "Major species"

lgf2 <- lgf2[ , c("Lityear","Site","Stand","Plot","Basket","Basket_Area",
                  "SP","mass","staplo")]

## Do a plot-level ordination for a single plot

df_sub <-lgf2[lgf2$Stand == "C3",]

df_sub


################################

library(vegan)
library(tidyverse)

# Assuming your data is in 'lgf2' dataframe
# Filter to your subset


# Step 1: Pivot to wide format
df_wide <- df_sub %>%
  select(Basket, Lityear, SP, mass, staplo) %>%
  # Replace NA with 0
  mutate(mass = replace_na(mass, 0)) %>%
  # Pivot wider
  pivot_wider(
    names_from = SP,
    values_from = mass,
    values_fill = 0,
    values_fn = sum  # In case of duplicates, sum them
  )



df_wide$basket_id <- paste(df_wide$Lityear, df_wide$staplo, df_wide$Basket)

# Step 2: Prepare community matrix (species columns only)
# Keep metadata separate
metadata <- df_wide %>%
  select(Basket, Lityear, staplo, basket_id)



# Create community matrix
comm_matrix <- df_wide %>%
  select(-staplo,-Basket, -Lityear, -basket_id) %>%
  as.data.frame()

# Add rownames for clarity
rownames(comm_matrix) <- df_wide$basket_id

# Step 3: Data quality checks and cleaning
# Check for negative values
print("Checking for negative values:")
print(summary(as.vector(as.matrix(comm_matrix))))

# Check for remaining NAs
print(paste("Number of NAs:", sum(is.na(comm_matrix))))

# Check for empty rows (all zeros)
row_sums <- rowSums(comm_matrix, na.rm = TRUE)
print(paste("Number of empty rows:", sum(row_sums == 0)))
print("Empty rows:")
print(names(row_sums[row_sums == 0.00]))

# Fix negative values - set to 0 (or investigate why they exist!)
comm_matrix[comm_matrix < 0] <- 0

# Replace any remaining NAs with 0
comm_matrix[is.na(comm_matrix)] <- 0

# Remove empty rows (baskets with no species)
comm_matrix_clean <- comm_matrix[rowSums(comm_matrix) > 0, ]

dim(comm_matrix_clean)
dim(comm_matrix)

# Update metadata to match cleaned data
metadata_clean <- metadata[metadata$basket_id %in% rownames(comm_matrix_clean), ]

print(paste("Original samples:", nrow(comm_matrix)))
print(paste("Samples after removing empty rows:", nrow(comm_matrix_clean)))

# For now, use untransformed data
comm_matrix_transformed <- comm_matrix_clean

# Step 4: Run NMDS
set.seed(123)  # For reproducibility
nmds_result <- metaMDS(
  comm_matrix_transformed,
  distance = "bray",  # Bray-Curtis dissimilarity
  k = 2,              # 2 dimensions
  trymax = 100,       # Number of random starts
  autotransform = FALSE  # We're handling transformation ourselves
)




# Check stress value (< 0.2 is generally acceptable)
print(paste("Stress value:", round(nmds_result$stress, 3)))

# Step 5: Extract NMDS scores and combine with metadata
nmds_scores <- as.data.frame(scores(nmds_result, display = "sites"))
nmds_scores$basket_id <- rownames(nmds_scores)

# Merge with metadata
plot_data <- nmds_scores %>%
  left_join(metadata_clean, by = "basket_id")

# Step 6: Prepare species scores for biplot
species_scores <- as.data.frame(scores(nmds_result, display = "species"))
species_scores$species <- rownames(species_scores)

# Add combined basket label (Basket + Year)
plot_data$basket_label <- paste(plot_data$Basket, plot_data$Lityear, sep = " - ")

# Create combined biplot with both basket labels and species vectors
ggplot(plot_data, aes(x = NMDS1, y = NMDS2)) +
  xlim( min(plot_data$NMDS1)*1.2,max(plot_data$NMDS1)*1.2 )+
  ylim( min(plot_data$NMDS2)*1.2,max(plot_data$NMDS2)*1.2 )+
  # Add basket points
  geom_point(aes( color = staplo),size = 4, alpha = 0.8 )+
  geom_segment(data = species_scores,
               aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "red", alpha = 0.5, inherit.aes = FALSE) +
  geom_text(data = species_scores,
            aes(x = NMDS1, y = NMDS2, label = species),
            size = 3, color = "grey30", inherit.aes = FALSE) +
  facet_wrap(~staplo)+
  # Add basket labels
  geom_text(aes(label = basket_label), hjust = -0.2, vjust = -0.5, size = 3) +
  # Add stress value
  annotate("text", x = Inf, y = Inf, 
           label = paste("Stress =", round(nmds_result$stress, 3)),
           hjust = 1.1, vjust = 1.5, size = 4) +
  labs(
    title = "NMDS Ordination with Species Vectors",
    subtitle = paste("Site:", unique(df_sub$Stand)),
    x = "NMDS1",
    y = "NMDS2"
  ) +
  theme_bw() +
  theme(legend.position = "right") +
  coord_fixed()




  
  # Additional diagnostics
# Shepard plot to assess fit
#stressplot(nmds_result)


