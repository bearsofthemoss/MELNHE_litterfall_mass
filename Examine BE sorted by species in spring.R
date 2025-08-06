
library(ggplot2)
library(here)


# notes:
### examine if these 6 are the species
# Pin cherry
# Beech
# Yellow Birch
# White Birch
# Sugar Maple
# Red Maple


lit <- read.csv(here::here("data","MELNHE Litterfall massJune2025.csv"))
names(lit)
table(is.na(lit$Basket_Type))
table(is.na(lit$Basket_Area))
table(lit$Basket_Area, lit$Lityear)

lit$Total_Mass <- as.numeric(lit$Total_Mass)
lit$Leaf_Mass <- as.numeric(lit$Leaf_Mass)
lit$Total_Mass_g_m2 <- as.numeric(lit$Total_Mass_g_m2)


full_baskets <- lit[,c("Lityear","Year","Season","Stand","Plot","Basket",
                       "Total_Mass","Leaf_Mass","Basket_Area","Total_Mass_g_m2", "FAGR")]


# Decide which species to include

species_list <- c("ACPE", "ACRU", "ACSA3", "ACSP2", "BE_sp", "BEAL2", "BEPA", 
                  "BEPO", "FAGR", "FRAM2", "PIRU", "PO_sp", "POGR4", "POTR5",
                  "PRPE2", "PRSE2", "QURU", "SOAM3", "TIAM", "TSCA", "VILA11", 
                  "Unknown","OTHER_Spring", "Other")

sorted_species_weights <-lit[,c("Lityear","Year","Season","Stand","Plot","Basket",
                                species_list)]


### Wanted to understand where the NA values are across seasons.

# the model is basket level spring beech; which is predicted from spring and fall masses.

#### in other words, all spring litter would be converted to BE vs non-BE.

# explanatory variables-  Fall_mass_gm2, BE_gm2, BE_perc, Spring_mass_gm2
# response variable- spring BE mass

# Fall mass at the basket level
library(ggplot2)

head(full_baskets)
table(full_baskets$Leaf_Mass == full_baskets$Total_Mass)


ggplot(full_baskets, aes(x=Total_Mass, y=Leaf_Mass))+geom_point()

full_baskets$buk <- paste(full_baskets$Lityear, full_baskets$Stand, full_baskets$Plot, full_baskets$Basket)
head(full_baskets)

full_baskets$baskID <- paste(full_baskets$Basket, full_baskets$Season)

ggplot(full_baskets[full_baskets$Stand=="C6" &
                      full_baskets$Lityear>2011,], aes(x= Lityear, y=Total_Mass , col=Season))+
  geom_point()+
  geom_line(aes(group=baskID))+
  facet_grid(Basket~Plot)+
  ggtitle("Stand C6")



 # use season total to capture the species composition


spring_total <- aggregate( list(spring_total = full_baskets$Total_Mass),
                           by=list( buk = full_baskets$buk ,
                                    Stand = full_baskets$Stand,
                                    Lityear = full_baskets$Lityear,
                                    season = full_baskets$Season,
                                    Basket = full_baskets$Basket),
                           FUN="mean", na.rm=T)

season_total <- aggregate( list(season_total = spring_total$spring_total),
                           by=list( buk = spring_total$buk ),
                           FUN="sum", na.rm=T)

spring_total <- spring_total[spring_total$season=="Spring",]

spring_total$season_total <- season_total$season_total[match(spring_total$buk, season_total$buk)]

spring_total$prop_spring <- spring_total$spring_total / spring_total$season_total

#Fall litter, BE in fall, prop BE in fall, 

lit <- lit[lit$Year>=2009,]
lit <- lit[lit$Lityear<=2023,]


## Species composition: BE proportion in the fall.
fall <-sorted_species_weights[sorted_species_weights$Season=="Fall", ]

fall$FAGR <- as.numeric(fall$FAGR)

avg_FAGR_fall <- aggregate(list(FAGR_mass_avg =fall$FAGR),
          by= list( Stand = fall$Stand,
                    Plot =  fall$Plot,
                    Basket =  fall$Basket),
          FUN= "mean", na.rm=T)


# most years we didn't sort spring.

spring <- sorted_species_weights[sorted_species_weights$Season=="Spring", ]

# Replace columns 4 through 12 with numeric versions
spring[,7:30] <- lapply(spring[,7:30], as.numeric)

spring$spring_total <- rowSums( spring[ , c(7:30)], na.rm=T)
table(is.na(spring$spring_total))
hist(spring$spring_total)

spb <- spring[spring$spring_total>0,]

spb$lityear_year <- paste(spb$Lityear, spb$Year)
table(spb$lityear_year, spb$Season)

### Bring in the two predictors

sorted_spring <- spring[spring$Sorted=="Y",]

table( sorted_spring$Stand, sorted_spring$Lityear)


# make basket ID to match in fall total and summer total;
spb$buk <- paste(spb$Lityear, spb$Stand, spb$Plot, spb$Basket)
spb$staplobask <- paste(spb$Stand, spb$Plot, spb$Basket)
avg_FAGR_fall$staplobask <- paste(avg_FAGR_fall$Stand, avg_FAGR_fall$Plot, avg_FAGR_fall$Basket)
# Add predictors

spb$FAGR_mass_avg <- avg_FAGR_fall$FAGR_mass_avg[match(spb$staplobask, avg_FAGR_fall$staplobask)]


spb$prop_spring <- spring_total$prop_spring[match(spb$buk , spring_total$buk)]

ggplot(spb, aes(x= Stand, y= FAGR_mass_avg,
                col = FAGR))+
  geom_boxplot()+
  scale_color_gradientn(colours = terrain.colors(10))



############

library(broom)


head(spb)
 spb$prop_FAGR <- spb$FAGR / spb$spring_total
model <- lm( prop_FAGR ~FAGR_mass_avg +prop_spring  , data=spb)

spb$predicted <- predict( model, newdata= spb, type = "response")

spb$predict_mass <- spb$predicted * spb$spring_total



# Residuals vs Fitted Values Plot

ggplot(spb, aes(x = predict_mass, y = FAGR, col=as.factor(Lityear))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values",
       x = "Predicted FAGR",
       y = "Actual FAGR") +
  theme_bw()
 # facet_wrap(~Stand)



library(performance)
check_model(model)


##  Claude helps with code

##########################################

# Load required libraries
library(ggplot2)
library(dplyr)
library(broom)
library(lmer)



# Fit the overall model
#model <- lmer(FAGR ~ FAGR_mass_avg + prop_spring + (1 | Stand), data = spb)

model <- lm( prop_FAGR ~FAGR_mass_avg +prop_spring  , data=spb)

spb$predicted <- predict( model, newdata= spb, type = "response")

spb$predict_mass <- spb$predicted * spb$spring_total


# Calculate R-squared for each stand
stand_r_squared <- spb %>%
  group_by(Stand) %>%
  summarise(
    n_observations = n(),
    r_squared = cor(FAGR, predict_mass, use = "complete.obs")^2,
    .groups = "drop"
  ) %>%
  arrange(desc(r_squared))

# Display R-squared results
print("R-squared by Stand:")
print(stand_r_squared)


# Create the actual vs predicted plot with R-squared annotations
plot_data <- spb %>%
  left_join(stand_r_squared, by = "Stand") %>%
  mutate(
    stand_label = paste0("Stand ", Stand, "\nR² = ", round(r_squared, 2))
  )


# Improved plot
p1 <- ggplot(plot_data, aes(x = predict_mass, y = FAGR,
                            col=as.factor(Lityear))) +
  geom_point(alpha = 0.6, size = 1.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", size = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "blue", alpha = 0.3) +
  labs(
    title = "Actual vs Predicted FAGR Values by Stand",
    subtitle = "Litterfall analysis across 13 forest stands",
    x = "Predicted FAGR",
    y = "Actual FAGR",
    caption = "Red dashed line = perfect prediction; Blue line = actual relationship"
  ) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill = "lightgray"),
    strip.text = element_text(size = 8)
  ) +
  facet_wrap(~stand_label, nrow=3)
 # xlim(0,30)+ylim(0,30)
p1
summary(spb$FAGR)

# Summary statistics
overall_r_squared <- summary(model)$r.squared
cat("\nOverall model R-squared:", round(overall_r_squared, 4))
cat("\nMean R-squared across stands:", round(mean(stand_r_squared$r_squared), 4))
cat("\nRange of stand R-squared:", round(min(stand_r_squared$r_squared), 4), 
    "to", round(max(stand_r_squared$r_squared), 4))

# Residuals analysis by stand
residuals_by_stand <- spb %>%
  mutate(residuals = FAGR - predicted) %>%
  group_by(Stand) %>%
  summarise(
    mean_residual = mean(residuals, na.rm = TRUE),
    sd_residual = sd(residuals, na.rm = TRUE),
    rmse = sqrt(mean(residuals^2, na.rm = TRUE)),
    .groups = "drop"
  )

print("\nResidual analysis by stand:")
print(residuals_by_stand)
