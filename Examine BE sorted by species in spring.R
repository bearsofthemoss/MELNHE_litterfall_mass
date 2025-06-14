
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



#####
lit <- read.csv(here::here("data","MELNHE Litterfall mass.csv"))
head(lit)
table(lit$Year)

## exclude 2005 for now
lit <- lit[lit$Year>=2009,]
table(lit$Season)

# Summer, fall, spring
str(lit)

lit$Total_Mass <- as.numeric(lit$Total_Mass)
lit$Leaf_Mass <- as.numeric(lit$Leaf_Mass)
lit$Total_Mass_g_m2 <- as.numeric(lit$Total_Mass_g_m2)

# Select the intended columns to reduce confusion.

head(lit)

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

full_baskets[full_baskets$Season=="Fall", "time"] <- 3
full_baskets[full_baskets$Season=="Spring", "time"] <- 6
full_baskets[full_baskets$Season=="Summer", "time"] <- 9

full_baskets$time <- as.numeric(paste0(full_baskets$Lityear,".", full_baskets$time))
full_baskets$seas_bask <- paste(full_baskets$Season, full_baskets$Basket)

sel_stands <- c("C1","C2","C3","C4","C5","C6")

#ggplot( full_baskets[full_baskets$Stand==c(sel_stands),], 
ggplot( full_baskets, 
        aes(x=time, y=Total_Mass+1, col=Season, group=seas_bask))+
  geom_point()+
  geom_line()+
  scale_y_log10()+
  facet_grid(Stand~Plot)


ggplot(full_baskets, aes(x=Total_Mass, y=Leaf_Mass))+geom_point()

table(full_baskets$Lityear, full_baskets$Season)
#Fall litter, BE in fall, prop BE in fall, 






# most years we didn't sort spring.

spring <- sorted_species_weights[sorted_species_weights$Season=="Spring", ]
str(spring)

View(spring)
table(spring)
head(spring)

summary(spring)
# Replace columns 4 through 12 with numeric versions
spring[,7:30] <- lapply(spring[,7:30], as.numeric)

summary(spring)
as.data.frame(table(lit$Sorted, lit$Lityear, lit$Stand))

head(spring)

spring$spring_total <- rowSums( spring[ , c(7:30)], na.rm=T)
table(is.na(spring$spring_total))
hist(spring$spring_total)

spb <- spring[spring$spring_total>0,]
dim(spb)
table(spb$Stand, spb$Lityear)

head(spb)
sorted_spring <- spring[spring$Sorted=="Y",]

table( sorted_spring$Stand, sorted_spring$Lityear)


# make basket ID to match in fall total and summer total;
spb$buk <- paste(spb$Lityear, spb$Stand, spb$Plot, spb$Basket)

# do subsetting
fb_fall <- full_baskets[full_baskets$Season=="Fall", ]
fb_summer <- full_baskets[full_baskets$Season=="Summer",]
# sorted BE is only fall I think. 

# create unique keys
fb_fall$buk <- paste(fb_fall$Lityear,  fb_fall$Stand, fb_fall$Plot, fb_fall$Basket)
fb_summer$buk <- paste(fb_summer$Lityear, fb_summer$Stand, fb_summer$Plot, fb_summer$Basket)



spb$fall_mass <- fb_fall$Total_Mass[match(spb$buk, fb_fall$buk)]
spb$summer_mass <- fb_summer$Total_Mass[match(spb$buk, fb_summer$buk)]

spb$BE_mass_fall <- as.numeric(fb_fall$FAGR[match(spb$buk, fb_fall$buk)])


############

library(broom)

# prop BE from fall 
spb$prop_FAGR <- spb$FAGR/ spb$fall_mass

# Fit the model
 model <- lm(FAGR ~ prop_FAGR+fall_mass + summer_mass + BE_mass_fall, data=spb)
 
spring_FAGR_model <- lm(FAGR ~ prop_FAGR+ BE_mass_fall, data=spb)

anova(spring_FAGR_model)
summary(spring_FAGR_model)$r.squared

# Create a dataframe of model results
model_data <- augment(spring_FAGR_model)

summary(spring_FAGR_model)$r.squared

# Residuals vs Fitted Values Plot
ggplot(model_data, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residuals vs Fitted",
       x = "Fitted Values",
       y = "Residuals") +
  theme_bw()

ggplot(model_data, aes(x = .fitted, y = FAGR)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Actual vs Predicted Values",
       x = "Predicted FAGR",
       y = "Actual FAGR") +
  theme_bw()



library(broom)

# Extract coefficients
coef_data <- tidy(spring_FAGR_model)

ggplot(coef_data[-1,], aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - std.error, 
                    ymax = estimate + std.error), 
                width = 0.2) +
  labs(title = "Model Coefficients",
       x = "Predictors",
       y = "Coefficient Estimate") +
  theme_bw() +
  coord_flip()


library(performance)
check_model(model)


##  Make the predictions for the proportion of BE in spring.

table(full_baskets$Season)
##########################################

full_baskets$buk <- paste(full_baskets$Lityear, full_baskets$Stand, full_baskets$Plot, full_baskets$Basket)


full_baskets$fall_mass <- fb_fall$Total_Mass[match(full_baskets$buk, fb_fall$buk)]

full_baskets$BE_mass_fall <- as.numeric(fb_fall$FAGR[match(full_baskets$buk, fb_fall$buk)])

# tricky-  do we set the BE proportion to 0 uf there isn't a sorted value for BE?
full_baskets[is.na(full_baskets$BE_mass_fall), "BE_mass_fall"] <- 0

full_baskets$prop_FAGR <- full_baskets$BE_mass_fall/ full_baskets$fall_mass

full_baskets$spring_BE_weight <- predict( spring_FAGR_model, newdata=  full_baskets , type= "response")


hist(full_baskets$spring_BE_weight)
