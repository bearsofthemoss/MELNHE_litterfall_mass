


# examine if a basket is outside its range


library(ggplot2)
library(here)


#####
lit <- read.csv(here::here("data","MELNHE Litterfall mass_Nov2025.csv"))

ggplot(lit, aes(x= Leaf_Mass, y= Total_Mass, col=Sorted))+geom_point()+
  facet_wrap(~Season, scales="free")

a <- as.data.frame(table(is.na(lit$Total_Mass), lit$Season, lit$Year))
a <- a[a$Freq>0, ]
head(a)

ggplot(a, aes(x=Var3, y=Freq, fill=Var2))+geom_col(position="stack")+
  facet_grid(~Var1)

lit$Total_Mass <- as.numeric(lit$Total_Mass)
lit$Leaf_Mass <- as.numeric(lit$Leaf_Mass)
lit$Total_Mass_g_m2 <- as.numeric(lit$Total_Mass_g_m2)

# Select the intended columns to reduce confusion.
table(lit$Year, lit$Season)

head(lit)
head(lit)
summary(lit)
la <- aggregate( list( mass = lit$Total_Mass),
           by=list(Stand = lit$Stand,
                   lityear = lit$Year,
                   season = lit$Season),
           FUN= "mean", na.rm=T)

dim(la)
a <- lm( mass ~ as.factor(lityear) + Stand,  data = la[la$season=="Fall",])
a
options(scipen=999)
anova(a)

head(la)
ggplot( la[la$season=="Fall",] , aes(x=lityear, y= mass))+geom_point()+
  facet_wrap(~Stand)

t8 <-lit[lit$Year==2008,]

t8[t8$Stand=="HBM",]

table
table(t8$Stand, t8$Plot)

table(lit$Basket_Type, lit$Basket_Area)
table(is.na(lit$Basket_Type))
table(is.na(lit$Basket_Area))

a <- as.data.frame(table(lit$Year, lit$Basket_Type, lit$Stand))
a <- a[a$Freq>0,]
ggplot(a, aes(x=Var1, y=Freq, fill=Var2))+
  facet_wrap(~Var3, nrow=5)+
  geom_col()+
#  geom_line(aes(group = Var2))+
  labs(x="Year",y="Number of baskets",
       fill = "Basket type")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# will look at species later, for now start with whole basket mass values
full_baskets <- lit[,c("Lityear","Year","Season","Stand","Plot","Basket",
                       "Total_Mass","Leaf_Mass","Basket_Area","Total_Mass_g_m2", "FAGR")]

table(full_baskets$Leaf_Mass == full_baskets$Total_Mass)

full_baskets$seas_bask <- paste(full_baskets$Season, full_baskets$Basket)

# Alex has a question here- how are 'Leaf_Mass' and 'Total_Mass' different?
ggplot(full_baskets, aes(x=Total_Mass, y=Leaf_Mass))+geom_point()

table(full_baskets$Lityear, full_baskets$Season)

# select years just for exploration
full_baskets <- full_baskets[full_baskets$Lityear>=2010,]
full_baskets <- full_baskets[full_baskets$Lityear< 2023,]

full_baskets$better_facet <- paste( full_baskets$Season, full_baskets$Stand)

ggplot( full_baskets, 
        #ggplot( full_baskets, 
        aes(x=Lityear, y=Total_Mass, col=Season,  group=bask_uk))+
  geom_boxplot(aes(group=Lityear))+
#  scale_y_log10()+
  facet_wrap(~better_facet , nrow=3)+
  theme(axis.text.x = element_text(angle=90, vjust=.5, hjust=1))+
  theme(legend.position = "bottom")


#########################################################################

############

# Create composite identifiers
full_baskets$staplo <- paste(full_baskets$Stand, full_baskets$Plot)
full_baskets$bask_uk <- paste(full_baskets$Season, full_baskets$staplo, full_baskets$Basket)
full_baskets$plot_uk <- paste(full_baskets$Season, full_baskets$staplo)

# Get unique plot-season combinations
full_baskets$year_bask_uk <- paste(full_baskets$Lityear, full_baskets$bask_uk)
full_baskets$year_plot_uk <- paste(full_baskets$Lityear, full_baskets$plot_uk)

plot_output <- list()
basket_output <- list()

# Loop through each unique year-season-plot, examine the baskets within
for (i in 1:length(unique(full_baskets$year_plot_uk))) {

  current_plot <- unique(full_baskets$year_plot_uk)[i]
  plot_bask_data <- full_baskets[full_baskets$year_plot_uk == current_plot, ]
  
  # Extract basket values from a given plot, by season and by year.
  basket_values <- plot_bask_data$Total_Mass
  
  # Remove NA values for statistics calculation
  valid_values <- basket_values[!is.na(basket_values)]
  zero_count <- dim(plot_bask_data[plot_bask_data$Total_Mass== 0,])[1]
  valid_count <- length(valid_values)

  
  # Calculate mean and standard deviation
  plot_mean <- mean(valid_values)
  plot_sd <- sd(valid_values)
    
  z_score <- (basket_values - plot_mean) / plot_sd
  
  bask_name <- plot_bask_data$Basket
  
  ploto = data.frame(
    year_season_plot = current_plot,
    "number_valid" = valid_count,
    "zero_mass_basket" = zero_count,
    "plot_mean" = plot_mean,
    "plot_sd" = plot_sd,
    "Lityear" = unique(plot_bask_data$Lityear),
    "Season" = unique(plot_bask_data$Season),
    "Stand" = unique(plot_bask_data$Stand) ,
    "staplo" = unique(plot_bask_data$staplo))
  
  basketo <- data.frame(
    "year_bask_uk" = plot_bask_data$year_bask_uk,
    "Basket" =  plot_bask_data$Basket,
    "z1" = z_score,
    "Lityear" = plot_bask_data$Lityear,
    "Season" = plot_bask_data$Season,
    "Stand" = plot_bask_data$Stand,
    "staplo" = plot_bask_data$staplo)

plot_output <- rbind( ploto , plot_output)  
basket_output <- rbind( basketo , basket_output)

# Print progress every 50 iterations
if (i %% 50 == 0) {
  cat("Processed", i, "of", length(unique(full_baskets$year_plot_uk)), "year-season-plot combinations\n")
  }
}  

head(plot_output)


## Now with both the plot output and the basket output, look at results

# Dashed lines are around what we'd expect for 5 baskets in 4 plots.

# Some years are higher than others?  Could be the Ca plot in C6 and C8?
plot_output$Season <- factor(plot_output$Season , levels = c("Summer","Spring","Fall"))
ggplot(plot_output, aes(x=Lityear, y=number_valid,
                        fill=Season))+
  geom_col(position="stack")+
  facet_wrap(~Stand, nrow=3)+
  ggtitle("Number of non-NA basket masses")+
  geom_hline(yintercept = 20, linetype = "dashed")+
  geom_hline(yintercept = 40, linetype = "dashed")+
  geom_hline(yintercept = 60, linetype = "dashed")+
  scale_y_continuous(breaks=c(20, 40, 60, 80))
  


ggplot(plot_output, aes(x=Lityear, y=zero_mass_basket ,
                        fill=Season))+
  geom_col(position=position_dodge())+
  facet_wrap(~Stand)+
  ggtitle("Number of Zero basket masses")


# I'm a bit suspicious of the Fall basket masses where all 5 baskets are zero?

table(plot_output$zero_mass_basket, plot_output$Season)
# Maybe we should impute the ~100 baskets where fall litter is 0?

## Move on to baskets
head(basket_output)
valid_baskets <- basket_output[!is.na(basket_output$z1),]

ggplot(valid_baskets)+
  geom_density(aes(x=z1, col=Season, fill=Season), alpha = .3)+
  facet_wrap(~Stand)+
  xlim(-2.5, 2.5)+
  geom_vline(xintercept = c(-1, 1), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(-2, 2), linetype = "dashed", color = "darkred")+
  labs(x="Z score", y="Density")

# examine the z scores that are larger or smaller than 1.5 standard deviations away from the mean value
large_z <- valid_baskets[ valid_baskets$z1 > 1.5 |  valid_baskets$z1 < -1.5, ]  
dim(large_z)


a <- as.data.frame(table(large_z$staplo, large_z$Stand, large_z$Basket, large_z$Season))
a <- a[a$Freq>0 ,]

# across years, do some baskets have more 'z' type baskets?
c <- as.data.frame(table(a$Var1, a$Var3))
use_basks <- c("A1","A3","B2","C1","C3")

c <- c[c$Var2 %in% use_basks, ]



# Alternative heatmap with discrete color scale for easier reading
p2 <- ggplot(c, aes(x = Var1, y = Var2, fill = factor(Freq))) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_brewer(name = "Count", palette = "Reds", direction = 1) +
  geom_text(aes(label = Freq), color = "black", size = 3) +
  labs(
    title = "High Z-Value Counts with Numbers",
    subtitle = "Count of extreme baskets (|Z| > 1.5) by plot and basket type over the years of collection",
    x = "Basket ID", 
    y = "Stand-Plot Combination"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p2)



