
library(ggplot2)
library(here)


lit <- read.csv(here::here("data","MELNHE Litterfall mass.csv"))
head(lit)

table(lit$Season)

# Summer, fall, spring


names(lit)


# most years we didn't sort spring.

spring <- lit[lit$Season=="Spring", ]

as.data.frame(table(lit$Sorted, lit$Lityear, lit$Stand))


sorted_spring <- spring[spring$Sorted=="Y",]

table( sorted_spring$Stand, sorted_spring$Lityear)




### examine if these 6 are the species
Pin cherry
Beech
Yellow Birch
White Birch
Sugar Maple
Red Maple


# Beech biorch maple old;  pin cherry