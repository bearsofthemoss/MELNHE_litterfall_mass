


library(here)
library(ggplot2)

# read in data
br <- read.csv(here::here("data","basket risk assessment.csv"))
head(br)

# examine the structure of the data
str(br)

# count the number of times each risk was used
table(br$risk.level)
table(br$new.risk.level)

# visualize risk score by plot in each stand
ggplot(br, aes(x=plot, y=new.risk.level, fill=as.factor(new.risk.level)))+
  geom_col(aes(group=plot), col="black")+
  facet_wrap(~stand)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  labs(x="New risk score", y="Plot",
       fill="Risk score ")




br$new.risk.level <- as.numeric(br$new.risk.level)

# create a table of risk level by stand, then turn into a data.frame
risk <- as.data.frame(table(br$new.risk.level, br$stand, br$basket))

str(risk)

library(ggplot2)


  