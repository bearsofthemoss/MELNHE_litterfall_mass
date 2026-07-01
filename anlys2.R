
#Install packages:
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)

# FALL GRAPHS

#Import data
lf <-  read.csv("D:/Users/bears/Downloads/MELNHE Litterfall EDI Data - Final Data Sheet for EDI(6).csv")

lf$staplo <-paste(lf$Stand, lf$Plot)

table(lf$Treat)

table(lf$Season, lf$Lityear)
head(lf)

lf[is.na(lf$Total_Mass), "Total_Mass"] <- lf[is.na(lf$Total_Mass), "Leaf_Mass"]
## calculate season level medians

lf$g_m2 <- lf$Total_Mass / lf$Basket_Area


sp <- lf[lf$Season=="Summer", ]
sp <- sp[sp$Treatment!="Ca",]
sm <- aggregate(list( mass = sp$g_m2),
                by=list(Stand = sp$Stand,
                        Lityear = sp$Lityear,
                        Plot = sp$Plot,
                        Treatment = sp$Treatment,
                        Season = sp$Season),
                FUN="mean", na.rm=T)

sm$Ntrmt <- factor(  ifelse(sm$Treatment == "N" | sm$Treatment == "NP", "N", "NoN"))
sm$Ptrmt <- factor(  ifelse(sm$Treatment %in% c("P", "NP"), "P", "NoP"))

sn <- lmer( mass ~ Ntrmt*Ptrmt*Lityear  +(1|Stand), data=sm)
anova(sn)

ggplot( sm, aes(x=Lityear, y=mass))+
  geom_smooth( method="lm", se=F,alpha= .1,
               aes(group=Treatment, col=Treatment))+
  # geom_point( aes( col=Treatment))+
  #  geom_line( aes(group=staplo))+
  scale_color_manual(values=c("black","blue","red","purple", "yellow"))+
  facet_wrap(~ Stand, ncol=4)+
  
  labs(x="Year", y="Litterfall g/m2")+
  theme_bw()

emtrends(sn, ~ Ntrmt * Ptrmt, var = "Lityear")

# Just N vs no N
emtrends(sn, ~ Ntrmt, var = "Lityear")





#################################################

### next with fall

lf <- lf[lf$Season=="Fall",]

# Add baskets together
pm <- aggregate(list( mass = lf$g_m2),
                by=list(Stand = lf$Stand,
                        Lityear = lf$Lityear,
                        Plot = lf$Plot,
                        
                        Season = lf$Season),
                FUN="median", na.rm=T)

# pm <- aggregate(list( mass = pl$mass),
#                 by=list(Stand = pl$Stand,
#                         Lityear = pl$Lityear,
#                         Plot = pl$Plot),
#                 FUN="sum", na.rm=T)


pm$Age[pm$Stand=="C1"]<-"Young forest"
pm$Age[pm$Stand=="C2"]<-"Young forest"
pm$Age[pm$Stand=="C3"]<-"Young forest"
pm$Age[pm$Stand=="C4"]<-"Mid-aged forest"
pm$Age[pm$Stand=="C5"]<-"Mid-aged forest"
pm$Age[pm$Stand=="C6"]<-"Mid-aged forest" 
pm$Age[pm$Stand=="C7"]<-"Mature forest"
pm$Age[pm$Stand=="C8"]<-"Mature forest"
pm$Age[pm$Stand=="C9"]<-"Mature forest"

pm$Age[pm$Stand=="JBM"]<-"Mid-aged forest"
pm$Age[pm$Stand=="JBO"]<-"Mature forest"

pm$Age[pm$Stand=="HBM"]<-"Mid-aged forest"
pm$Age[pm$Stand=="HBO"]<-"Mature forest"



head(pm)
pm <- pm[pm$mass > 20,]

pm$staplo <- paste(pm$Stand, pm$Plot)

pm$Treatment<-sapply(pm[ ,"staplo"],switch,
                     "C1 1"="P",   "C1 2"="N",   "C1 3"="Control", "C1 4"="NP",
                     "C2 1"="NP",  "C2 2"="Control","C2 3"="P",    "C2 4"="N",
                     "C3 1"="NP",  "C3 2"="P",   "C3 3"="N",    "C3 4"="Control",
                     "C4 1"="NP",  "C4 2"="N",   "C4 3"="Control", "C4 4"="P",
                     "C5 1"="Control","C5 2"="NP",  "C5 3"="N",    "C5 4"="P",
                     "C6 1"="NP",  "C6 2"="Control","C6 3"="N",    "C6 4"="P","C6 5"="Ca",
                     "C7 1"="N",   "C7 2"="NP",  "C7 3"="P",    "C7 4"="Control",
                     "C8 1"="P",   "C8 2"="Control","C8 3"="N",    "C8 4"="NP","C8 5"="Ca",
                     "C9 1"="Control","C9 2"="P",   "C9 3"="NP",   "C9 4"="N",
                     "HBM 1"="NP", "HBM 2"="N",  "HBM 3"="Control","HBM 4"="P",
                     "HBO 1"="P",  "HBO 2"="N",  "HBO 3"="NP",  "HBO 4"="Control", "HBO 7"="Control",
                     "JBM 1"="NP", "JBM 2"="N",  "JBM 3"="Control","JBM 4"="P",
                     "JBO 1"="NP", "JBO 2"="P",  "JBO 3"="N",   "JBO 4"="Control")


pm$Treatment <- factor(pm$Treatment, levels=c("Control","N","P","NP","Ca"))

pm <- pm[!pm$Treatment == "Ca",]

ggplot( pm, aes(x=Lityear, y=mass))+
  geom_smooth( method="lm", se=F,alpha= .1,
               aes(group=Treatment, col=Treatment))+
  # geom_point( aes( col=Treatment))+
  #  geom_line( aes(group=staplo))+
  scale_color_manual(values=c("black","blue","red","purple"))+
  facet_wrap(~ Stand, ncol=4)+
  
  labs(x="Year", y="Litterfall g/m2")+
  theme_bw()

tail(pm)
max(pm$Lityear)

library(lme4)
library(lmerTest)

table(pm$Ptrmt)

pm$Ntrmt <- factor(  ifelse(pm$Treatment == "N" | pm$Treatment == "NP", "N", "NoN"))
pm$Ptrmt <- factor(  ifelse(pm$Treatment %in% c("P", "NP"), "P", "NoP"))

an <- lmer( mass ~ Ntrmt*Ptrmt*Lityear+Age  +(1|Stand), data=pm)

anova(an)


hist(lf[lf$Season=="Fall","Total_Mass"])

library(emmeans)
### answers here!  
# Or all combinations
emtrends(an, ~ Ntrmt * Ptrmt, var = "Lityear")


# Just N vs no N
emtrends(an, ~ Ntrmt, var = "Lityear")


##################################

no_N <- pm[pm$Ntrmt=="NoN",]

anova(lmer( mass ~ Lityear + (1|Stand), data=no_N))
lmer( mass ~ Lityear + (1|Stand), data=no_N)


##########

pm$Age<- factor(pm$Age, levels=c("Young forest","Mid-aged forest","Mature forest"))

ggplot( pm[!is.na(pm$Age),], aes(x=Lityear, y=mass))+
  geom_smooth( method="lm", se=F,alpha= .1,
               aes(group=Treatment, col=Treatment))+
  scale_color_manual(values=c("black","blue","red","purple"))+
  labs(x="Year", y="Litterfall g/m2")+
  theme_bw()+
  facet_wrap(~Age)


###########

pm$Age <- factor(pm$Age, levels = c("Young forest", "Mid-aged forest", "Mature forest"))

ggplot(pm[!is.na(pm$Age), ], aes(x = Lityear, y = mass)) +
  
  # ── Individual plot trajectories ─────────────────────────────────────────
  geom_line(
    aes(group = staplo, col = Treatment),
    linewidth = 0.4,
    alpha     = 0.35          # subtle — don't overwhelm the smooth
  ) +
  
  geom_point(
    aes(col = Treatment),
    size  = 1.2,
    alpha = 0.5
  ) +
  
  # ── Treatment trend lines ────────────────────────────────────────────────
  geom_smooth(
    method    = "lm",
    se        = FALSE,
    linewidth = 1.3,           # thicker so it reads over the raw lines
    aes(group = Treatment, col = Treatment)
  ) +
  
  scale_color_manual(values = c("Control" = "black",
                                "N"       = "blue",
                                "P"       = "red",
                                "NP"      = "purple")) +
  
  facet_wrap(~ Age) +
  
  labs(
    x        = "Year",
    y        = expression("Litterfall g m"^{-2}),
    col      = "Treatment"  ) +
  
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

