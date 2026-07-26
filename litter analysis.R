
#Install packages:
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(lme4)
library(lmerTest)
library(emmeans)
# FALL GRAPHS

#Import data
#lf <-  read.csv("D:/Users/bears/Downloads/MELNHE Litterfall EDI Data - Final Data Sheet for EDI(6).csv")
lf <- read.csv(here::here("data","MELNHE Litterfall EDI Data June2026.csv"))

lf$staplo <-paste(lf$Stand, lf$Plot)

table(lf$Treat)

table(lf$Season, lf$Lityear)
head(lf)
str(lf)

lf$g_m2 <- lf$Total_Mass / lf$Basket_Area

lf[lf$g_m2>400,]



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
table(is.na(pm$Age), pm$Stand)

table(pm$Treatment, pm$Stand)

pm <- pm[!is.na(pm$Age),]

 pm[pm$Plot==7, "Plot"] <- "4"
 pm[pm$staplo=="HBO 7", "staplo"] <- "HBO 4"
 
 table(pm$Treatment)

 ggplot( pm[pm$Lityear>2007,], aes(x=Lityear, y=mass))+
  geom_smooth( method="lm", se=F,alpha= .1,
               aes(group=staplo, col=Treatment))+
geom_point( aes( col=Treatment))+
 geom_line( aes(group=staplo))+
  scale_color_manual(values=c("black","blue","red","purple"))+
  facet_wrap(~ Stand, nrow=3)+

  labs(x="Year", y="Litterfall g/m2")+
  theme_bw()+
  ggtitle("Fall litter production")

 
 ##############

 library(ggplot2)
 
 d <- pm[pm$Lityear > 2007, ]

 d <- d[d$mass>0,]
  
 # ---- Build the 3x5 layout with two blanks in the top row ----
 # Row 1: C1, C2, C3, [blank], [blank]  <- legend goes in this open space
 # Row 2: C4, C5, C6, HBM, JBM
 # Row 3: C7, C8, C9, HBO, JBO
 stand_order <- c("C1","C2","C3", "BLANK1", "BLANK2",
                  "C4","C5","C6","HBM","JBM",
                  "C7","C8","C9","HBO","JBO")
 
 d$Stand <- factor(d$Stand, levels = stand_order)
 
 ggplot(d, aes(x = Lityear, y = mass)) +
   geom_smooth(method = "lm", se = FALSE, alpha = .1,
               aes(group = staplo, col = Treatment)) +
   geom_point(aes(col = Treatment)) +
   geom_line(aes(group = staplo)) +
   scale_color_manual(values = c("black","blue","red","purple")) +
   facet_wrap(~ Stand, nrow = 3, drop = FALSE) +
   labs(x = "Year", y = "Litterfall g/m2") +
   theme_bw() +
   ggtitle("Fall litter production") +
   theme(legend.position = c(0.87, 0.87),      # top-right, in the empty panels
         legend.background = element_rect(fill = "white", color = "grey70"),
         legend.key = element_rect(fill = "white"))+
   ylim(150 , 450)

 
 
 ##########################


library(lme4)
library(lmerTest)

table(d$Treatment)
d$Ntrmt <- factor(  ifelse(d$Treatment == "N" | d$Treatment == "NP", "N", "NoN"))
d$Ptrmt <- factor(  ifelse(d$Treatment %in% c("P", "NP"), "P", "NoP"))


an <- lmer( mass ~ Ntrmt*Ptrmt*Lityear+Age  +(1|Stand), data=d)

anova(an)

##########

library(ggplot2)
library(lme4)
library(lmerTest)

d <- pm[pm$Lityear > 2007, ]
d$staplo <- paste(d$Stand, d$Plot)

# ---- unique staplo list ----
staplo_list <- unique(d$staplo)
length(staplo_list)   # should be 52

# ---- loop: slope + intercept per plot ----
slopes <- data.frame()

for (s in staplo_list) {
  x <- d[d$staplo == s, ]
  if (length(unique(x$Lityear)) < 3) next          # need >=3 years
  
  fit <- lm(mass ~ Lityear, data = x)
  cf  <- summary(fit)$coefficients
  
  slopes <- rbind(slopes, data.frame(
    staplo      = s,
    Stand       = x$Stand[1],
    Plot        = x$Plot[1],
    Treatment   = x$Treatment[1],
    intercept   = cf[1, 1],
    slope       = cf[2, 1],
    slope_se    = cf[2, 2],
    slope_p     = cf[2, 4],
    r2          = summary(fit)$r.squared,
    n_years     = nrow(x),
    mean_mass   = mean(x$mass, na.rm = TRUE),       # the "collapsed" plot average
    row.names   = NULL))
}

# treatment factors + Age
slopes$Ntrmt <- factor(ifelse(slopes$Treatment %in% c("N","NP"),  "N", "NoN"))
slopes$Ptrmt <- factor(ifelse(slopes$Treatment %in% c("P","NP"),  "P", "NoP"))
slopes$Age <- pm$Age[match(slopes$staplo, pm$staplo)]

print(slopes)
print(table(slopes$Treatment))

# ---- Analysis of slopes (Stehman & Meredith style) ----
mod <- lmer(slope ~ Ntrmt * Ptrmt + Age + (1 | Stand), data = slopes)
anova(mod)
summary(mod)

emm_N <- emmeans(mod, ~ Ntrmt)
emm_N                    # estimate = mean slope in each group, with CI
pairs(emm_N)             # N vs NoN difference, with p-value


# same for the collapsed plot mean, if useful
mod_mean <- lmer(mean_mass ~ Ntrmt * Ptrmt + Age + (1 | Stand), data = slopes)
anova(mod_mean)

# ---- Plot 1: each plot's slope, 0-centered, faceted by stand ----
stand_order <- c("C1","C2","C3","C4","C5","C6","HBM","JBM",
                 "C7","C8","C9","HBO","JBO")
slopes$Stand <- factor(slopes$Stand, levels = stand_order)

ggplot(slopes, aes(x = slope, y = Treatment, col = Treatment)) +
  geom_vline(xintercept = 0, linetype = "dashed", col = "grey40") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = slope - slope_se, xmax = slope + slope_se),
                 height = 0.2) +
  facet_wrap(~ Stand, nrow = 3) +
  scale_color_manual(values = c("black","blue","red","purple")) +
  labs(x = "Slope (g/m2 per year)", y = "",
       title = "Litterfall trend per plot, collapsed through time") +
  theme_bw()

# ---- Plot 2: all 52 plots on one axis, by treatment ----
ggplot(slopes, aes(x = Treatment, y = slope, col = Treatment)) +
  geom_hline(yintercept = 0, linetype = "dashed", col = "grey40") +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 5, col = "black") +
  scale_color_manual(values = c("black","blue","red","purple")) +
  labs(y = "Slope (g/m2 per year)",
       title = "Per-plot litterfall slopes by treatment",
       subtitle = "Each point = one plot; diamond = treatment mean") +
  theme_bw()

