
# Hubbard Brook Fine Litterfall

options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/49/11/4b55c69068b625ec15027f1d28604e45" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "TRTMT",     
                 "COMP",     
                 "SITE",     
                 "ELEV",     
                 "YEAR",     
                 "DATE",     
                 "CELL",     
                 "TAG",     
                 "DRY_MASS",     
                 "M_COUNT",     
                 "M_MASS",     
                 "f_COUNT",     
                 "t_COUNT",     
                 "Q_COUNT",     
                 "B_COUNT",     
                 "B_MASS",     
                 "W_COUNT",     
                 "Y_COUNT",     
                 "Y_MASS",     
                 "P_COUNT",     
                 "a_COUNT",     
                 "COMMENT"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$TRTMT)!="factor") dt1$TRTMT<- as.factor(dt1$TRTMT)
if (class(dt1$COMP)!="factor") dt1$COMP<- as.factor(dt1$COMP)
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$ELEV)!="factor") dt1$ELEV<- as.factor(dt1$ELEV)
if (class(dt1$CELL)!="factor") dt1$CELL<- as.factor(dt1$CELL)
if (class(dt1$TAG)!="factor") dt1$TAG<- as.factor(dt1$TAG)
if (class(dt1$DRY_MASS)=="factor") dt1$DRY_MASS <-as.numeric(levels(dt1$DRY_MASS))[as.integer(dt1$DRY_MASS) ]               
if (class(dt1$DRY_MASS)=="character") dt1$DRY_MASS <-as.numeric(dt1$DRY_MASS)
if (class(dt1$M_COUNT)=="factor") dt1$M_COUNT <-as.numeric(levels(dt1$M_COUNT))[as.integer(dt1$M_COUNT) ]               
if (class(dt1$M_COUNT)=="character") dt1$M_COUNT <-as.numeric(dt1$M_COUNT)
if (class(dt1$M_MASS)=="factor") dt1$M_MASS <-as.numeric(levels(dt1$M_MASS))[as.integer(dt1$M_MASS) ]               
if (class(dt1$M_MASS)=="character") dt1$M_MASS <-as.numeric(dt1$M_MASS)
if (class(dt1$f_COUNT)=="factor") dt1$f_COUNT <-as.numeric(levels(dt1$f_COUNT))[as.integer(dt1$f_COUNT) ]               
if (class(dt1$f_COUNT)=="character") dt1$f_COUNT <-as.numeric(dt1$f_COUNT)
if (class(dt1$t_COUNT)=="factor") dt1$t_COUNT <-as.numeric(levels(dt1$t_COUNT))[as.integer(dt1$t_COUNT) ]               
if (class(dt1$t_COUNT)=="character") dt1$t_COUNT <-as.numeric(dt1$t_COUNT)
if (class(dt1$Q_COUNT)=="factor") dt1$Q_COUNT <-as.numeric(levels(dt1$Q_COUNT))[as.integer(dt1$Q_COUNT) ]               
if (class(dt1$Q_COUNT)=="character") dt1$Q_COUNT <-as.numeric(dt1$Q_COUNT)
if (class(dt1$B_COUNT)=="factor") dt1$B_COUNT <-as.numeric(levels(dt1$B_COUNT))[as.integer(dt1$B_COUNT) ]               
if (class(dt1$B_COUNT)=="character") dt1$B_COUNT <-as.numeric(dt1$B_COUNT)
if (class(dt1$B_MASS)=="factor") dt1$B_MASS <-as.numeric(levels(dt1$B_MASS))[as.integer(dt1$B_MASS) ]               
if (class(dt1$B_MASS)=="character") dt1$B_MASS <-as.numeric(dt1$B_MASS)
if (class(dt1$W_COUNT)=="factor") dt1$W_COUNT <-as.numeric(levels(dt1$W_COUNT))[as.integer(dt1$W_COUNT) ]               
if (class(dt1$W_COUNT)=="character") dt1$W_COUNT <-as.numeric(dt1$W_COUNT)
if (class(dt1$Y_COUNT)=="factor") dt1$Y_COUNT <-as.numeric(levels(dt1$Y_COUNT))[as.integer(dt1$Y_COUNT) ]               
if (class(dt1$Y_COUNT)=="character") dt1$Y_COUNT <-as.numeric(dt1$Y_COUNT)
if (class(dt1$Y_MASS)=="factor") dt1$Y_MASS <-as.numeric(levels(dt1$Y_MASS))[as.integer(dt1$Y_MASS) ]               
if (class(dt1$Y_MASS)=="character") dt1$Y_MASS <-as.numeric(dt1$Y_MASS)
if (class(dt1$P_COUNT)=="factor") dt1$P_COUNT <-as.numeric(levels(dt1$P_COUNT))[as.integer(dt1$P_COUNT) ]               
if (class(dt1$P_COUNT)=="character") dt1$P_COUNT <-as.numeric(dt1$P_COUNT)
if (class(dt1$a_COUNT)=="factor") dt1$a_COUNT <-as.numeric(levels(dt1$a_COUNT))[as.integer(dt1$a_COUNT) ]               
if (class(dt1$a_COUNT)=="character") dt1$a_COUNT <-as.numeric(dt1$a_COUNT)
if (class(dt1$COMMENT)!="factor") dt1$COMMENT<- as.factor(dt1$COMMENT)

# Convert Missing Values to NA for non-dates

dt1$DRY_MASS <- ifelse((trimws(as.character(dt1$DRY_MASS))==trimws("-9999")),NA,dt1$DRY_MASS)               
suppressWarnings(dt1$DRY_MASS <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$DRY_MASS))==as.character(as.numeric("-9999"))),NA,dt1$DRY_MASS))
dt1$M_COUNT <- ifelse((trimws(as.character(dt1$M_COUNT))==trimws("-9999")),NA,dt1$M_COUNT)               
suppressWarnings(dt1$M_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$M_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$M_COUNT))
dt1$M_MASS <- ifelse((trimws(as.character(dt1$M_MASS))==trimws("-9999")),NA,dt1$M_MASS)               
suppressWarnings(dt1$M_MASS <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$M_MASS))==as.character(as.numeric("-9999"))),NA,dt1$M_MASS))
dt1$f_COUNT <- ifelse((trimws(as.character(dt1$f_COUNT))==trimws("-9999")),NA,dt1$f_COUNT)               
suppressWarnings(dt1$f_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$f_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$f_COUNT))
dt1$t_COUNT <- ifelse((trimws(as.character(dt1$t_COUNT))==trimws("-9999")),NA,dt1$t_COUNT)               
suppressWarnings(dt1$t_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$t_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$t_COUNT))
dt1$Q_COUNT <- ifelse((trimws(as.character(dt1$Q_COUNT))==trimws("-9999")),NA,dt1$Q_COUNT)               
suppressWarnings(dt1$Q_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$Q_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$Q_COUNT))
dt1$B_COUNT <- ifelse((trimws(as.character(dt1$B_COUNT))==trimws("-9999")),NA,dt1$B_COUNT)               
suppressWarnings(dt1$B_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$B_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$B_COUNT))
dt1$B_MASS <- ifelse((trimws(as.character(dt1$B_MASS))==trimws("-9999")),NA,dt1$B_MASS)               
suppressWarnings(dt1$B_MASS <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$B_MASS))==as.character(as.numeric("-9999"))),NA,dt1$B_MASS))
dt1$W_COUNT <- ifelse((trimws(as.character(dt1$W_COUNT))==trimws("-9999")),NA,dt1$W_COUNT)               
suppressWarnings(dt1$W_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$W_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$W_COUNT))
dt1$Y_COUNT <- ifelse((trimws(as.character(dt1$Y_COUNT))==trimws("-9999")),NA,dt1$Y_COUNT)               
suppressWarnings(dt1$Y_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$Y_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$Y_COUNT))
dt1$Y_MASS <- ifelse((trimws(as.character(dt1$Y_MASS))==trimws("-9999")),NA,dt1$Y_MASS)               
suppressWarnings(dt1$Y_MASS <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$Y_MASS))==as.character(as.numeric("-9999"))),NA,dt1$Y_MASS))
dt1$P_COUNT <- ifelse((trimws(as.character(dt1$P_COUNT))==trimws("-9999")),NA,dt1$P_COUNT)               
suppressWarnings(dt1$P_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$P_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$P_COUNT))
dt1$a_COUNT <- ifelse((trimws(as.character(dt1$a_COUNT))==trimws("-9999")),NA,dt1$a_COUNT)               
suppressWarnings(dt1$a_COUNT <- ifelse(!is.na(as.numeric("-9999")) & (trimws(as.character(dt1$a_COUNT))==as.character(as.numeric("-9999"))),NA,dt1$a_COUNT))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(TRTMT)
summary(COMP)
summary(SITE)
summary(ELEV)
summary(YEAR)
summary(DATE)
summary(CELL)
summary(TAG)
summary(DRY_MASS)
summary(M_COUNT)
summary(M_MASS)
summary(f_COUNT)
summary(t_COUNT)
summary(Q_COUNT)
summary(B_COUNT)
summary(B_MASS)
summary(W_COUNT)
summary(Y_COUNT)
summary(Y_MASS)
summary(P_COUNT)
summary(a_COUNT)
summary(COMMENT) 
# Get more details on character variables

summary(as.factor(dt1$TRTMT)) 
summary(as.factor(dt1$COMP)) 
summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$ELEV)) 
summary(as.factor(dt1$CELL)) 
summary(as.factor(dt1$TAG)) 
summary(as.factor(dt1$COMMENT))
detach(dt1)               

# rename for convenience
l <- dt1

# create ID for each record
l$id <- paste(l$SITE, l$COMP, l$YEAR,l$ELEV, l$DATE, l$TAG)

# View boxplots
ggplot(l, aes(x=YEAR, y=g_m2))+geom_boxplot(aes(group=YEAR))+
  facet_grid(COMP ~ SITE)

# convert -999 values to NA
l[l == -999] <- NA

# convert mass to g/m2
l$g_m2 <- l$DRY_MASS/.097

#summary(l$g_m2)

# some years were pooled?  ignore.
l <- l[ !l$TAG=="pooled",]

# View dist
hist(l$g_m2, breaks=200)

# Remove small values
l <- l[l$g_m2 > 0,]

# Remove large values
l <- l[l$g_m2 < 1000,]



# Average to the elevation-site-year level
year_sum  <- aggregate(list(g_m2 =l$g_m2),
          by=list(
            YEAR = l$YEAR,
                   SITE= l$SITE,
                   ELEV = l$ELEV,
                   TAG = l$TAG),
          FUN="sum", na.rm=T)

site_avg <- aggregate(list(g_m2 =year_sum$g_m2),
                      by=list(
                        YEAR = year_sum$YEAR,
                        SITE= year_sum$SITE,
                        ELEV = year_sum$ELEV),
                      FUN="mean", na.rm=T)






ggplot(site_avg, aes(x=YEAR,y= g_m2, col=ELEV))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="Litterfall mass (g/m2)",
       col="Elevation")+
  geom_hline(yintercept=210)+
  facet_wrap(~SITE, nrow=2)


library(lme4)
library(lmerTest)
library(emmeans)

# Simple mixed effect model
mod <- lmer( g_m2 ~ YEAR   + (1|ELEV), data = site_avg[site_avg$SITE=="BB",])
mod
anova(mod)
mod <- lmer( g_m2 ~ YEAR   + (1|ELEV), data = site_avg[site_avg$SITE=="BB",])



library(ggplot2)
library(emmeans)

# Get your trend
trend <- emtrends(mod, ~ 1, var = "YEAR")

# Extract the slope and confidence intervals
trend_summary <- summary(trend)
slope <- trend_summary$YEAR.trend

# Get predictions from the model
# Create a prediction dataset
pred_data <- data.frame(
  YEAR = seq(min(site_avg[site_avg$SITE=="BB",]$YEAR), 
             max(site_avg[site_avg$SITE=="BB",]$YEAR), 
             length.out = 100)  # Use mean elevation for prediction
)

# Get predictions
pred_data$predicted <- predict(mod, newdata = pred_data, re.form = NA)

# Create the plot
ggplot() +
  # Raw data points
  geom_point(data = site_avg[site_avg$SITE=="BB",], 
             aes(x = YEAR, y = g_m2, color = factor(ELEV)), 
             alpha = 0.6, size = 2) +
  # Model prediction line (fixed effect)
  geom_line(data = pred_data, 
            aes(x = YEAR, y = predicted), 
            color = "blue", linewidth = 1.2) +
  labs(x = "Year", 
       y = expression(g/m^2),
       color = "Elevation",
       title = "Litterfall Mass Over Time",
       subtitle = paste0("Slope: ", round(slope, 3), " g/m²/year")) +
  theme_minimal()



head(l)
names(l)
l$total_count <-  l$a_COUNT+ l$B_COUNT+ l$f_COUNT + l$M_COUNT +
  l$P_COUNT+ l$Q_COUNT + l$W_COUNT + l$Y_COUNT + l$t_COUNT

l$count_g <- l$total_count / l$DRY_MASS


ggplot(l, aes(x=YEAR, y= count_g ))+
  geom_boxplot( aes(group=YEAR))



# Average to the elevation-site-year level
year_sum  <- aggregate(list(count_g =l$count_g),
                       by=list(
                         YEAR = l$YEAR,
                         SITE= l$SITE,
                         ELEV = l$ELEV,
                         TAG = l$TAG),
                       FUN="sum", na.rm=T)

site_avg <- aggregate(list(count_g =year_sum$count_g),
                      by=list(
                        YEAR = year_sum$YEAR,
                        SITE= year_sum$SITE,
                        ELEV = year_sum$ELEV),
                      FUN="mean", na.rm=T)


ggplot(site_avg, aes(x=YEAR,y= count_g, col=ELEV))+
  geom_point()+
  geom_line()+
  labs(x="Year", y="Litterfall mass (g/m2)",
       col="Elevation")+
  facet_wrap(~SITE, nrow=2)


## lower mass per unit area.

s <- aggregate(site_avg$count_g, by=list(
  year = site_avg$YEAR,
  SITE = site_avg$SITE
), FUN="mean", na.rm=T)

s <- s[s$x>0,]
s
ss <- tidyr::spread(s, "SITE" ,"x")
??spread()



