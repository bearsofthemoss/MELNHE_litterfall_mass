# Package ID: knb-lter-hbr.186.1 Cataloging System:https://pasta.edirepository.org.
# Data set title: Litter chemistry and masses for the MELNHE NxP fertilization experiment.
# Data set creator:  Melany Fisk - Miami University 
# Data set creator:  Ruth Yanai - SUNY-ESF 
# Data set creator:  Sunghoon Hong - SUNY-ESF 
# Data set creator:  Craig See - SUNY-ESF 
# Data set creator:  Shinjini Goswami - Miami University 
# Contact:    -  Hubbard Brook Ecosystem Study  - hbr-im@lternet.edu
# Stylesheet v2.15 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu      
# Uncomment the following lines to have R clear previous work, or set a working directory
# rm(list=ls())      

# setwd("C:/users/my_name/my_dir")       



options(HTTPUserAgent="EDI_CodeGen")


inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/186/1/f70aecfb317c6d1d7b90e7c312ebcbe1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl",extra=paste0(' -A "',getOption("HTTPUserAgent"),'"')))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site",     
                 "Stand",     
                 "Plot",     
                 "Treatment",     
                 "subplot",     
                 "year",     
                 "collection.time",     
                 "mass",     
                 "C",     
                 "N",     
                 "P",     
                 "Al",     
                 "Ca",     
                 "Fe",     
                 "K",     
                 "Mg",     
                 "Mn",     
                 "Na",     
                 "S"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
if (class(dt1$Stand)!="factor") dt1$Stand<- as.factor(dt1$Stand)
if (class(dt1$Plot)!="factor") dt1$Plot<- as.factor(dt1$Plot)
if (class(dt1$Treatment)!="factor") dt1$Treatment<- as.factor(dt1$Treatment)
if (class(dt1$subplot)!="factor") dt1$subplot<- as.factor(dt1$subplot)
if (class(dt1$collection.time)!="factor") dt1$collection.time<- as.factor(dt1$collection.time)
if (class(dt1$mass)=="factor") dt1$mass <-as.numeric(levels(dt1$mass))[as.integer(dt1$mass) ]               
if (class(dt1$mass)=="character") dt1$mass <-as.numeric(dt1$mass)
if (class(dt1$C)=="factor") dt1$C <-as.numeric(levels(dt1$C))[as.integer(dt1$C) ]               
if (class(dt1$C)=="character") dt1$C <-as.numeric(dt1$C)
if (class(dt1$N)=="factor") dt1$N <-as.numeric(levels(dt1$N))[as.integer(dt1$N) ]               
if (class(dt1$N)=="character") dt1$N <-as.numeric(dt1$N)
if (class(dt1$P)=="factor") dt1$P <-as.numeric(levels(dt1$P))[as.integer(dt1$P) ]               
if (class(dt1$P)=="character") dt1$P <-as.numeric(dt1$P)
if (class(dt1$Al)=="factor") dt1$Al <-as.numeric(levels(dt1$Al))[as.integer(dt1$Al) ]               
if (class(dt1$Al)=="character") dt1$Al <-as.numeric(dt1$Al)
if (class(dt1$Ca)=="factor") dt1$Ca <-as.numeric(levels(dt1$Ca))[as.integer(dt1$Ca) ]               
if (class(dt1$Ca)=="character") dt1$Ca <-as.numeric(dt1$Ca)
if (class(dt1$Fe)=="factor") dt1$Fe <-as.numeric(levels(dt1$Fe))[as.integer(dt1$Fe) ]               
if (class(dt1$Fe)=="character") dt1$Fe <-as.numeric(dt1$Fe)
if (class(dt1$K)=="factor") dt1$K <-as.numeric(levels(dt1$K))[as.integer(dt1$K) ]               
if (class(dt1$K)=="character") dt1$K <-as.numeric(dt1$K)
if (class(dt1$Mg)=="factor") dt1$Mg <-as.numeric(levels(dt1$Mg))[as.integer(dt1$Mg) ]               
if (class(dt1$Mg)=="character") dt1$Mg <-as.numeric(dt1$Mg)
if (class(dt1$Mn)=="factor") dt1$Mn <-as.numeric(levels(dt1$Mn))[as.integer(dt1$Mn) ]               
if (class(dt1$Mn)=="character") dt1$Mn <-as.numeric(dt1$Mn)
if (class(dt1$Na)=="factor") dt1$Na <-as.numeric(levels(dt1$Na))[as.integer(dt1$Na) ]               
if (class(dt1$Na)=="character") dt1$Na <-as.numeric(dt1$Na)
if (class(dt1$S)=="factor") dt1$S <-as.numeric(levels(dt1$S))[as.integer(dt1$S) ]               
if (class(dt1$S)=="character") dt1$S <-as.numeric(dt1$S)

# Convert Missing Values to NA for non-dates

dt1$mass <- ifelse((trimws(as.character(dt1$mass))==trimws("NA")),NA,dt1$mass)               
suppressWarnings(dt1$mass <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$mass))==as.character(as.numeric("NA"))),NA,dt1$mass))
dt1$C <- ifelse((trimws(as.character(dt1$C))==trimws("NA")),NA,dt1$C)               
suppressWarnings(dt1$C <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$C))==as.character(as.numeric("NA"))),NA,dt1$C))
dt1$N <- ifelse((trimws(as.character(dt1$N))==trimws("NA")),NA,dt1$N)               
suppressWarnings(dt1$N <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$N))==as.character(as.numeric("NA"))),NA,dt1$N))
dt1$P <- ifelse((trimws(as.character(dt1$P))==trimws("NA")),NA,dt1$P)               
suppressWarnings(dt1$P <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$P))==as.character(as.numeric("NA"))),NA,dt1$P))
dt1$Al <- ifelse((trimws(as.character(dt1$Al))==trimws("NA")),NA,dt1$Al)               
suppressWarnings(dt1$Al <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Al))==as.character(as.numeric("NA"))),NA,dt1$Al))
dt1$Ca <- ifelse((trimws(as.character(dt1$Ca))==trimws("NA")),NA,dt1$Ca)               
suppressWarnings(dt1$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Ca))==as.character(as.numeric("NA"))),NA,dt1$Ca))
dt1$Fe <- ifelse((trimws(as.character(dt1$Fe))==trimws("NA")),NA,dt1$Fe)               
suppressWarnings(dt1$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Fe))==as.character(as.numeric("NA"))),NA,dt1$Fe))
dt1$K <- ifelse((trimws(as.character(dt1$K))==trimws("NA")),NA,dt1$K)               
suppressWarnings(dt1$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$K))==as.character(as.numeric("NA"))),NA,dt1$K))
dt1$Mg <- ifelse((trimws(as.character(dt1$Mg))==trimws("NA")),NA,dt1$Mg)               
suppressWarnings(dt1$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mg))==as.character(as.numeric("NA"))),NA,dt1$Mg))
dt1$Mn <- ifelse((trimws(as.character(dt1$Mn))==trimws("NA")),NA,dt1$Mn)               
suppressWarnings(dt1$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Mn))==as.character(as.numeric("NA"))),NA,dt1$Mn))
dt1$Na <- ifelse((trimws(as.character(dt1$Na))==trimws("NA")),NA,dt1$Na)               
suppressWarnings(dt1$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$Na))==as.character(as.numeric("NA"))),NA,dt1$Na))
dt1$S <- ifelse((trimws(as.character(dt1$S))==trimws("NA")),NA,dt1$S)               
suppressWarnings(dt1$S <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt1$S))==as.character(as.numeric("NA"))),NA,dt1$S))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Site)
summary(Stand)
summary(Plot)
summary(Treatment)
summary(subplot)
summary(year)
summary(collection.time)
summary(mass)
summary(C)
summary(N)
summary(P)
summary(Al)
summary(Ca)
summary(Fe)
summary(K)
summary(Mg)
summary(Mn)
summary(Na)
summary(S) 
# Get more details on character variables

summary(as.factor(dt1$Site)) 
summary(as.factor(dt1$Stand)) 
summary(as.factor(dt1$Plot)) 
summary(as.factor(dt1$Treatment)) 
summary(as.factor(dt1$subplot)) 
summary(as.factor(dt1$collection.time))
detach(dt1)               


head(dt1)

library(ggplot2)

ggplot( dt1, aes(x= year, y= mass))+geom_boxplot(aes(group = year))

dt1[dt1$Stand=="C1",]

table(dt1$Treatment)
dt1$Treatment <- factor(dt1$Treatment, levels=c("Con","N","P","NP"))
dt1$staplo_bask <- paste(dt1$Stand, dt1$Plot, dt1$subplot)

table(dt1$staplo_bask, dt1$year)

avg <- aggregate(list(N = dt1$N) ,
                 by=list(
                   Site = dt1$Site,
                   Stand = dt1$Stand,
                   Plot = dt1$Plot,
                   year = dt1$year,
                   Treatment = dt1$Treatment
                 ),
                 FUN="mean", na.rm=T)


ggplot(avg[avg$Site=="Bartlett",], 
       aes(x= year, y= N, col=Treatment))+
  geom_point()+
  geom_line()+
  scale_color_manual(values=c("black","blue","red","purple"))+
  facet_wrap(~Stand)+
  labs(x="Year",y="Litterfall N (mg/g)")

out <- avg[avg$year=="2018"& avg$Site=="Bartlett",]

write.csv(out , 
          file="litter_N_2018.csv")
