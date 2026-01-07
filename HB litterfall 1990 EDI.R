
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


head(dt1)
table(dt1$TAG)
hist(dt1$DRY_MASS, breaks=200, main= "")
library(ggplot2)
ggplot(dt1[dt1$DRY_MASS>0 & dt1$DRY_MASS <100, ],
       aes(x=YEAR, y= DRY_MASS ))+
  geom_boxplot(aes(group = YEAR))

table(dt1$SITE, dt1$YEAR)

dim(dt1)

l <- dt1
l$id <- paste(l$SITE, l$COMP, l$YEAR,l$ELEV, l$DATE, l$TAG)

l$g_m2 <- l$DRY_MASS/.1

 l[ l$TAG=="pooled","DRY_MASS"] <- NA



summary(l$DRY_MASS)


c <- as.data.frame(table(l$id))
c[c$Freq>1,]


l[l$id %in% c("TF 2013 Low 2013-08 10",
              "W1 1999 High 1999-11 1",
              "W1 1999 High 1999-11 10"),]


a <- as.data.frame(table(l$id, l$YEAR))
a <- a[a$Freq>0,]
table(a$Freq)
dual <- a[a$Freq==2,]


l[l$id %in% dual$Var1, ]

head(l)
l[l$g_m2>3000,]

length(unique(l$id))
dim(l)

l <- l[!is.na(l$g_m2),]

ggplot( l[l$g_m2 < 800   , ], aes(x=YEAR, y=g_m2))+
  geom_point()+
  geom_line(aes(group= TAG ))+
  facet_grid(ELEV~SITE)



ggplot( l[l$SITE == "BB"   , ], aes(x=YEAR, y=g_m2))+
  geom_point()+
  geom_line(aes(group= TAG ))+
  facet_grid(ELEV~SITE)


h <- l[l$g_m2 < 1000, ]

y <- aggregate( h$g_m2,
           by=list( 
             Site = h$SITE,
             year = h$YEAR),
           FUN="median", na.rm=T)


ggplot(y, aes(x= year, y= x))+geom_point()+geom_line(aes(group=Site))
