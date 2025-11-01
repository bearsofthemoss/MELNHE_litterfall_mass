
#############################################################
#Code from Jenna for graphing litter for checking - adapted from code by Alex Young


#Install packages:
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
library(here)
library(dplyr)
# library(plyr)  # dplyr is useful, plyr leaks like a sieve and should be avoided
library(readr)
#FALL GRAPHS


#Import data using the code below for bartlett stands, but there may still be some errors Alex!
lit.all <- read_csv("C:/Users/ssonoknowles/Downloads/MELNHE Litterfall EDI Data - Final Data Sheet for EDI.csv", quote = "\"")
#lit.all2<- read.csv( here::here("data","MELNHE Litterfall EDI Data 2025-10-26.csv")) #This is the csv of the "Master Data Sheet" in the EDI file
#import using the code below for hubbard and jeffers brook
lit.all<- read.csv("C:/Users/ssonoknowles/Downloads/MELNHE Litterfall EDI Data - Final Data Sheet for EDI.csv")

#Isolate fall data
lit.fall.sort<-subset(lit.all, Season == "Fall" & Sorted == "Y")

#Convert to long form
names(lit.fall.sort)
# columns ACPE to Nonleaf?   Do we want other spring, other, twigs? or problematic?
lgf<- gather(lit.fall.sort[,c(1:40)], "SP","mass",c(19:40))
lgf$staplo<-paste(lgf$Stand, lgf$Plot)
lgf$Year<-as.factor(lgf$Year)
lgf$mass<-as.numeric(lgf$mass)

#Pick a color palette
# why 27?
length(unique(lgf$Basket)) # I see 19 unique species

n <- 27 # 19 is for 19 'species'
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# nice way to visualize the colors.
pie(rep(1,n), col=sample(col_vector, n))
litcol<-col_vector #for convenience.

#streamline the basket labels so that there are only five
#This line is for all stands except for HBM, HBO, and JBM!!!!
simp_baskets <- c("LF1"="A1","LF2"="A3","LF3"="B2","LF4"="C1","LF5"="C3","1"="A1","2"="A3","3"="B2","4"="C1","5"="C3","A1"="A1","A3"="A3","B2"="B2","C1"="C1","C3"="C3")

#This line is for only HBM and JBM!
simp_baskets_HBM_JBM <- c("LF1"="A1","LF2"="A2","LF3"="CENTER","LF4"="B1","LF5"="B2","1"="A1","2"="A2","3"="CENTER","4"="B1","5"="B2","A1"="A1","A2"="A2","CENTER"="CENTER","B1"="B1","B2"="B2")

#This line is for only HBO!
simp_baskets_HBO <- c("LF1"="A1","LF2"="A3","LF3"="B2","LF4"="C1","LF5"="Y3","1"="A1","2"="A3","3"="B2","4"="C1","5"="Y3","A1"="A1","A3"="A3","B2"="B2","C1"="C1","Y3"="Y3")

lgf$Basket <- simp_baskets_HBM_JBM[as.character(lgf$Basket)]


# First, make Year a factor in correct order
lgf$Year <- factor(lgf$Year, levels = sort(unique(as.numeric(as.character(lgf$Year)))))

#create plot
df_sub <- lgf[lgf$staplo == "HBM 1",]

ggplot(df_sub, aes(x = Year, y = mass, fill = SP)) + 
  geom_bar(stat = "identity", col = "black") + 
  theme_bw() +
  facet_wrap(~Basket, scales = "fixed", nrow = 2) +
  scale_fill_manual(values = litcol[1:length(unique(df_sub$SP))]) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  ggtitle("Stand HBM 1 over the years") +
  # Add vertical separator lines BETWEEN certain years
  #xintercept should be (2.5, 4.5) for all of BEF plots, and (3.5) for Hubbard and Jeffers Brooks
  geom_vline(xintercept = c(2.5), linetype = "dashed", color = "red", size = 1)
head(lgf)

#Fall graphs that show all the basket labels
#Make the graphs - replace the stand/plot combination and ggtitle for each graph and re-run the code
m1<-ggplot(lgf[lgf$staplo=="C1 3",], aes(x=Year, y=mass, fill=SP))+geom_bar(stat="identity", col="black")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  facet_wrap(~Basket, scales="fixed", nrow=2)+
  scale_fill_manual( values= litcol)+
  theme(axis.text.x = element_text(angle = 90, vjust =.5))+
  ggtitle("Stand C1 3 over the years")
m1


# Example of one total mass graphs from one stand:

lit.fall.jb<-subset(lit.all2, Season =="Fall" & Stand == "C1"|Season == "Fall" & Stand =="C1")
###########run the following line only if you want to combine all the baskets into the 5 modern labels
lit.fall.jb$Basket <- simp_baskets[as.character(lit.fall.jb$Basket)]
###########
lit.fall.jb$staplo<-paste(lit.fall.jb$Stand, lit.fall.jb$Plot)
lit.fall.jb$Year<-as.factor(lit.fall.jb$Year)
lit.fall.jb$Total_Mass<-as.numeric(lit.fall.jb$Total_Mass)


lit.fall.jb  %>% filter(lit.fall.jb$Stand == "C1") %>%
  ggplot(aes(x = factor(Year), y = Total_Mass, fill = factor(Year))) +
  geom_bar(stat = "identity") +
  facet_grid(Plot~Basket) +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Total Masses Fall C1") +
  #if not combining the old basket labels delete the line below as well as the plus sign above
  geom_vline(xintercept = c(2.5, 4.5), linetype = "dashed", color = "red", linewidth = 1)


# Spring Graphs:

lit.spring<-subset(lit.all2, Season =="Spring")
######run the following line only if you want to combine all the baskets into the 5 modern labels
lit.spring$Basket <- simp_baskets[as.character(lit.spring$Basket)]
######
lit.spring$Total_Mass<-as.numeric(lit.spring$Total_Mass)
lit.spring %>% filter(Stand == "C1") %>%
  ggplot(aes(x = factor(Year), y = Total_Mass, fill = factor(Year))) +
  geom_bar(stat = "identity")+
  facet_grid(Plot~Basket) +
  theme_bw()+
  scale_fill_manual(values = litcol[1:length(unique(lit.spring$Year))]) +
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Spring Litterfall C1") +
  #if not combining the old basket labels delete the line below as well as the plus sign above
  #if using JB or HB sites, change the xintercept to c(1.5)
  geom_vline(xintercept = c(2.5), linetype = "dashed", color = "red", linewidth = 1)


# Summer Graphs:

lit.summer<-subset(lit.all2, Season =="Summer")
######run the following line only if you want to combine all the baskets into the 5 modern labels
lit.summer$Basket <- simp_baskets[as.character(lit.summer$Basket)]
######
lit.summer$Stand<-as.factor(lit.summer$Stand)
lit.summer$Total_Mass<-as.numeric(lit.summer$Total_Mass)
lit.summer %>% filter(Stand == "C1") %>%
  ggplot(aes(x = factor(Year), y = Total_Mass, fill = factor(Year))) +
  geom_bar(stat = "identity")+
  facet_grid(Plot~Basket) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("Summer Litterfall C1") +
  #if not combining the old basket labels delete the line below as well as the plus sign above
  #if using JB or HB sites, change the xintercept to c(2.5)
  geom_vline(xintercept = c(3.5), linetype = "dashed", color = "red", linewidth = 1)



#Total Graphs (Annual):

lit.all2$Total_Mass<-as.numeric(lit.all2$Total_Mass)
lit.all2$Total_Mass_g_m2<-as.numeric(lit.all2$Total_Mass_g_m2)
lit.all2$N<-as.factor(lit.all2$N)
lit.all2$P<-as.factor(lit.all2$P)


annuallf<-ddply(lit.all2, c("Site","Stand","Plot","Lityear","Treatment","N","P","Ca","Basket"), summarise,
                totalmass=sum(Total_Mass, na.rm=F),
                totalmassgm2=sum(Total_Mass_g_m2, na.rm=F))


annuallf %>% filter(Stand == "JBO") %>%
  ggplot(aes(x = factor(Lityear), y = totalmassgm2, fill = factor(Lityear))) +
  geom_bar(stat = "identity")+
  facet_grid(Plot~Basket) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("JBO")

meanannuallf<-ddply(annuallf, c("Site","Stand","Plot","Lityear","Treatment","N","P","Ca"), summarise,
                    meanmass=mean(totalmass, na.rm=T),
                    meanmassgm2=mean(totalmassgm2,na.rm=T))


meanannuallfNP<-subset(meanannuallf, Treatment !="Ca")
meanannuallfNP<-subset(meanannuallfNP, Stand=="C1"|Stand=="C2"|Stand=="C4"|Stand=="C6"|Stand=="C8"|Stand=="C9"|Stand=="HBM"|Stand=="HBO"|Stand=="JBM"|Stand=="JBO")

meanannuallfNP %>% filter(Stand == "C1") %>%
  ggplot(aes(x = factor(Lityear), y = meanmassgm2, fill = factor(Lityear))) +
  geom_bar(stat = "identity")+
  facet_grid(Stand~Plot) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("C1")


#total annual litterfall graphs with stacked seasons by plot

seasonal_annual_mass <- ddply(
  lit.all2,
  c("Site", "Stand", "Plot", "Lityear", "Season", "Treatment", "N", "P", "Ca", "Basket"),
  summarise,
  totalmass_gm2 = sum(Total_Mass_g_m2, na.rm = TRUE))

seasonal_annual_mass %>%
  filter(Stand == "C1") %>%
  ggplot(aes(x = factor(Lityear), y = totalmass_gm2, fill = Season)) +
  geom_bar(stat = "identity") +
  facet_grid(Plot ~ .) +
  scale_fill_manual(values = c("Fall" = "#E69F00", "Spring" = "#56B4E9", "Summer" = "#009E73")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("C1 – Seasonal Contribution to Annual Litterfall Mass")


##################################################
#here is what AI gave me when I tried to fix the averages that were including missing baskets as 0s rather than NAs
#another issue that popped up was the earlier years had way higher averages b/c all the seasons were weighed equally
#now these combine the seasons to just average the totals, but 2019 still looks strangely low
#Alex you probably can do this much neater than what I have below but maybe this works?

#annual totals graph
detach("package:plyr", unload=TRUE)

lit.all2$Total_Mass <- as.numeric(lit.all2$Total_Mass)
lit.all2$Total_Mass_g_m2 <- as.numeric(lit.all2$Total_Mass_g_m2)
lit.all2$N <- as.factor(lit.all2$N)
lit.all2$P <- as.factor(lit.all2$P)


# 1️⃣ Sum all seasons per basket for each litterfall year
annual_basket_totals <- lit.all2 %>%
  group_by(Site, Stand, Plot, Lityear, Basket) %>%
  summarise(
    basket_total_gm2 = sum(Total_Mass_g_m2, na.rm = TRUE)  # sum all seasons
  ) %>%
  ungroup()

# 2️⃣ Compute mean annual litterfall per stand/plot
mean_annual_totals <- annual_basket_totals %>%
  group_by(Site, Stand, Plot, Lityear) %>%
  summarise(
    mean_annual_gm2 = mean(basket_total_gm2, na.rm = TRUE),
    n_baskets_used = sum(!is.na(basket_total_gm2))
  ) %>%
  ungroup()

# 3️⃣ Plot for a specific Stand, e.g., C1
mean_annual_totals %>%
  filter(Stand == "C1") %>%
  ggplot(aes(x = factor(Lityear), y = mean_annual_gm2, fill = factor(Lityear))) +
  geom_bar(stat = "identity") +
  facet_grid(Plot ~ .) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("C1 – Mean Annual Litterfall per Plot (All Seasons Combined)")


#stacked season graphs
# 1️⃣ Sum litterfall per basket for each season within a litterfall year
seasonal_basket_totals <- lit.all2 %>%
  group_by(Site, Stand, Plot, Lityear, Basket, Season) %>%
  summarise(
    basket_season_total = sum(Total_Mass_g_m2, na.rm = TRUE)
  ) %>%
  ungroup()

# 2️⃣ Compute mean seasonal totals per plot
mean_seasonal_totals <- seasonal_basket_totals %>%
  group_by(Site, Stand, Plot, Lityear, Season) %>%
  summarise(
    mean_season_gm2 = mean(basket_season_total, na.rm = TRUE),
    n_baskets_used = sum(!is.na(basket_season_total))
  ) %>%
  ungroup()

# 3️⃣ Plot seasonal contributions for a specific Stand, e.g., C1
mean_seasonal_totals %>%
  filter(Stand == "C1") %>%
  ggplot(aes(x = factor(Lityear), y = mean_season_gm2, fill = Season)) +
  geom_bar(stat = "identity") +
  facet_grid(Plot ~ .) +
  scale_fill_manual(values = c("Fall" = "#E69F00", "Spring" = "#56B4E9", "Summer" = "#009E73")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle("C1 – Seasonal Contribution to Annual Litterfall (Mean per Plot)")
