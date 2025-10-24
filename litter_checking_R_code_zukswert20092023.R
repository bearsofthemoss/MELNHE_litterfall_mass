
#############################################################
#Code from Jenna for graphing litter for checking - adapted from code by Alex Young


#Install packages:
library(ggplot2)
library(tidyr)
library(gridExtra)
library(RColorBrewer)
#these packages are not needed for the fall graphs
library(dplyr)
library(plyr)


#FALL GRAPHS


#Import data
lit.all2<- read.csv("C:/Users/ssonoknowles/Downloads/MELNHE Litterfall EDI Data - Final Data Sheet for EDI.csv") #This is the csv of the "Master Data Sheet" in the EDI file

#Isolate fall data
lit.fall.sort2<-subset(lit.all2, Season == "Fall" & Sorted == "Y")

#Convert to long form
lgf2<- gather(lit.fall.sort2[,c(1:41)], "SP","mass",c(18:41))
lgf2$staplo<-paste(lgf2$Stand, lgf2$Plot)
lgf2$Year<-as.factor(lgf2$Year)
lgf2$mass<-as.numeric(lgf2$mass)

#Pick a color palette
n <- 27 # 19 is for 19 'species'
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

# nice way to visualize the colors.
pie(rep(1,n), col=sample(col_vector, n))
litcol<-col_vector #for convenience.

unique(lgf2$Basket)
###################################################################################
#TEMP CHANGES made by Suika messing around 10/03/25
#only use this code when you want simplified basket labels using the modern names. Only works for BEF plots 1-3.
#baskets may not correspond with other baskets in the same panel across red dotted line
#otherwise, use the original code below this section
#if after using this code, you need to then make different graphs (eg spring) clear environment then re-run code excluding this section

#streamline the basket labels so that there are only five
#This line is for all stands except for HBM, HBO, and JBM!!!!
simp_baskets <- c("LF1"="A1","LF2"="A3","LF3"="B2","LF4"="C1","LF5"="C3","1"="A1","2"="A3","3"="B2","4"="C1","5"="C3","A1"="A1","A3"="A3","B2"="B2","C1"="C1","C3"="C3")

#This line is for only HBM and JBM!
simp_baskets <- c("LF1"="A1","LF2"="A2","LF3"="CENTER","LF4"="B1","LF5"="B2","1"="A1","2"="A2","3"="CENTER","4"="B1","5"="B2","A1"="A1","A2"="A2","CENTER"="CENTER","B1"="B1","B2"="B2")

#This line is for only HBO!
simp_baskets <- c("LF1"="A1","LF2"="A3","LF3"="B2","LF4"="C1","LF5"="Y3","1"="A1","2"="A3","3"="B2","4"="C1","5"="Y3","A1"="A1","A3"="A3","B2"="B2","C1"="C1","Y3"="Y3")

lgf2$Basket <- simp_baskets[as.character(lgf2$Basket)]

# First, make Year a factor in correct order
lgf2$Year <- factor(lgf2$Year, levels = sort(unique(as.numeric(as.character(lgf2$Year)))))

# Subset
df_sub <- lgf2[lgf2$staplo == "HBM 2",]

# Create plot
ggplot(df_sub, aes(x = Year, y = mass, fill = SP)) + 
  geom_bar(stat = "identity", col = "black") + 
  theme_bw() +
  facet_wrap(~Basket, scales = "fixed", nrow = 2) +
  scale_fill_manual(values = litcol[1:length(unique(df_sub$SP))]) +
  theme(axis.text.x = element_text(angle = 90, vjust = .5)) +
  ggtitle("Stand HBM 2 over the years") +
  # Add vertical separator lines BETWEEN certain years
  #xintercept should be (2.5, 4.5) for all of BEF, and (3.5) for Hubbard and Jeffers Brooks
  geom_vline(xintercept = c(3.5), linetype = "dashed", color = "red", size = 1)
###################################################################################################

#Fall graphs that show all the basket labels
#Make the graphs - replace the stand/plot combination and ggtitle for each graph and re-run the code
m1<-ggplot(lgf2[lgf2$staplo=="C1 3",], aes(x=Year, y=mass, fill=SP))+geom_bar(stat="identity", col="black")+ theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
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


#Spring Graphs:

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


#Summer Graphs:
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

meanannuallfNP %>% filter(Stand == "JBO") %>%
  ggplot(aes(x = factor(Lityear), y = meanmassgm2, fill = factor(Lityear))) +
  geom_bar(stat = "identity")+
  facet_grid(Stand~Plot) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  ggtitle("JBO")


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

