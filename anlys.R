

f24 <- read.csv( "D:/Users/bears/Downloads/fall 2004 litter.csv")

f24 <- f24[ , c(1,2,3,4,5)]
# cm <- tidyr::spread(f24, "Description","Sampleweight")
# 
# cm$year <- 2004
head(cm)


library(ggplot2)
ggplot(f24, aes(x=Plot, y= Sampleweight, fill=Description))+
  geom_col(position="stack", col="black")+
  facet_wrap(~Site)+
  ggtitle("Litter 2004")


## ALl the data

lf <- read.csv("D:/Users/bears/Downloads/MELNHE Litterfall EDI Data - Final Data Sheet for EDI(6).csv")

lf$Basket

str(lf)
table(lf$Year, lf$Basket)
table( lf$Basket)

head(lf)

## View outliers in whole basket mass

ggplot(lf, aes(x=Lityear, y=Leaf_Mass, group=Treatment, col=Treatment ) )+
  geom_point()+facet_wrap(~Season)+
  geom_smooth(method="lm")



ggplot(lf, aes(x=Lityear, y=Total_Mass, group=Treatment, col=Treatment ) )+
  geom_point()+facet_wrap(~Season)+
  geom_smooth(method="lm", nrow=3)



