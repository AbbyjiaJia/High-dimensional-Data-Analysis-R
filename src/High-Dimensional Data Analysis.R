## clear data and variables  
rm(list=ls())

## Data Preparetation 
library(dplyr)
library(tidyverse)
library(ggplot2)

stock <-read.csv("stocks.csv")
head(stock)
summary(stock)


## Remove Total ESG risk score.

stock_rmESG <- stock %>% select(-"Name", -"Market", -"Sector", -"Industry",
                            -"Total.ESG.risk.score")
summary(stock_rmESG)

## Remove Forward.P.E & PEG.ratio..5.yr.expected. So many NA values --- meaningless

stock_rmNA <- stock_rmESG %>% select(-"Forward.P.E", -"PEG.ratio..5.yr.expected.")
summary(stock_rmNA)


# Preliminary Analysis 
## Boxplot

stock_rmNA %>%pivot_longer(cols = -Symbol,names_to='Variable',values_to='Value')%>%
  ggplot(aes(y=(Value)))+
  geom_boxplot()+
  scale_y_log10()+
  facet_wrap(~Variable, nrow = 2,scale='free_y') +
  theme(axis.text.x=element_blank())

#correlation table
all.num=stock_rmNA%>%select(-"Symbol") #remove non-numerical variables
all.num.nona=all.num[complete.cases(all.num),] #remove all na
round(cor(all.num.nona),5) #compute corr table

# PCA Analysis
## PCA
stock_rmNA %>% column_to_rownames(var="Symbol")%>% na.omit()%>%
  prcomp(scale = TRUE)->pca_total

summary(pca_total)
screeplot(pca_total,type="lines")
biplot(pca_total,scale=0, cex = 0.5)

### remove outliers
stock_rmNA%>%filter((Symbol %in% c("MSFT","AAPL", "RDSA.L", "LIN.DE", 
                                  "0006.HK", "1299.HK", "TSLA", 
                                  "0669.HK", "V1928.HK", 
                                  "1928.HK", "DB1.DE", "ILMN", "RTO.L", 
                                  "VILMN", "1038.HK"))==FALSE)%>%
  column_to_rownames(var="Symbol")%>% na.omit()%>%
  prcomp(scale = TRUE)->pca_rmoutliers

summary(pca_rmoutliers)

pca_rmoutliers$rotation
screeplot(pca_rmoutliers,type="lines")

#Correlation
biplot(pca_rmoutliers,scale=0,xlim=c(-4,4.5),ylim=c(-4,2.5),cex=0.5)
#Distance
biplot(pca_rmoutliers,cex=0.5)


## Remove Enterprise.value.revenue., Market.cap..intra.day.

stock_rmsame <- stock_rmNA %>% select(-"Enterprise.value.revenue", -"Market.cap..intra.day.")
summary(stock_rmsame)

## pca
stock_rmsame%>%filter((Symbol %in% c("MSFT","AAPL", "RDSA.L", "LIN.DE", 
                                  "0006.HK", "1299.HK", "TSLA", 
                                  "0669.HK", "V1928.HK", 
                                  "1928.HK", "DB1.DE", "ILMN", "RTO.L"))==FALSE)%>%
  column_to_rownames(var="Symbol")%>% na.omit()%>%
  prcomp(scale = TRUE)->pca_new
pca_new$rotation
summary(pca_new)
screeplot(pca_new,type="lines")
#Correlation
biplot(pca_new,scale=0,cex=0.5)
#Distance
biplot(pca_new,cex=0.5)





