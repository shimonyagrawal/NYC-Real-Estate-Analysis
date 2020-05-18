BOROUGH <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/BOROUGH.xlsx")
NEIGHBORHOOD <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/NEIGHBORHOOD.xlsx")
BUILDING_CLASS <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/BUILDING_CLASS.xlsx")
NYC_HISTORICAL <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/NYC_HISTORICAL.xlsx")


install.packages("readxl")
install.packages("DBI")
install.packages("odbc")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("cluster")
install.packages("factoextra")

library(readxl)
library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)


#Descriptive Statistics for Neighborhood Madison (149)
#create dataframe with required data and filter N/A or missing values


NYCdf <- NYC_HISTORICAL %>%
  left_join(NEIGHBORHOOD, by= "NEIGHBORHOOD_ID") %>%
  left_join(BUILDING_CLASS, by= c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID")) %>%
  select (NEIGHBORHOOD_ID, NEIGHBORHOOD_NAME, SALE_DATE, SALE_PRICE, GROSS_SQUARE_FEET, RESIDENTIAL_UNITS,COMMERCIAL_UNITS, TYPE) %>%
  filter(SALE_PRICE >0) %>%
  separate(SALE_DATE, c("Year", "Month", "Date"))%>%
  group_by(Year)

view(NYCdf)

#Q: Provide descriptive statistics for real estate sales in your neighborhood
#Total Sales since 2009 (Madison)
Madison <- filter(NYCdf,NEIGHBORHOOD_ID=="149", Year> 2008) %>%
  group_by(Year)
Madison.Sales <- summarise(Madison, TotalSales = n())%>%
  filter (TotalSales > 0)
view(Madison.Sales)

ggplot()+geom_line(data=Madison.Sales, size=1, aes(x=Year, y=TotalSales, group=1, color="blue"))+
  scale_color_discrete(name="Madison", label="Sales")

#Mean sales and mean gross square feet since 2009 (Madison)
Madison.SalesSqFt <- filter(NYCdf,SALE_PRICE>0, GROSS_SQUARE_FEET>0, TYPE=="RESIDENTIAL", NEIGHBORHOOD_ID=="149") %>%
  summarise(TotalSalePrice = sum(SALE_PRICE), TotalGrossFeet= sum(GROSS_SQUARE_FEET))
Madison.MeanSaleSqFt <- summarise (Madison.SalesSqFt, MeanSales= mean(TotalSalePrice), MeanGrossFeet = mean(TotalGrossFeet)) 
view(Madison.SalesSqFt)
view(Madison.MeanSalesSqFt)

#five number summary for sales and gross square feet 
summary(Madison.SalesSqFt)

#proportion of Residential, Commercial, mixed and other sales 
Madison.Proportion <- NYCdf %>%
  select(NEIGHBORHOOD_NAME, NEIGHBORHOOD_ID, TYPE, RESIDENTIAL_UNITS, COMMERCIAL_UNITS)
Madison.Proportion <- group_by(NYCdf, TYPE) %>%
  drop_na() %>%
  summarise(Total_Units = (sum(RESIDENTIAL_UNITS, na.rm = T) + sum(COMMERCIAL_UNITS, na.rm = T))) %>%
  mutate(Proportion = Total_Units/sum(Total_Units))

#StandardDeviation for Sales Prices in Residential properties
SdSalePrices.Residential <- filter(NYCdf, TYPE=="RESIDENTIAL", SALE_PRICE>0, NEIGHBORHOOD_ID==149, Year>2008)%>%
  group_by(Year)%>%
  select(SALE_PRICE, TYPE,RESIDENTIAL_UNITS)%>%
  summarise(TotalSales = sum(SALE_PRICE, na.rm = T), TotalResidentialUnits = sum(RESIDENTIAL_UNITS, na.rm = T)) 
summarise(SdSalePrices.Residential, SdSalePrice= sd(TotalSales))
view(SdSalePrices.Residential)

#Correlation between sale price and gross square feet in residential units
cor(Madison.SalesSqFt[c(2,3)])  

#Q: Perform K-means clustering, comparing your neighborhood to other neighborhoods
#Comparing K-means cluster for Median Sales for Residential properties of all neighborhoods
KPI1 <- filter(NYCdf, TYPE=="RESIDENTIAL", SALE_PRICE>0,Year>2008)%>%
  group_by(NEIGHBORHOOD_ID)%>%
  select(SALE_PRICE, TYPE, RESIDENTIAL_UNITS) %>%
  summarise(TotalSales = sum(SALE_PRICE, na.rm = T), 
         TotalResidentialUnits = sum(RESIDENTIAL_UNITS, na.rm = T), 
         MedSales=median(TotalSales))
view(KPI1)

zscores1 <- select(KPI1, "MedSales", "TotalResidentialUnits")
zscores1 <- as.data.frame(scale(zscores1))

clustering1 <- kmeans(zscores1, centers = 5)
clustering1[["centers"]]

zscores1 <- cbind(zscores1, cluster=clustering1$cluster)
zScores1 <- cbind(KPI1[c(1)], zscores1)

#finding the optimal number of clusters using the elbow method
k.max <- 15
data <- zscores1
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k)$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

ggplot(zScores1)+ 
  geom_point (aes(x=TotalResidentialUnits, y=MedSales, color=as.factor(cluster)))


#Comparing K-means cluster for Number of Sales for Residential properties of  all neighborhoods  
KPI2 <- filter(NYCdf, SALE_PRICE>0,Year>2008)%>%
  group_by(NEIGHBORHOOD_ID)%>%
  select(SALE_PRICE, TYPE, RESIDENTIAL_UNITS, COMMERCIAL_UNITS)%>%
  drop_na()%>%
  summarise(TotalUnits=sum(RESIDENTIAL_UNITS+COMMERCIAL_UNITS), TotalResidentialUnits = sum(RESIDENTIAL_UNITS, na.rm = T)) 
                            
zscores2 <- select(KPI2, "TotalResidentialUnits" , "TotalUnits")
zscores2 <- scale(zscores2)
view(zscores2)
                            
clustering2 <- kmeans (zscores2, centers = 5)
clustering2[["centers"]]

zscores2 <- cbind(zscores2, cluster=clustering2$cluster)
zScores2 <- cbind.data.frame(KPI2[c(1)], zscores2)


#finding the optimal number of clusters using the elbow method                            
k.max <- 15
data <- zscores2
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k)$tot.withinss})

wss
plot(1:k.max, 
     wss,type="b",pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
                            
ggplot(zScores2)+geom_point(aes(x=TotalUnits, y=TotalResidentialUnits, color=as.factor(cluster)))

#Comparing K-means cluster for Proportion of Residential Sales for all neighborhoods
KPI3 <- filter (NYCdf,Year>2008,SALE_PRICE>0, TYPE=="RESIDENTIAL")%>%
  group_by(NEIGHBORHOOD_ID)%>%
  select(RESIDENTIAL_UNITS, NEIGHBORHOOD_ID) %>%
  summarise(TotalUnits=n(), TotalResUnits = sum(RESIDENTIAL_UNITS))%>%
  drop_na()%>%
  mutate(Proportion=TotalUnits/sum(TotalUnits)) 
                            
zscores3 <- select(KPI3,"TotalResUnits", "Proportion")
zscores3 <- scale(zscores3)
view(zscores3)
                            
clustering3 <- kmeans (zscores3, centers = 5)
clustering3[["centers"]]
                            
zscores3 <- cbind(zscores3, cluster=clustering3$cluster)
zScores3 <- cbind.data.frame(KPI3[c(1)], zscores3)

#finding the optimal number of clusters using the elbow method                            
k.max <- 15
data <- zscores3
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k)$tot.withinss})
                           
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE,
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
                            
ggplot(zScores3)+geom_point(aes(x=TotalResUnits, y=Proportion, color=as.factor(cluster)))

                            
                            
#Q3:Choose one other neighborhood and compare the average residential property costs with your neighborhood. 
#calculating revenue and cost   
NYC_Madison <- filter(NYCdf,NEIGHBORHOOD_ID=="149", Year> 2008) %>%
  group_by(Year) %>%
  select ("SALE_PRICE", "RESIDENTIAL_UNITS", "COMMERCIAL_UNITS")
Revenue_Madison <- summarise(NYC.Madison, Revenue = sum(SALE_PRICE, na.rm = T), TotalResUnits=sum(RESIDENTIAL_UNITS))%>%
  filter (Revenue > 0) %>%
  drop_na() %>%
  mutate (AveragePropertyCost=Revenue/TotalResUnits)
                            
NYC_LongIsland <- filter(NYCdf,NEIGHBORHOOD_ID=="147", Year>2008) %>%
  group_by(Year)%>%
  select ("SALE_PRICE", "RESIDENTIAL_UNITS", "COMMERCIAL_UNITS")
Revenue_LongIsland <- summarise(NYC.LongIsland, Revenue = sum(SALE_PRICE, na.rm = T), TotalResUnits=sum(RESIDENTIAL_UNITS))%>%
  filter (Revenue > 0) %>%
  drop_na()%>%
  mutate (AveragePropertyCost=Revenue/TotalResUnits)
                            
#descriptivestatistics_Madison 
summarise(Revenue_Madison, MadisonMeanRevenue = mean(Revenue))
                            
#descriptivestatistics_LongIsland
summarise(Revenue_LongIsland, LIMeanRevenue = mean(Revenue))
                            
#t test
t.test(x= Revenue_Madison$AveragePropertyCost, y=Revenue_LongIsland$AveragePropertyCost, alternative = "t", conf.level = 0.95)
