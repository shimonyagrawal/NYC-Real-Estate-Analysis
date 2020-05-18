BOROUGH <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/BOROUGH.xlsx")
NEIGHBORHOOD <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/NEIGHBORHOOD.xlsx")
BUILDING_CLASS <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/BUILDING_CLASS.xlsx")
NYC_HISTORICAL <- read_excel("/Users/shimonyagrawal/Desktop/NYC Real Estate/NYC_HISTORICAL.xlsx")

install.packages("readxl")
install.packages("DBI")
install.packages("odbc")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("GGally")
install.packages("forecast")

library(DBI)
library(odbc)
library(tidyverse)
library(lubridate)
library (GGally)
library(forecast)
library(readxl)

#Predictive Statistics for Neighborhood Madison (149) to forecast sales for next 8 quarters
#create dataframe with required data and filter N/A or missing values



NYCdf <- NYC_HISTORICAL %>%
  left_join(NEIGHBORHOOD, by= "NEIGHBORHOOD_ID") %>%
  left_join(BUILDING_CLASS, by= c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID")) %>%
  select (NEIGHBORHOOD_ID, NEIGHBORHOOD_NAME, SALE_DATE, SALE_PRICE, GROSS_SQUARE_FEET, RESIDENTIAL_UNITS,COMMERCIAL_UNITS, TYPE) %>%
  filter(SALE_PRICE >0, TYPE == "RESIDENTIAL", GROSS_SQUARE_FEET > 0 ) %>%
  mutate(Year = year(SALE_DATE), Quarter = quarter(SALE_DATE)) %>%
  select(TYPE, SALE_PRICE, Quarter, Year, NEIGHBORHOOD_ID)

view(NYCdf)

#Time series analysis on the total dollar amount of residential real estate sales in Madison using sales beginning in the year 2009 to develop the model. 
#Developed a forecast for the next 8 quarters of sales.

forecast <- NYCdf %>%
  filter(NEIGHBORHOOD_ID == 149, SALE_PRICE > 0, Year > 2008) %>%
  mutate(t = as.numeric(Year)*4 + Quarter - 2009*4) %>%
  group_by(t) %>%
  summarise (TotalSales = sum(SALE_PRICE))  


Timeseries_Madison <- ts(forecast$TotalSales, start = c(2009,1), frequency = 4)
ets_madison <- ets(Timeseries_Madison, model = "MAN")
Forecast_Madison <- forecast (ets_madison, 8)
decomposets <- decompose(timeseries_madison, type  = "multiplicative")

plot(decomposets)
plot(ets_madison)
plot(Forecast_Madison)
summary(Forecast_Madison)
checkresiduals(Forecast_Madison)


#Use a multiple regression model to come up with another forecast for the next 8 quarters of sales. 
#Include time and seasonality. Use sales beginning in the year 2009 to develop your model.

forecast <- cbind(forecast, c("Q1", "Q2","Q3","Q4"))
names(forecast)[3] <- "Quarter"

#regression including time   
regression_time <- lm ( data=forecast, formula = TotalSales~t)
summary(regression_time)
x <- data.frame(t=c(41,42,43,44,45,46,47,48), TotalSales = c(0,0,0,0,0,0,0,0), Quarter=c("Q1", "Q2", "Q3", "Q4"))
predict.lm(regression_time, x, interval = "confidence")

#regression using time and seasonality 
regression_timeseason <- lm(data = forecast, TotalSales~t+Quarter)
summary(regression_timeseason)
y <- data.frame(t=c(41,42,43,44,45,46,47,48), TotalSales = c(0,0,0,0,0,0,0,0), Quarter = c("Q1", "Q2", "Q3", "Q4"))
predict.lm(regression_timeseason, y, interval = "confidence")

#	Use a multiple regression model to determine the sale of a given residential property in your neighborhood. Include:
#a.Sale Date
#b.Year built
#c.Building type (categorical)
#d.Gross Square Feet
#e.Number of Units

NYC.df1 <- NYC_HISTORICAL %>%
  left_join(NEIGHBORHOOD, by= "NEIGHBORHOOD_ID") %>%
  left_join(BUILDING_CLASS, by= c("BUILDING_CLASS_FINAL_ROLL"="BUILDING_CODE_ID")) %>%
  select (NEIGHBORHOOD_ID, NEIGHBORHOOD_NAME, SALE_DATE, DESCRIPTION, YEAR_BUILT, ADDRESS, SALE_PRICE,GROSS_SQUARE_FEET, RESIDENTIAL_UNITS,COMMERCIAL_UNITS, TYPE, BUILDING_CLASS_FINAL_ROLL) %>%
  filter(SALE_PRICE >0, TYPE == "RESIDENTIAL", GROSS_SQUARE_FEET > 0 ) %>%
  mutate (Year = year(SALE_DATE),Quarter = quarter(SALE_DATE))%>%
  select (BUILDING_CLASS_FINAL_ROLL, NEIGHBORHOOD_ID, TYPE,SALE_DATE, DESCRIPTION, ADDRESS, Year, YEAR_BUILT, SALE_PRICE, RESIDENTIAL_UNITS, GROSS_SQUARE_FEET)

view(NYC.df1)

Madison_Regression <- NYC.df1 %>% 
  filter (Year>2008, NEIGHBORHOOD_ID == 149) %>%
  select (SALE_DATE,YEAR_BUILT, SALE_PRICE, GROSS_SQUARE_FEET,BUILDING_CLASS_FINAL_ROLL,RESIDENTIAL_UNITS)

Madison_Model <- lm(SALE_PRICE~. , data = Madison_Regression)
summary (Madison_Model)

#Properties that are the biggest bargains and most expensive 
Madison_Address <- NYC.df1 %>%
  filter (Year>2008, NEIGHBORHOOD_ID == 149) %>%
  select (ADDRESS,DESCRIPTION)

Madison_Regression["Residuals"] <- Madison_Model$residuals
Madison_Regression["Address"] <- Madison_Address$ADDRESS
Madison_Regression["Description"] <- Madison_Address$DESCRIPTION