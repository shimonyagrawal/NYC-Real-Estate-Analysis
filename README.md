# NYC Real Estate Analysis 

## Introduction 
The dataset is available on official NYC government website. The data includes real estate sales in New York City over the past 9 years. The data has characteristics of each property sale in each borough on the basis of sale date, sale price, year built, gross square fit, address and building code. The data includes the 5 boroughs of NYC – The Bronx, Brooklyn, Manhattan, Queens and Staten Island and more than 150 neighborhoods. The aim of the data is to help a real estate firm entering the neighborhood Madison in analysing the market scenario and predicting future sales.Download the dataset from https://drive.google.com/open?id=16Mh4nDk5TI3le1dzEGntcs63LgqmgB6a

The aim of the project was to make decisions on the profitability of the real estate firm by understanding the current market scenario and forecasting the sales for the next 8 quarters. 

## Analysis 
1. I first performed Data Wrangling on 1M records of real estate sales in NYC over the past 9 years. Using the cleaned data set, I built various supervised and unsupervised machine learning models. 
2. For the descriptive statistics, I first studied the summary statistics of the various featurs from dataset. Then I performed k-means clustering to compare selected features: Compare Median Sales for Residential properties of all neighborhoods , Number of Sales for Residential properties of  all neighborhoods  and  Proportion of Residential Sales for all neighborhoods. Lastly, to understand existing competitors, I performed t-test with nearby neighbourhoods. 
3. For the predictive statistics, I performed time series analysis and multiple linear regression to forecast the sales for the next 8 quarters. Both the models predicted an increase of $5m. Based on multiple linear regression model, I determined the sale prices for residential properties using key predictors like year built, building type, gross square feet and number of units sold. This helped in filtering our redundant variables to improve the performance.
4. For the prescriptive model, I built an optimisation model for the next 8 quarters to maximise the net present value by satisfying the constraints of budget, market penetration and employees hired and determining the optimal values of decreased commission rate and employees.The maximized net present value is $1.49m indicating high profitability in 2019-2020



