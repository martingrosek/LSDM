#import librarys

library(dplyr)
library(readr)


#import data

path<- "data/spot-challenge-wildfires/Jan_09"

wildfires <- read_csv(file.path(path, "Historical_Wildfires.csv"))
weather <- read_csv(file.path(path, "HistoricalWeather.csv"))
forecasts <- read_csv(file.path(path, "HistoricalWeatherForecasts.csv"))
ndvi <- read_csv(file.path(path, "VegetationIndex.csv"))
landclass <- read_csv(file.path(path, "LandClass.csv"))

# Osnovni pregled
summary(wildfires)
summary(weather)
summary(forecasts)
summary(ndvi)
summary(landclass)


head(wildfires)