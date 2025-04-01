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


# Preverjanje manjkajočih vrednosti
colSums(is.na(wildfires))
colSums(is.na(weather))
colSums(is.na(forecasts))
colSums(is.na(ndvi))
colSums(is.na(landclass))


#we removed these two columns ->too much missing values -> wont use it with analyzing fires therefore, their presence would only unnecessarily complicate further data processing and modeling. 
wildfires <- wildfires %>% select(-Std_confidence, -Var_confidence)



