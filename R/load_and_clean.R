#import librarys

library(dplyr)
library(readr)
library(tidyr)


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

#how many rows from forecasts_wide match wildfires by Region and Date. 
# how many forecasts actually match observed fires (important for meaningful grouping).

nrow(semi_join(forecasts_wide, wildfires, by = c("Region", "Date")))

#we removed these two columns ->too much missing values ->
#wont use it with analyzing fires therefore, their presence would only
#complicate further data processing and modeling. 
wildfires <- wildfires %>% select(-Std_confidence, -Var_confidence)

names(wildfires)  # izpiše imena stolpcev – ne bi smelo biti več "Std_confidence" in "Var_confidence"


# Format dates to a common format
wildfires$Date <- format(as.Date(wildfires$Date, format = "%m/%d/%Y"), "%Y-%m-%d")
ndvi$Date <- format(as.Date(ndvi$Date, format = "%m/%d/%Y"), "%Y-%m-%d")
weather$Date <- as.character(weather$Date)
forecasts$Date <- as.character(forecasts$Date)

names(forecasts) <- names(forecasts) %>%
  gsub("count\\(\\)\\[unit: km\\^2\\]", "count_km2", .) %>%
  gsub("min\\(\\)", "min_val", .) %>%
  gsub("max\\(\\)", "max_val", .) %>%
  gsub("mean\\(\\)", "mean_val", .) %>%
  gsub("variance\\(\\)", "var_val", .)

names(weather) <- names(weather) %>%
  gsub("count\\(\\)\\[unit: km\\^2\\]", "count_km2", .) %>%
  gsub("min\\(\\)", "min_val", .) %>%
  gsub("max\\(\\)", "max_val", .) %>%
  gsub("mean\\(\\)", "mean_val", .) %>%
  gsub("variance\\(\\)", "var_val", .)


#are there duplicate records by combination of Region, Date and Parameter in weather data (no)
weather %>% count(Region, Date, Parameter) %>% filter(n > 1)

#each of the 7 regions in weather has the same number of records
weather %>% count(Region)  # ali n_distinct(weather$Region)

#each region has exactly one record
landclass %>% count(Region)


weather_wide <- weather %>%
  #chose parameters important for wildfires
  filter(Parameter %in% c("Temperature", "Precipitation", "RelativeHumidity", "WindSpeed")) %>%
  
  #we convert the data from long format to wide format, Each combination of Region + Date becomes one record.
  #This gives us a structured, transparent set of weather data, where we have all values for all parameters in one row per day.
  #for each selected weather variable (e.g. temperature), it creates five separate columns with information about area coverage, 
  #minimum, maximum, average, and variance.
  pivot_wider(names_from = Parameter, values_from = c(count_km2, min_val, max_val, mean_val, var_val))


forecasts_wide <- forecasts %>%
  filter(Parameter %in% c("Temperature", "Precipitation", "RelativeHumidity", "WindSpeed"),
         `Lead time` == 5) %>%
  pivot_wider(names_from = Parameter, values_from = c(count_km2, min_val, max_val, mean_val, var_val))

# Join all data together based on Region and Date
combined_data <- wildfires %>%
  left_join(weather_wide, by = c("Region", "Date")) %>%
  left_join(forecasts_wide, by = c("Region", "Date")) %>%
  left_join(ndvi, by = c("Region", "Date")) %>%
  left_join(landclass, by = "Region")

# Explanation:
# We used left_join because the wildfires dataset is our primary data source,
# and we want to keep all wildfire records while adding matching information from other
# datasets (like weather, forecasts, NDVI, and land class). If a match is not found in the other datasets,
# the wildfire record is still kept, and the new columns will contain NA.



# Missing values
colSums(is.na(combined_data))


head(weather)

head(weather_wide)


# Filtriraj samo podatke od leta 2017 naprej
combined_data <- combined_data %>%
  filter(as.Date(Date) >= as.Date("2017-01-01"))


nrow(combined_data)
range(as.Date(combined_data$Date))

# Število manjkajočih vrednosti po stolpcih
na_counts <- colSums(is.na(combined_data))
na_counts[na_counts > 0]

# Odstotek manjkajočih vrednosti po stolpcih
na_percent <- colMeans(is.na(combined_data)) * 100
na_percent[na_percent > 0]



 #Odstrani forecast .y stolpce in NDVI (več kot 50–90% manjkajočih)
combined_data_clean <- combined_data %>%
  select(-contains(".y"),
       -"Lead time",
        -starts_with("Vegetation_index"))



# (Neobvezno) ponovno preveri dimenzije
dim(combined_data_clean)

# Preveri NA vrednosti
colSums(is.na(combined_data_clean))

# Dodaj log-transformirano ciljno spremenljivko
#We used log-transformation to reduce the influence of extremely large values,
#stabilize the distribution, and improve the performance of the models.
combined_data_clean$log_estimated_fire_area <- log1p(combined_data_clean$Estimated_fire_area)


write_csv(combined_data_clean, "combined_data_clean.csv")



names(combined_data_clean)


