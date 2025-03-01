library(dplyr)
library(randomForest)
library(lubridate)
library(tidyverse)
library(rvest)


-------------------------------------------------
#DC Code:
  
fulldata <- read.csv("DC.csv")

fulldata2 <- fulldata[!is.na(fulldata$TMIN), ]

filtered_data <- fulldata %>%
  filter(NAME == "NATIONAL ARBORETUM DC, MD US")

cherry <- read.csv("peak-bloom-prediction-main/data/washingtondc.csv") |> 
  bind_rows(read.csv("peak-bloom-prediction-main/data/liestal.csv")) |> 
  bind_rows(read.csv("peak-bloom-prediction-main/data/kyoto.csv")) |> 
  bind_rows(read.csv("peak-bloom-prediction-main/data/vancouver.csv")) |> 
  bind_rows(read.csv("peak-bloom-prediction-main/data/nyc.csv"))



cherry <- cherry %>%
  mutate(bloom_date = as.Date(bloom_date))

cherry <- cherry %>%
  mutate(Year = as.numeric(format(bloom_date, "%Y")))

dc_bloom <- cherry %>%
  filter(location == "washingtondc") %>%
  select(Year, bloom_date)

filtered_data <- filtered_data %>%
  mutate(Date = as.Date(DATE)) %>%      
  mutate(Year = as.numeric(format(Date, "%Y"))) 

data <- left_join(filtered_data, dc_bloom, by = "Year")


data <- data %>%
  mutate(Date = as.Date(Date),
         bloom_date = as.Date(bloom_date)) 

data <- data %>%
  mutate(TMIN = as.numeric(TMIN),
         TMAX = as.numeric(TMAX))

data <- data %>%
  arrange(Date) %>%
  group_by(Year) %>%
  mutate(TMIN = ifelse(is.na(TMIN), lag(TMIN, default = mean(TMIN, na.rm = TRUE)), TMIN),
         TMAX = ifelse(is.na(TMAX), lag(TMAX, default = mean(TMAX, na.rm = TRUE)), TMAX)) %>%
  ungroup()

data <- data %>%
  mutate(Mean_Temp = (TMIN + TMAX) / 2)

cumulative_temp <- data %>%
  group_by(Year) %>%
  arrange(Date) %>%
  mutate(Cum_Temp = cumsum(Mean_Temp)) %>%
  filter(Date == bloom_date) %>%
  mutate(bloom_numeric = as.numeric(bloom_date - as.Date(paste0(Year, "-01-01"))))  


cumulative_temp <- na.omit(cumulative_temp)

set.seed(123)  
rf_model <- randomForest(bloom_numeric ~ Cum_Temp, data = cumulative_temp, ntree = 500)

avg_cum_temp <- cumulative_temp %>%
  summarize(avg_Cum_Temp = mean(Cum_Temp, na.rm = TRUE)) %>%
  pull(avg_Cum_Temp)

predicted_bloom_numeric <- predict(rf_model, newdata = data.frame(Cum_Temp = avg_cum_temp))


-----------------------------------------------
#Kyoto Code:
  
  
japan <- read.csv("Kyoto.csv")
japan_filtered <- japan %>%
  filter(STATION == "JA000047759")

japan_filtered <- japan %>%
  filter(STATION == "JA000047759")

japan_filtered <- japan_filtered %>%
  mutate(DATE = as.Date(DATE),
         Year = year(DATE))  

japan_filtered <- japan_filtered %>%
  mutate(TAVG = as.numeric(TAVG))

japan_filtered <- japan_filtered %>%
  arrange(DATE) %>%
  group_by(Year) %>%
  mutate(TAVG = ifelse(is.na(TAVG), lag(TAVG, default = mean(TAVG, na.rm = TRUE)), TAVG)) %>%
  ungroup()

japan_merged <- japan_filtered %>%
  left_join(cherry %>% filter(location == "kyoto"), by = c("Year" = "year"))

japan_merged <- japan_merged %>%
  mutate(bloom_date = as.Date(bloom_date))

cumulative_temp_japan <- japan_merged %>%
  group_by(Year) %>%
  arrange(DATE) %>%
  mutate(Cum_Temp = cumsum(TAVG)) %>%
  filter(DATE == bloom_date) %>%
  mutate(bloom_numeric = as.numeric(bloom_date - as.Date(paste0(Year, "-01-01"))))  

cumulative_temp_japan <- na.omit(cumulative_temp_japan)


set.seed(123) 
rf_model_japan <- randomForest(bloom_numeric ~ Cum_Temp, data = cumulative_temp_japan, ntree = 500)

avg_cum_temp_japan <- cumulative_temp_japan %>%
  summarize(avg_Cum_Temp = mean(Cum_Temp, na.rm = TRUE)) %>%
  pull(avg_Cum_Temp)

predicted_bloom_numeric_japan <- predict(rf_model_japan, newdata = data.frame(Cum_Temp = avg_cum_temp_japan))



-------------------------------------------
#Liestal Code:
  
liestal <- read.csv("Liestal.csv")

liestal <- liestal %>%
  mutate(DATE = as.Date(DATE),
         Year = year(DATE),
         TAVG = (TMAX + TMIN) / 2)  

liestal <- liestal %>%
  arrange(DATE) %>%
  group_by(Year) %>%
  mutate(TAVG = ifelse(is.na(TAVG), lag(TAVG, default = mean(TAVG, na.rm = TRUE)), TAVG)) %>%
  ungroup()

liestal_merged <- liestal %>%
  left_join(cherry %>% filter(location == "liestal"), by = c("Year" = "year"))

liestal_merged <- liestal_merged %>%
  mutate(bloom_date = as.Date(bloom_date))

cumulative_temp_liestal <- liestal_merged %>%
  group_by(Year) %>%
  arrange(DATE) %>%
  mutate(Cum_Temp = cumsum(TAVG)) %>%
  filter(DATE == bloom_date) %>%
  mutate(bloom_numeric = as.numeric(bloom_date - as.Date(paste0(Year, "-01-01"))))  

cumulative_temp_liestal <- na.omit(cumulative_temp_liestal)



set.seed(123)  
rf_model_liestal <- randomForest(bloom_numeric ~ Cum_Temp, data = cumulative_temp_liestal, ntree = 500)

avg_cum_temp_liestal <- cumulative_temp_liestal %>%
  summarize(avg_Cum_Temp = mean(Cum_Temp, na.rm = TRUE)) %>%
  pull(avg_Cum_Temp)

predicted_bloom_numeric_liestal <- predict(rf_model_liestal, newdata = data.frame(Cum_Temp = avg_cum_temp_liestal))



-------------------------------------
#New York Code:
  
  newyork <- read.csv("newyork.csv")

newyork <- newyork %>%
  mutate(DATE = as.Date(DATE),
         Year = year(DATE),
         TAVG = (TMAX + TMIN) / 2)

newyork <- newyork %>%
  arrange(DATE) %>%
  group_by(Year) %>%
  mutate(TAVG = ifelse(is.na(TAVG), lag(TAVG, default = mean(TAVG, na.rm = TRUE)), TAVG)) %>%
  ungroup()

newyork_merged <- newyork %>%
  left_join(cherry %>% filter(location == "washingtondc"), by = c("Year" = "year"))

newyork_merged <- newyork_merged %>%
  mutate(bloom_date = as.Date(bloom_date))

cumulative_temp_newyork <- newyork_merged %>%
  group_by(Year) %>%
  arrange(DATE) %>%
  mutate(Cum_Temp = cumsum(TAVG)) %>%
  filter(DATE == bloom_date) %>%
  mutate(bloom_numeric = as.numeric(bloom_date - as.Date(paste0(Year, "-01-01"))))  

cumulative_temp_newyork <- na.omit(cumulative_temp_newyork)

set.seed(123)  
rf_model_newyork <- randomForest(bloom_numeric ~ Cum_Temp, data = cumulative_temp_newyork, ntree = 500)

avg_cum_temp_newyork <- cumulative_temp_newyork %>%
  summarize(avg_Cum_Temp = mean(Cum_Temp, na.rm = TRUE)) %>%
  pull(avg_Cum_Temp)

predicted_bloom_numeric_newyork <- predict(rf_model_newyork, newdata = data.frame(Cum_Temp = avg_cum_temp_newyork))

predicted_bloom_numeric_newyork <- max(60, min(120, predicted_bloom_numeric_newyork))


------------------------------
#Vancouver Code:
vancouver <- read.csv("vancouver.csv")

vancouver <- vancouver %>%
  mutate(DATE = as.Date(DATE),
         Year = year(DATE),
         TAVG = (TMAX + TMIN) / 2)

vancouver <- vancouver %>%
  arrange(DATE) %>%
  group_by(Year) %>%
  mutate(TAVG = ifelse(is.na(TAVG), lag(TAVG, default = mean(TAVG, na.rm = TRUE)), TAVG)) %>%
  ungroup()

vancouver_merged <- vancouver %>%
  left_join(cherry %>% filter(location == "washingtondc"), by = c("Year" = "year"))

vancouver_merged <- vancouver_merged %>%
  mutate(bloom_date = as.Date(bloom_date))

cumulative_temp_vancouver <- vancouver_merged %>%
  group_by(Year) %>%
  arrange(DATE) %>%
  mutate(Cum_Temp = cumsum(TAVG)) %>%
  filter(DATE == bloom_date) %>%
  mutate(bloom_numeric = as.numeric(bloom_date - as.Date(paste0(Year, "-01-01"))))  

cumulative_temp_vancouver <- na.omit(cumulative_temp_vancouver)

set.seed(123)  
rf_model_vancouver <- randomForest(bloom_numeric ~ Cum_Temp, data = cumulative_temp_vancouver, ntree = 500)

avg_cum_temp_vancouver <- cumulative_temp_vancouver %>%
  summarize(avg_Cum_Temp = mean(Cum_Temp, na.rm = TRUE)) %>%
  pull(avg_Cum_Temp)

predicted_bloom_numeric_vancouver <- predict(rf_model_vancouver, newdata = data.frame(Cum_Temp = avg_cum_temp_vancouver))

predicted_bloom_numeric_vancouver <- max(60, min(120, predicted_bloom_numeric_vancouver))


