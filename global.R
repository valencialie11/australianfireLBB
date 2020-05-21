library(tidyverse)
library(plotly)
library(glue)
library(shinydashboard)
library(shiny)
library(leaflet)
library(ggrepel)
library(DT)
library(lubridate)

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')
nasa_fire <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')


temperature %>% 
  is.na() %>% 
  colSums()

temp_new <- temperature %>% 
  mutate(temperature = replace_na(temperature, replace = mean(temperature, na.rm = T)))

temperature1 <-  temp_new %>% 
  mutate(year = year(date)) %>% 
  mutate(month = month(date, label = T, abbr = T))

temp111 <- temperature1 %>% 
  group_by(year, month, city_name, temp_type) %>% 
  summarise(mean_temperature = round(mean(temperature)))

temp112 <- temperature1 %>% 
  group_by(year, city_name, temp_type) %>% 
  summarise(mean_temp = mean(temperature))

temp112

temp222 <- temperature1 %>% 
  group_by(year, city_name) %>% 
  summarise(mean_temp = mean(temperature))
  
temp3 <-  temp222 %>% 
  arrange(year)

rainfall %>% 
  is.na() %>% 
  colSums()

rainfall[,"period"]

table(rainfall$quality)

rainfall_new <- rainfall %>% 
  mutate(rainfall = replace_na(rainfall, replace = mean(rainfall, na.rm = T))) %>%
  mutate(quality = replace_na(quality, replace = "Y"))

rainfall_new %>% 
  is.na() %>% 
  colSums()

rain1 <- rainfall_new %>% 
  select(-period)

rain1 %>% 
  is.na() %>% 
  colSums()
rain1

rain1 <- rain1 %>% 
  mutate(month = as.character(month))

rain1 <- rain1 %>% 
  arrange(month)

rain2 <- rain1 %>% 
  mutate(month = case_when(month == "01" ~ "January",
                   month == "02" ~ "February",
                   month == "03" ~ "March",
                   month == "04" ~ "April",
                   month == "05" ~ "May",
                   month == "06" ~ "June",
                   month == "07" ~ "July",
                   month == "08" ~ "August",
                   month == "09" ~ "September",
                   month == "10" ~ "October",
                   month == "11" ~ "November",
                   month == "12" ~ "December"))

rain2

rain111 <- rain2 %>% 
  group_by(year, month, long, lat, city_name) %>% 
  summarise(mean_rainfall = mean(rainfall))


rain112 <- rain2 %>% 
  group_by(year, month, city_name) %>% 
  summarise(mean_rain = mean(rainfall)) 

rain113 <- rain2 %>% 
  group_by(year, city_name) %>% 
  summarise(mean_rain1 = mean(rainfall))

rain113

rain4 <- as.data.frame(table(rain2$quality, rain2$city_name))
rain4
rain4 <- rain4 %>% 
  mutate(Var1 = case_when(Var1 == "Y" ~ "Good Quality",
                              Var1 == "N" ~ "Poor Quality" ))

rainnn <- as.data.frame(prop.table(table(rain2$quality, rain2$city_name), margin = 2))

rainperc <- rainnn %>%
  mutate(Freq = Freq*100)

rainperc

nasa_fire %>% 
  is.na() %>% 
  colSums()

nasaleaf <- nasa_fire %>% 
  group_by(longitude, latitude, acq_date) %>% 
  summarise(mean_bright = mean(brightness))

min(nasa_fire$brightness)
max(nasa_fire$brightness)

nasaleaf$colour <- sapply(nasaleaf$mean_bright, FUN = magcol)
nasaleaf$colour <- as.factor(nasaleaf$colour)

nasaleaf <- as.data.frame(nasaleaf)

nasaleaf$acq_date <- ymd(nasaleaf$acq_date)
