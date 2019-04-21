
# Climatological data from National Oceanic and Atmospheric Administration
# april 2019

# List of weather stations:
# http://old.wetterzentrale.de/klima/stnlst.html
# add '99999' to station codes

#install.packages("rnoaa")

# example:
# data <- lcd(station = "71164099999" , year = "2019")

# documentation:

# ISD ftp source:
# ftp://ftp.ncdc.noaa.gov/pub/data/noaa

######################################################
# DK 
######################################################

library(rnoaa)
library(tidyverse)
library(lubridate)

# years

years <- as.character(2000:2019)
  
# Stations in DK

odense     <- "06120099999"
esbjerg    <- "06080099999"
kastrup    <- "06180099999"
aalborg    <- "06030099999"
tirstrup   <- "06070099999"
sonderborg <- "06118099999"

stations <- c(odense,esbjerg,kastrup,aalborg,tirstrup,sonderborg)

df <- NULL

for (station in stations) {
  for (year in years) {
    d <- lcd(station = station, year = year)
    d <- d %>% mutate_all(as.character)
    
    df <- bind_rows(df, d)
    print(station)
    print(year)
  }
}

df <- df %>% 
  mutate(date_time = parse_date_time(date,orders = "Ymd HMS")) %>%
  mutate(date = as.Date(date)) %>%
  #mutate(tmp  = str_replace(tmp, "[+]","")) %>%
  mutate(tmp = str_replace(tmp,",",".")) %>%
  mutate(tmp = as.numeric(tmp)) %>%
  filter(tmp < 999)
  

write_csv(df, "weather_DK_2000-2019_lcd.csv")

df %>%
  ggplot(aes(x=name,y=tmp)) +
  geom_boxplot()

df %>% 
  filter(date > "2010-01-01") %>%
  mutate(ym = as.yearmon(date)) %>%
  group_by(name,ym) %>%
  summarise(m = mean(tmp,na.rm=T)) %>%
  ggplot(aes(x=ym, y=m, colour=name)) +
  geom_line(alpha=.3, size = 1)

# ISD

df2 <- NULL

for (station in stations) {
  for (year in years) {
    
    d <- isd(usaf = substr(station, start = 1, stop = 6),wban = "99999", year = year, force = T)

    df2 <- bind_rows(df2, d)
    print(station)
    print(year)
  }
}

dfx <- df2 %>% 
  mutate(date_time = parse_date_time(date,orders = "Ymd HMS")) %>%
  mutate(date = as.Date(date)) %>%
  #mutate(tmp  = str_replace(tmp, "[+]","")) %>%
  mutate(tmp = str_replace(tmp,",",".")) %>%
  mutate(tmp = as.numeric(tmp)) %>%
  filter(tmp < 999)

write_csv(df2, "weather_DK_2000-2019_isd.csv")

