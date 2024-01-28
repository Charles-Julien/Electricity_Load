### Get meteo data
library(dplyr)

relative_folder_path <- "meteo"
csv_files <- list.files(relative_folder_path, pattern = "\\.csv$", full.names = TRUE)
concatenated_data <- data.frame()

# Loop through each CSV file and concatenate the data
is_first_file <- TRUE
for (file in csv_files) {
  data <- read.csv(file, colClasses = rep("character"))
  data = select(data, c(5,10,12,14,16,18))
  concatenated_data <- bind_rows(concatenated_data, data)
  is_first_file <- FALSE
}

meteo$Date = as.Date(meteo$Date.Time)
meteo$MaxTemp = as.numeric(meteo$Max.Temp...C.)
meteo$MinTemp = as.numeric(meteo$Min.Temp...C.)
meteo$MeanTemp = as.numeric(meteo$Mean.Temp...C.)
meteo$HDD = as.numeric(meteo$Heat.Deg.Days...C.)
meteo$CDD = as.numeric(meteo$Cool.Deg.Days...C.)

# Overwrite HDD and CDD calculations
meteo$HDD = ifelse(meteo$MeanTemp < 10, 10-meteo$MeanTemp, 0)
meteo$CDD = ifelse(meteo$MeanTemp > 10, meteo$MeanTemp-10, 0)

meteo = select(meteo, 7:12)

write.csv(meteo, file = "meteo_data.csv", row.names = FALSE)


df <- read.csv('meteo_data.csv')
df$Date = as.Date(df$Date)
head(df)


### Feature engineering and NA imputation
library(lubridate)

df$Month <- month(df$Date)
df$Year <- year(df$Date)
df$Dow <- wday(df$Date, label = TRUE)

#df[!complete.cases(df),]
#df[2099, c('MaxTemp','MeanTemp','HDD','CDD')] = c(23.3,	13.8,	4.2,	0.0)

# COnsecutive hdd and cdd
library(dplyr)
library(zoo)
df <- df %>%
  mutate(ConsHDD = as.factor(ifelse(lag(HDD) > 0, 1, 0)))

df <- df %>%
  mutate(ConsCDD = as.factor(ifelse(lag(CDD) > 0, 1, 0)))

df <- df %>%
  mutate(HDD_1 = lag(HDD)
         ,HDD_2 = lag(HDD, n=2)
         ,CDD_1 = lag(CDD)
         ,CDD_2 = lag(CDD, n=2))


# get business days
#install.packages("bizdays")
library(bizdays)

all_holidays_vector <- as.Date(c(
  "2011-01-01", "2011-02-21", "2011-04-15", "2011-05-23", "2011-07-01", "2011-09-05", "2011-10-10", "2011-11-11", "2011-12-25", "2011-12-26",
  "2012-01-01", "2012-02-21", "2012-04-15", "2012-05-23", "2012-07-01", "2012-09-05", "2012-10-10", "2012-11-11", "2012-12-25", "2012-12-26",
  "2013-01-01", "2013-02-21", "2013-03-29", "2013-05-20", "2013-07-01", "2013-09-02", "2013-10-14", "2013-11-11", "2013-12-25", "2013-12-26",
  "2014-01-01", "2014-02-21", "2014-04-18", "2014-05-19", "2014-07-01", "2014-09-01", "2014-10-13", "2014-11-11", "2014-12-25", "2014-12-26",
  "2015-01-01", "2015-02-16", "2015-04-03", "2015-05-18", "2015-07-01", "2015-09-07", "2015-10-12", "2015-11-11", "2015-12-25", "2015-12-26",
  "2016-01-01", "2016-02-15", "2016-03-25", "2016-05-23", "2016-07-01", "2016-09-05", "2016-10-10", "2016-11-11", "2016-12-25", "2016-12-26",
  "2017-01-01", "2017-02-20", "2017-04-14", "2017-05-22", "2017-07-01", "2017-09-04", "2017-10-09", "2017-11-11", "2017-12-25", "2017-12-26",
  "2018-01-01", "2018-02-19", "2018-03-30", "2018-05-21", "2018-07-01", "2018-09-03", "2018-10-08", "2018-11-11", "2018-12-25", "2018-12-26",
  "2019-01-01", "2019-02-18", "2019-04-19", "2019-05-20", "2019-07-01", "2019-09-02", "2019-10-14", "2019-11-11", "2019-12-25", "2019-12-26",
  "2020-01-01", "2020-02-17", "2020-04-10", "2020-05-18", "2020-07-01", "2020-09-07", "2020-10-12", "2020-11-11", "2020-12-25", "2020-12-26"
))

canadian_calendar <- create.calendar(
  name = "Canada",
  holidays = all_holidays_vector,
  weekdays=c("saturday", "sunday")
)
df$IsBusinessDay <- as.factor(ifelse(is.bizday(df$Date, canadian_calendar), 1, 0))


saveRDS(df, 'weather.rds')

write.csv(df, "weather.csv", row.names = FALSE)

test = readRDS("weather.rds")




