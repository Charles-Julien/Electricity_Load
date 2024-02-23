### Get meteo data


# Blatchord
relative_folder_path <- "../meteo/blatchford"
csv_files <- list.files(relative_folder_path, pattern = "\\.csv$", full.names = TRUE)
concatenated_data <- data.frame()

# Loop and concatonate data
is_first_file <- TRUE
for (file in csv_files) {
  data <- read.csv(file, colClasses = rep("character"))
  data <- data[, c(5, 10, 12, 14, 16, 18, 24, 30)]  # Selecting specific columns
  if (is_first_file) {
    concatenated_data <- data
    is_first_file <- FALSE
  } else {
    concatenated_data <- rbind(concatenated_data, data)  # Binding rows
  }
}

meteo = concatenated_data

meteo$Date = as.Date(meteo$Date.Time)
meteo$MaxTemp = as.numeric(meteo$Max.Temp...C.)
meteo$MinTemp = as.numeric(meteo$Min.Temp...C.)
meteo$MeanTemp = as.numeric(meteo$Mean.Temp...C.)
meteo$Precip = as.numeric(meteo$Total.Precip..mm.)
meteo$MaxGust = as.numeric(meteo$Spd.of.Max.Gust..km.h.)


# Overwrite HDD and CDD calculations
meteo$HDD = ifelse(meteo$MeanTemp < 8, 8-meteo$MeanTemp, 0)
meteo$CDD = ifelse(meteo$MeanTemp > 13, meteo$MeanTemp-13, 0)

meteo <- meteo[, 9:16]

blatchford = meteo



#### Get Namao AWOS data

relative_folder_path <- "../meteo/namao"
csv_files <- list.files(relative_folder_path, pattern = "\\.csv$", full.names = TRUE)
concatenated_data <- data.frame()

# Loop and concat data
is_first_file <- TRUE
for (file in csv_files) {
  data <- read.csv(file, colClasses = rep("character"))
  data <- data[, c(5, 10, 12, 14, 16, 18, 24, 30)]  # Selecting specific columns
  if (is_first_file) {
    concatenated_data <- data
    is_first_file <- FALSE
  } else {
    concatenated_data <- rbind(concatenated_data, data)  # Binding rows
  }
}

meteo = concatenated_data

meteo$Date = as.Date(meteo$Date.Time)
meteo$MaxTemp = as.numeric(meteo$Max.Temp...C.)
meteo$MinTemp = as.numeric(meteo$Min.Temp...C.)
meteo$MeanTemp = as.numeric(meteo$Mean.Temp...C.)
#meteo$HDD = as.numeric(meteo$Heat.Deg.Days...C.)
#meteo$CDD = as.numeric(meteo$Cool.Deg.Days...C.)
meteo$Precip = as.numeric(meteo$Total.Precip..mm.)
meteo$MaxGust = as.numeric(meteo$Spd.of.Max.Gust..km.h.)

# Overwrite HDD and CDD calculations
meteo$HDD = ifelse(meteo$MeanTemp < 8, 8-meteo$MeanTemp, 0)
meteo$CDD = ifelse(meteo$MeanTemp > 13, meteo$MeanTemp-13, 0)

meteo <- meteo[, 9:16]

namao = meteo


### Further data manipulation
# Replace blatchford missing values with namao.
merged_df <- merge(blatchford, namao, by = "Date", all = FALSE, suffixes = c("_bla", "_nam"))

cols_to_replace <- c("MinTemp","MaxTemp","MeanTemp", "Precip", "MaxGust", "HDD", "CDD")
for (col in cols_to_replace) {
  merged_df[[paste0(col, "_bla")]][is.na(merged_df[[paste0(col, "_bla")]])] <- merged_df[[paste0(col, "_nam")]][is.na(merged_df[[paste0(col, "_bla")]])]
}

print(colSums(is.na(merged_df)))

# Remaining missing values will be replaced by previous value 
cols_to_replace <- c("MaxTemp_bla", "MinTemp_bla", "MeanTemp_bla", "Precip_bla", "MaxGust_bla", "HDD_bla", "CDD_bla")

for (col in cols_to_replace) {
  for (i in 2:length(merged_df[[col]])) {
    if (is.na(merged_df[[col]][i])) {
      merged_df[[col]][i] <- merged_df[[col]][i - 1]
    }
  }
}

print(colSums(is.na(merged_df)))

df = merged_df[,1:8]

# Date features
df$Month <- month.name[as.integer(format(df$Date, "%m"))]
df$Year <- as.integer(format(df$Date, "%Y"))
df$DOW <- weekdays(df$Date)

# Lag of temperature
df$HDD_1 <- c(15, head(df$HDD_bla, -1))
df$HDD_2 <- c(15, 15, head(df$HDD_bla, -2))
df$CDD_1 <- c(0, head(df$CDD_bla, -1))
df$CDD_2 <- c(0, 0, head(df$CDD_bla, -2))

# Create business day indicator
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
df$IsBusinessDay <- ifelse(df$Date %in% all_holidays_vector | df$DOW %in% c("Saturday", "Sunday"), 0, 1)

df$Holiday = ifelse(df$Date %in% all_holidays_vector, 1, 0)

# How much colder than 5 degrees did it get?
df$MaxHDD = ifelse(df$MinTemp_bla < 5, 5-df$MinTemp_bla, 0)
# How much hotter than 15 degrees did it get?
df$MaxCDD = ifelse(df$MaxTemp_bla > 15, df$MaxTemp_bla-15, 0)

# Remove _bla
names(df) <- gsub("_bla", "", names(df))

# Convert to ordered factors
df$Month <- factor(df$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))
df$DOW <- factor(df$DOW, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create weekend variable
df$wknd = ifelse(df$DOW %in% c("Saturday", "Sunday"), 1, 0)

saveRDS(df, 'weather.rds')
# 
write.csv(df, "weather.csv", row.names = FALSE)

#test = readRDS("weather.rds")




