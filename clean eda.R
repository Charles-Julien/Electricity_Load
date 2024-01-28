load("aeso.RData")
Sys.setlocale("LC_TIME", "C")
library(timeSeries)

### Dealing with NA's
rows_with_missing_values <- aeso[is.na(aeso[['Edmonton']]), ]
print(rows_with_missing_values)
# This link shows daylight savings dates in Canada, 
# they coincide with the missing values.
# 'https://www.timetemperature.com/canada/daylight_saving_time_canada.shtml'
# The rest of the data does not have missing values
aeso <- na.omit(aeso)


### Get daily max value
aeso$DT_MST <- as.POSIXct(aeso$DT_MST, format = "%Y-%m-%d %H:%M:%S")
aeso$Date <- as.Date(aeso$DT_MST)
max <- aggregate(Edmonton ~ Date, data = aeso, FUN = max, na.rm = TRUE)

### Creating timeSeries
ts = timeSeries(max$Edmonton,
                max$Date, format = "%Y-%m-%d")

### Plots

# Whole dataset
plot(ts, ylab="Edmonton hourly demand (in MW)", at="pretty", main = 'Max Daily Demand Edmonton')

# Year 2015
plot(window(ts, start=timeDate("2015-01-01", format="%Y-%m-%d"),
            end=timeDate("2015-12-31", format="%Y-%m-%d")),
     ylab="Edmonton Max daily demand 2015",
     xlab="Year 2015", at="pretty")

# First week of 2015
plot(window(ts, start=timeDate("2015-01-04", format="%Y-%m-%d"),
            end=timeDate("2015-01-10", format="%Y-%m-%d")),
     ylab="Edmonton max daily demand (in MW)",
     xlab="Sunday to Saturday",
     at="pretty")

# Show two different weeks in January 2015
week1 <- series(window(ts,
                       start=timeDate("2015-01-04", format="%Y-%m-%d"),
                       end=timeDate("2015-01-10", format="%Y-%m-%d")))
week2 <- series(window(ts,
                       start=timeDate("2015-01-11", format="%Y-%m-%d"),
                       end=timeDate("2015-01-17", format="%Y-%m-%d")))
plot(week1, axes=F,
     lty=1, type="l", ylim=c(min(week1,week2),max(week1,week2)),
     ylab="Edmonton max daily demand (in MW)", xlab="")
lines(week2, lty=3, lwd=1.8)
axis(1,at =1:7,
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2)
legend("bottomleft", legend=c("Jan 4-10, 2015","Jan 11-17, 2015"), 
       lty=c(1,3))


# Show weekly difference between January and June
week1 <- series(window(ts,
                       start=timeDate("2015-01-04", format="%Y-%m-%d"),
                       end=timeDate("2015-01-10", format="%Y-%m-%d")))
week2 <- series(window(ts,
                       start=timeDate("2015-06-05", format="%Y-%m-%d"),
                       end=timeDate("2015-06-11", format="%Y-%m-%d")))
plot(week1, axes=F,
     lty=1, type="l", ylim=c(min(week1,week2),max(week1,week2)),
     ylab="Edmonton max daily demand (in MW)", xlab="")
lines(week2, lty=3, lwd=1.8)
axis(1,at =1:7,
     labels=c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2)

legend("bottomright", legend=c("Jan 4-10, 2015","Jun 5-11, 2015"), 
       lty=c(1,3))

### Load Covariates
weather = readRDS("weather.rds")
df <- merge(max, weather, by = "Date", all = FALSE)

# Demand versus temperature
plot(df$MeanTemp, df$Edmonton, 
     xlab = "MeanTemp", ylab = "Demand MW",
     main = "Scatterplot of Demand versus Mean Temp")

# Note that T ref was modified to be 10 degrees

# Demand versus HDD (Accounting for ConsHDD)
plot(df$HDD, df$Edmonton, 
     xlab = "HDD", ylab = "Demand MW",
     main = "Scatterplot of Demand versus HDD",
     col = ifelse(df$ConsHDD == 1, "red", "blue"))
legend("bottomright", legend = c("ConsHDD = 1 ", "ConsHDD = 0"), 
       col = c("red", "blue"), pch = 1)

# Demand versus HDD (Accounting for business day)
plot(df$HDD, df$Edmonton, 
     xlab = "HDD", ylab = "Demand MW",
     main = "Scatterplot of Demand versus HDD",
     col = ifelse(df$IsBusinessDay == 1, "red", "blue"))
legend("bottomright", legend = c("IsBusinessDay = 1", "IsBusinessDay = 0"), 
       col = c("red", "blue"), pch = 1)

# Demand versus CDD (Accounting for ConsCDD)
plot(df$CDD, df$Edmonton, 
     xlab = "CDD", ylab = "Demand MW",
     main = "Scatterplot of Demand versus CDD",
     col = ifelse(df$ConsCDD == 1, "blue", "red"))
legend("bottomright", legend = c("ConsCDD = 1 ", "ConsCDD = 0"), 
       col = c("blue","red"), pch = 1)

# Demand versus CDD (Accounting for business day)
plot(df$CDD, df$Edmonton, 
     xlab = "CDD", ylab = "Demand MW",
     main = "Scatterplot of Demand versus CDD",
     col = ifelse(df$IsBusinessDay == 1,"red","blue"))
legend("bottomright", legend = c("IsBusinessDay = 1", "IsBusinessDay = 0"), 
       col = c("red","blue"), pch = 1)

head(df)

