Sys.setlocale("LC_TIME", "C")
library(timeSeries)
library(forecast)

# Load Data
load("aeso.RData")

# Load Covariates
weather = readRDS("weather.rds")

# Imputation of NA
rows_with_missing_values <- aeso[is.na(aeso[['Edmonton']]), ]
print(rows_with_missing_values)
aeso <- na.omit(aeso)

### Get peak hourly load for day
aeso$DT_MST <- as.POSIXct(aeso$DT_MST, format = "%Y-%m-%d %H:%M:%S")
aeso$Date <- as.Date(aeso$DT_MST)

max <- aggregate(aeso$Edmonton, by = list(Date = aeso$Date), FUN = max, na.rm = TRUE)
colnames(max) <- c("Date", "Load")

### Creating timeSeries
ts = timeSeries(max$Load,
                max$Date, format = "%Y-%m-%d")

df <- merge(max, weather, by = "Date", all = FALSE)
head(df, n =200)


# Demand versus HDD (Accounting for business day)
plot(df$HDD, df$Load, 
     xlab = "HDD", ylab = "Demand MW",
     main = "Scatterplot of Demand versus HDD (BusinessDay)",
     col = ifelse(df$IsBusinessDay == 1, "red", "blue"))
legend("bottomright", legend = c("IsBusinessDay = 1", "IsBusinessDay = 0"), 
       col = c("red", "blue"), pch = 1)


# Demand versus CDD (Accounting for business day)
plot(df$CDD, df$Load, 
     xlab = "CDD", ylab = "Demand MW",
     main = "Scatterplot of Demand versus CDD (BusinessDay)",
     col = ifelse(df$IsBusinessDay == 1,"red","blue"))
legend("bottomright", legend = c("IsBusinessDay = 1", "IsBusinessDay = 0"), 
       col = c("red","blue"), pch = 1)


# Benchmark methods

winter_indices <- which(months(df$Date) %in% c("December", "January", "February"))
spring_indices <- which(months(df$Date) %in% c("March", "April", "May"))
summer_indices <- which(months(df$Date) %in% c("June", "July", "August"))
fall_indices <- which(months(df$Date) %in% c("September", "October", "November"))

all_season = list('winter' = winter_indices,
                  'spring' = spring_indices,
                  'summer' = summer_indices,
                  'fall' = fall_indices)


train_indices = which(df$Year %in% (2011:2015))
val_indices = which(df$Year %in% (2016:2017))
test_indices = which(df$Year %in% (2018:2019))

my_func <- function(split_indices, season_indices, fitted, x) {
  
  forecast = fitted[intersect(split_indices, season_indices)]
  observed = x[intersect(split_indices, season_indices)]
  
  MAPE = mean(abs((forecast - observed)/observed))*100
  MAE = mean(abs(forecast - observed))
  RMSE = sqrt(mean((forecast - observed)^2))
  bias = mean(forecast - observed)
  p_bias = mean((forecast - observed)/observed)*100
  
  metrics = c(MAPE, MAE, RMSE, bias, p_bias)
  
  return(metrics)
}

perf_func <- function(model_name, split_indices, fitted, x, df) {
  
  for (i in seq_along(all_season)) {
    out = my_func(split_indices, unlist(all_season[i]),fitted,x)
    out = c(model_name ,names(all_season)[i], out)
    df = rbind(df, out)
      }
  
  colnames(df) <- c('model_name','season','MAPE','MAE','RMSE','bias','p_bias')
  
  return(df)
}

# Create dataframe to store performance
performance <- data.frame()

# Naive
ts_daily <- ts(df$Load, start = c(2011,1), frequency = 365)
naive <- naive(ts_daily, h=1)

performance = perf_func('naive', val_indices, naive$fitted, naive$x, performance)

# Snaive Weekly
ts_weekly <- ts(df$Load, frequency = 7)
naiveS_w <- snaive(ts_weekly, h = 1)

performance = perf_func('snaive_weekly' ,val_indices, naiveS_w$fitted, naive$x, performance)

# Snaive Yearly
naiveS_y <- snaive(ts_daily, h = 1)
performance = perf_func('snaive_yearly' ,val_indices, naiveS_y$fitted, naiveS_y$x, performance)


# Rolling average
roll <- zoo::rollmean(ts_daily, 3, align="right")
# Preserve indexes
rollavg <- c(c(NA,NA,NA), roll)

performance = perf_func('roll_avg' ,val_indices, rollavg, ts_daily, performance)


# To get a better view of the data
head(performance)
reshape(performance[,c(1,2,3)], idvar = "model_name", timevar = "season", direction = "wide")


# Simple exponential smoothing
in.sample = ts_daily[train_indices]
out.sample = ts_daily[val_indices]

# Initialize
observed_values = in.sample
ses_model <- ses(in.sample, h=1)
predictions = c(forecast(ses_model, h = 1)$mean)

for (i in 1:(length(out.sample)-1)) {
  
  observed_values = c(observed_values, out.sample[i])
  ses_model <- ses(y = observed_values, h=1)
  predictions = c(predictions,forecast(ses_model, h = 1)$mean)
  
}

# Keep indices integrity
predictions <- c(rep(NA, length(in.sample)), predictions)

performance = perf_func('ses', val_indices, predictions, ts_daily, performance)

