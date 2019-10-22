# Library----
library(tidyverse)
library(highcharter)
library(zoo)
library(xts)
library(janitor)
library(forecast)
library(viridis)
library(lubridate)

# Read Data----
# First produciton:
ir_rd <- read_csv("time-serie/longt-interest_rate.csv") %>% clean_names()


# Filter & Plots----
  # HeatMap----
hm_plot <- ir_rd %>%
  filter(subject == 'IRLT',
         !country %in% c('Russia', 'Greece', 'Mexico', 'Chile', 'Luxembourg', 'Costa Rica', 'South Africa', 'New Zealand', 'Finland')) %>%
  transmute(dt = as.yearmon(time),
            value, 
            country, 
            year = year(dt),
            quarter = as.yearqtr(paste0(year(dt), '-', lubridate::quarter(dt)))) %>%
  filter(year > 1970) %>% 
  group_by(country, quarter) %>% 
  summarise(value = last(value))

hchart(hm_plot, "heatmap", hcaes(x = quarter, y = country, value = value)) %>% 
  hc_title(text = "Infectious Diseases and Vaccines") %>% 
  hc_legend(layout = "vertical", verticalAlign = "top",
            align = "right", valueDecimals = 0)

  # Filter country an type of IR----
df <- ir_rd %>% 
  filter(subject == 'IRLT', country == 'Austria') %>% 
  transmute(dt = as.yearmon(time), value = value) %>% 
  tail(300)

# df to ts
df_ts <- ts(df$value, start = df$dt[1], frequency = 12)

# STL diagnosis
stl(df_ts, "per") %>% hchart()

# Diff Plot
hchart(diff(df_ts))

# ACF & PACF
acf(diff(df_ts), plot = F) %>% hchart()
pacf(diff(df_ts), plot = F) %>% hchart()

# Forecasts----
# Test - Train split
end_train <- df$dt[(round(nrow(df)*0.8))]
start_test <- end_train+0.1
train <- window(df_ts, end = end_train)
h <- length(df_ts) - length(train)
error_mesures <- c('RMSE', 'MAE', 'MPE', 'MAPE')
.lambda = 0

pred.arima <- forecast(auto.arima(train, seasonal = T, biasadj=F, lambda = .lambda), h = h)
pred.ets <- forecast(ets(train, lambda = .lambda, biasadj=F), h = h)
pred.stl <- stlf(train, method = 'ets', lambda = .lambda, h = h, biasadj=F)
pred.nnar <- forecast(nnetar(train, lambda = .lambda, biasadj=F), h=h)
pred.tbats <- forecast(tbats(train, lambda = .lambda, biasadj=F), h=h)
pred.agg <- (pred.ets[["mean"]] + pred.arima[["mean"]] +
               pred.stl[["mean"]] + pred.nnar[["mean"]] + pred.tbats[["mean"]])/5

inspect.test <- cbind(ETS = accuracy(pred.ets, df_ts)["Test set", error_mesures],
  ARIMA = accuracy(pred.arima, df_ts)["Test set", error_mesures],
  `STL-ETS` = accuracy(pred.stl, df_ts)["Test set", error_mesures],
  NNAR = accuracy(pred.nnar, df_ts)["Test set", error_mesures],
  TBATS = accuracy(pred.tbats, df_ts)["Test set", error_mesures],
  Combination = accuracy(pred.agg, df_ts)["Test set", error_mesures])

df_ts %>% hchart() %>% 
  hc_add_series(pred.arima$mean, name = pred.arima$method) %>% 
  hc_add_series(pred.ets$mean, name = pred.ets$method) %>% 
  hc_add_series(pred.tbats$mean, name = pred.tbats$method) %>% 
  hc_add_series(pred.nnar$mean, name = pred.nnar$method) %>% 
  hc_add_series(pred.stl, name = pred.stl$method) %>% 
  hc_add_series(pred.agg, name = 'Combination')

# CheckResiduals Highcharts
pred.arima %>% checkresiduals()
pred.arima$residuals %>% hchart()
pred.arima$residuals %>% hist(plot = F, breaks = 12) %>% hchart()
pred.arima$residuals %>% pacf(plot = F) %>% hchart()

# Write CSV with essential values for Github storage-----
# Countries with data issues
lazy_countries <- c('Russia', 'Greece', 'Mexico', 'Chile', 'Luxembourg', 'Costa Rica', 'South Africa', 'New Zealand', 'Finland')

prod_data <- ir_rd %>% 
  filter(subject == 'IRLT', 
         !country %in% lazy_countries) %>% 
  transmute(country, time, longt_i = value)

prod_data %>% write_csv('long_term_interest_rates.csv')
