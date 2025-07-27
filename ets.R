# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)
library(tseries)
library(forecast)
library(dplyr)
library(tidyr)


# Aggregate and reshape the data
policy_trends <- crypto_data_with_region %>%
  group_by(Year, Main_Category) %>%
  summarise(policy_count = n(), .groups = "drop")

policy_wide <- policy_trends %>%
  pivot_wider(names_from = Main_Category, values_from = policy_count, values_fill = 0)

# Define forecasting parameters
years_to_forecast <- 5
start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)
categories <- names(policy_wide)[-1]  # exclude Year column

# Containers
all_forecasts <- list()
all_actuals <- list()
diagnostics_list <- list()

# Forecasting loop
for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  
  # ADF Test (Stationarity)
  adf_p <- tryCatch(adf.test(ts_data)$p.value, error = function(e) NA)
  
  # Fit ETS model: Prefer AAN, fallback to ANN
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  # Forecast future values
  fcast <- forecast(model, h = years_to_forecast)
  forecast_years <- (end_year + 1):(end_year + years_to_forecast)
  
  cat_forecast <- data.frame(
    Year = forecast_years,
    Main_Category = cat,
    policy_count = round(as.numeric(fcast$mean)),
    type = "Forecast"
  )
  
  actuals <- data.frame(
    Year = policy_wide$Year,
    Main_Category = cat,
    policy_count = policy_wide[[cat]],
    type = "Actual"
  )
  
  all_forecasts[[cat]] <- cat_forecast
  all_actuals[[cat]] <- actuals
  
  # Residual diagnostics
  res <- residuals(model)
  lb_p <- tryCatch(Box.test(res, lag = 10, type = "Ljung-Box")$p.value, error = function(e) NA)
  shap_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA)
  
  # Accuracy evaluation: Train/test split (last 3 years)
  holdout <- 3
  train_ts <- window(ts_data, end = end_year - holdout)
  test_ts <- window(ts_data, start = end_year - holdout + 1)
  
  acc_vals <- tryCatch({
    lambda <- BoxCox.lambda(train_ts + 1e-3)
    model_acc <- auto.arima(train_ts + 1e-3, lambda = lambda)
    fcast_acc <- forecast(model_acc, h = length(test_ts))
    acc <- accuracy(fcast_acc, test_ts)
    list(MAE = acc[2, "MAE"], RMSE = acc[2, "RMSE"], MAPE = acc[2, "MAPE"])
  }, error = function(e) list(MAE = NA, RMSE = NA, MAPE = NA))
  
  # Store diagnostics
  diagnostics_list[[cat]] <- data.frame(
    Main_Category = cat,
    ADF_p_value = adf_p,
    Ljung_Box_p_value = lb_p,
    Shapiro_p_value = shap_p,
    MAE = acc_vals$MAE,
    RMSE = acc_vals$RMSE,
    MAPE = acc_vals$MAPE
  )
}
combined_df <- purrr::map_dfr(combined_data, bind_rows)
combined_df <- purrr::imap_dfr(combined_data, ~ mutate(.x, Main_Category = .y))
# Assuming you already have combined_df as shown earlier
df_long <- combined_df
str(df_long)
# Should show: Year, Main_Category, policy_count, type


library(ggplot2)

ggplot(combined_df, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(
    title = "Policy Forecasts (Actuals + ETS-based Projections)",
    y = "Policy Count",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )




model_and_compare <- function(data, category, forecast_horizon = 3) {
  # Filter data for the given category
  cat_data <- data %>% filter(Main_Category == category)
  
  # Ensure time series is sorted
  cat_data <- cat_data %>% arrange(Year)
  
  # Create time series object
  ts_data <- ts(cat_data$policy_count, start = min(cat_data$Year), frequency = 1)
  
  # Split into train/test
  n <- length(ts_data)
  train_ts <- window(ts_data, end = time(ts_data)[n - forecast_horizon])
  test_ts <- window(ts_data, start = time(ts_data)[n - forecast_horizon + 1])
  
  # Initialize results list
  result <- list(Main_Category = category)
  
  # ETS model
  ets_model <- tryCatch(ets(train_ts), error = function(e) NULL)
  if (!is.null(ets_model)) {
    ets_forecast <- forecast(ets_model, h = forecast_horizon)
    ets_accuracy <- accuracy(ets_forecast, test_ts)
    
    result$ETS_RMSE <- ets_accuracy["Test set", "RMSE"]
    result$ETS_MAE  <- ets_accuracy["Test set", "MAE"]
    result$ETS_MAPE <- ets_accuracy["Test set", "MAPE"]
  } else {
    result$ETS_RMSE <- NA
    result$ETS_MAE <- NA
    result$ETS_MAPE <- NA
  }
  
  # ARIMA model (fallback)
  arima_model <- auto.arima(train_ts)
  arima_forecast <- forecast(arima_model, h = forecast_horizon)
  arima_accuracy <- accuracy(arima_forecast, test_ts)
  
  result$ARIMA_RMSE <- arima_accuracy["Test set", "RMSE"]
  result$ARIMA_MAE  <- arima_accuracy["Test set", "MAE"]
  result$ARIMA_MAPE <- arima_accuracy["Test set", "MAPE"]
  
  return(result)
}


# Get unique categories
categories <- unique(df_long$Main_Category)

# Run modeling
comparison_results <- lapply(categories, function(cat) {
  model_and_compare(df_long, cat, forecast_horizon = 3)
})

# Convert to dataframe
comparison_df <- bind_rows(comparison_results)

# View
print(comparison_df)

