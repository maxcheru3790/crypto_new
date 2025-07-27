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

# Combine actual and forecasted data
combined_data <- c(all_actuals, all_forecasts)
combined_df <- purrr::map_dfr(combined_data, bind_rows)

# Filter up to year 2030
combined_df_filtered <- combined_df %>%
  filter(Year <= 2030) %>%
  arrange(Main_Category, Year)

# View the combined table
print(combined_df_filtered)


library(ggplot2)

ggplot(combined_df, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 1.5, alpha = 0.9) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(
    title = "Policy Forecasts (Actuals vs. ETS-based Projections)",
    x = "Year",
    y = "Policy Count",
    color = "Data Type"
  ) +
  scale_color_manual(values = c("Actual" = "#1f78b4", "Forecast" = "#e31a1c")) +  # blue vs red
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "#333333"),
    strip.text = element_text(size = 11, face = "bold", color = "#444444"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90"),
    strip.background = element_rect(fill = "#f0f0f0", color = NA)
  )


adf.test(ts_data)
tseries::kpss.test(ts_data)
Box.test(na.omit(residuals(model)), lag = 10, type = "Ljung-Box")

shapiro.test(residuals(model))
acf(ts_data)
pacf(ts_data)
library(trend)
mk.test(ts_data)
library(lmtest)
bptest(model)
library(FinTS)
ArchTest(residuals(model), lags = 12)
plot(residuals(model), main = "Residuals")
abline(h = 0, col = "red")
res <- residuals(model)
bptest(lm(res^2 ~ seq_along(res)))



res <- na.omit(residuals(model))
Box.test(res, lag = 10)
ArchTest(res, lags = 5)
bptest(lm(res^2 ~ seq_along(res)))

lambda <- BoxCox.lambda(ts_data + 1e-3)
model <- ets(ts_data, lambda = lambda)


res <- na.omit(residuals(model))
Box.test(res, lag = min(5, length(res) - 1), type = "Ljung-Box")

library(FinTS)
ArchTest(res, lags = min(5, length(res) - 1))



diagnose_forecast <- function(ts_data, category, end_year, holdout = 3) {
  # ADF Test
  adf_p <- tryCatch(adf.test(ts_data)$p.value, error = function(e) NA)
  
  # KPSS Test
  kpss_p <- tryCatch(tseries::kpss.test(ts_data)$p.value, error = function(e) NA)
  
  # Mann-Kendall Trend Test
  mk_p <- tryCatch(trend::mk.test(ts_data)$p.value, error = function(e) NA)
  
  # ETS Model
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  res <- na.omit(residuals(model))
  
  # Ljung-Box
  lb_p <- tryCatch(Box.test(res, lag = min(5, length(res) - 1), type = "Ljung-Box")$p.value, error = function(e) NA)
  
  # Shapiro (Normality)
  shap_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA)
  
  # Breusch-Pagan (Homoscedasticity)
  bp_p <- tryCatch(bptest(lm(res^2 ~ seq_along(res)))$p.value, error = function(e) NA)
  
  # ARCH
  arch_p <- tryCatch(FinTS::ArchTest(res, lags = min(5, length(res) - 1))$p.value, error = function(e) NA)
  
  # Accuracy Metrics from train/test split
  train_ts <- window(ts_data, end = end_year - holdout)
  test_ts <- window(ts_data, start = end_year - holdout + 1)
  
  acc_vals <- tryCatch({
    lambda <- BoxCox.lambda(train_ts + 1e-3)
    model_acc <- auto.arima(train_ts + 1e-3, lambda = lambda)
    fcast_acc <- forecast(model_acc, h = length(test_ts))
    acc <- accuracy(fcast_acc, test_ts)
    list(MAE = acc[2, "MAE"], RMSE = acc[2, "RMSE"], MAPE = acc[2, "MAPE"])
  }, error = function(e) list(MAE = NA, RMSE = NA, MAPE = NA))
  
  # Output
  return(data.frame(
    Main_Category = category,
    ADF_p = adf_p,
    KPSS_p = kpss_p,
    MK_p = mk_p,
    LjungBox_p = lb_p,
    Shapiro_p = shap_p,
    BreuschPagan_p = bp_p,
    ARCH_p = arch_p,
    MAE = acc_vals$MAE,
    RMSE = acc_vals$RMSE,
    MAPE = acc_vals$MAPE
  ))
}
# Accuracy Metrics from train/test split
n_total <- length(ts_data)
n_test <- min(3, floor(n_total / 3))  # at most 1/3 for testing

if (n_total > n_test + 1) {
  train_ts <- window(ts_data, end = c(time(ts_data)[n_total - n_test]))
  test_ts <- window(ts_data, start = c(time(ts_data)[n_total - n_test + 1]))
  
  acc_vals <- tryCatch({
    model_acc <- ets(train_ts)
    fcast_acc <- forecast(model_acc, h = length(test_ts))
    acc <- accuracy(fcast_acc, test_ts)
    list(MAE = acc[2, "MAE"], RMSE = acc[2, "RMSE"], MAPE = acc[2, "MAPE"])
  }, error = function(e) list(MAE = NA, RMSE = NA, MAPE = NA))
} else {
  acc_vals <- list(MAE = NA, RMSE = NA, MAPE = NA)
}

diagnostics_table <- purrr::map_dfr(categories, function(cat) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  diagnose_forecast(ts_data, cat, end_year)
})


print(diagnostics_table)



