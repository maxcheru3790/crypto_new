library(dplyr)
library(ggplot2)
library(forecast)      # For time series forecasting
library(tidyr)         # For reshaping



policy_trends <- crypto_data_with_region %>%
  group_by(Year, Main_Category) %>%
  summarise(policy_count = n()) %>%
  ungroup()
print(policy_trends)
View(policy_trends)

policy_wide <- policy_trends %>%
  pivot_wider(names_from = Main_Category, values_from = policy_count, values_fill = 0)
View(policy_wide)



years_to_predict <- 5
start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)

categories <- names(policy_wide)[-1]  # drop Year column
all_forecasts <- list()

for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  model <- auto.arima(ts_data)
  forecasted <- forecast(model, h = years_to_predict)
  
  future_years <- (end_year + 1):(end_year + years_to_predict)
  
  all_forecasts[[cat]] <- data.frame(
    Year = future_years,
    Main_Category = cat,
    Predicted_Count = round(forecasted$mean)
  )
}

final_forecasts <- bind_rows(all_forecasts)





ggplot(final_forecasts, aes(x = Year, y = Predicted_Count, fill = Main_Category)) +
  geom_col(position = "dodge") +
  labs(title = "Forecasted Policy Counts by Category",
       x = "Year", y = "Predicted Policy Count") +
  theme_minimal()



write.csv(final_forecasts, "predicted_policies_by_category.csv", row.names = FALSE)



library(ggplot2)

ggplot(final_forecasts, aes(x = Year, y = Predicted_Count, fill = Main_Category)) +
  geom_col(position = "dodge") +
  labs(title = "Forecasted Policies (2026–2030)", x = "Year", y = "Predicted Policy Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




model <- ets(ts_data)
forecasted <- forecast(model, h = years_to_predict)


library(tseries)

# Example: for one category
ts_data <- ts(policy_wide$`Regulation, Law & Compliance`, start = 2015)

adf.test(ts_data)



model <- auto.arima(ts_data)
checkresiduals(model)




library(forecast)
model_ets <- ets(ts_data)
checkresiduals(model_ets)
forecast(model_ets, h = 5)


model_ets_trend <- ets(ts_data, model = "AAN")
forecast(model_ets_trend, h = 5)


autoplot(forecast(model_ets_trend, h = 5)) +
  autolayer(ts_data, series = "Actual") +
  labs(title = "ETS(AAN) Forecast: Regulation, Law & Compliance")




start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)
years_to_forecast <- 5

all_forecasts <- list()
all_actuals <- list()

categories <- names(policy_wide)[-1]  # drop Year column

for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  
  # Try ETS(AAN), fallback to ETS(ANN)
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  fcast <- forecast(model, h = years_to_forecast)
  forecast_years <- (end_year + 1):(end_year + years_to_forecast)
  
  # Save forecast
  cat_forecast <- data.frame(
    Year = forecast_years,
    Main_Category = cat,
    policy_count = as.numeric(round(fcast$mean)),
    type = "Forecast"
  )
  
  # Save historical actuals
  actuals <- data.frame(
    Year = policy_wide$Year,
    Main_Category = cat,
    policy_count = policy_wide[[cat]],
    type = "Actual"
  )
  
  all_forecasts[[cat]] <- cat_forecast
  all_actuals[[cat]] <- actuals
}

combined_data <- bind_rows(all_forecasts, all_actuals)


ggplot(combined_data, aes(x = Year, y = policy_count, color = type)) +
  geom_line(size = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(title = "Policy Forecasts (Actuals + ETS-based Projections)",
       y = "Policy Count", x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"))


ggplot(combined_data, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(title = "Policy Forecasts (Actuals + ETS-based Projections)",
       y = "Policy Count", x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"))


str(combined_data)



library(dplyr)

combined_df <- bind_rows(combined_data, .id = "Main_Category")
library(dplyr)

combined_df <- bind_rows(combined_data, .id = "Main_Category")

library(ggplot2)

ggplot(combined_df, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(title = "Policy Forecasts (Actuals + ETS-based Projections)",
       y = "Policy Count", x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"))


ggplot(combined_df, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(title = "Policy Forecasts (Actuals + ETS-based Projections)",
       y = "Policy Count", x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"))



# Create full grid
all_years <- 2015:2030
all_categories <- unique(combined_df$Main_Category)

full_grid <- expand.grid(Year = all_years, Main_Category = all_categories)

# Join with actual data
full_combined <- merge(full_grid, combined_df, all.x = TRUE)

# Replace NAs in policy_count and type
full_combined$policy_count[is.na(full_combined$policy_count)] <- 0
full_combined$type[is.na(full_combined$type)] <- ifelse(full_combined$Year[is.na(full_combined$type)] <= 2025, "Actual", "Forecast")


table(combined_df$Main_Category)


duplicated_rows <- combined_df[duplicated(combined_df[, c("Year", "Main_Category", "type")]), ]
nrow(duplicated_rows)
table(combined_df$Main_Category, combined_df$type)




library(ggplot2)

ggplot(combined_df, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(title = "Policy Forecasts (Actuals + ETS-based Projections)",
       y = "Policy Count", x = "Year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 10, face = "bold"))



library(dplyr)
library(ggplot2)
library(forecast)
library(tidyr)
library(purrr)

generate_policy_forecast_plot <- function(data, category_col = "Main_Category", year_col = "Year",
                                          value_col = "policy_count", forecast_horizon = 5) {
  # Ensure symbols for tidy eval
  cat_sym <- sym(category_col)
  year_sym <- sym(year_col)
  val_sym <- sym(value_col)
  
  # 1. Forecast for each category
  forecasted_list <- data %>%
    group_by(!!cat_sym) %>%
    group_split() %>%
    map(function(df_cat) {
      df_cat <- df_cat %>% arrange(!!year_sym)
      ts_data <- ts(df_cat[[value_col]], start = min(df_cat[[year_col]]), frequency = 1)
      fit <- ets(ts_data)
      forecasted <- forecast(fit, h = forecast_horizon)
      years_forecast <- max(df_cat[[year_col]]) + seq_len(forecast_horizon)
      
      tibble(
        !!year_col := years_forecast,
        !!cat_sym := unique(df_cat[[category_col]])[1],
        !!value_col := as.numeric(forecasted$mean),
        type = "Forecast"
      )
    })
  
  forecast_df <- bind_rows(forecasted_list)
  
  # 2. Tag actuals
  actual_df <- data %>%
    mutate(type = "Actual")
  
  # 3. Combine both
  combined_df <- bind_rows(actual_df, forecast_df)
  
  # 4. Plot
  p <- ggplot(combined_df, aes(x = !!year_sym, y = !!val_sym, color = type, group = type)) +
    geom_line(linewidth = 1) +
    facet_wrap(as.formula(paste("~", category_col)), scales = "free_y") +
    labs(title = "Policy Forecasts (Actuals + ETS-based Projections)",
         y = "Policy Count", x = "Year") +
    theme_minimal() +
    theme(legend.position = "bottom",
          strip.text = element_text(size = 10, face = "bold"))
  
  return(p)
}




# Load required libraries
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)  # for adf.test

# Set forecast horizon
years_to_forecast <- 5

# Prepare year range
start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)

# Get policy categories (excluding the Year column)
categories <- setdiff(names(policy_wide), "Year")

# Initialize storage lists
all_forecasts <- list()
all_actuals <- list()

# Loop through each category
for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  
  # Stationarity test
  adf_result <- tryCatch(adf.test(ts_data), error = function(e) NA)
  cat("ADF test for", cat, ":", ifelse(!is.na(adf_result), adf_result$p.value, "failed"), "\n")
  
  # Fit ETS model (prefer AAN, fallback to ANN)
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  # Residual diagnostic
  res <- residuals(model)
  if (length(res) >= 10) {
    lb_test <- Box.test(res, lag = 10, type = "Ljung-Box")
    cat("Ljung-Box p-value for", cat, ":", lb_test$p.value, "\n")
  } else {
    cat("Ljung-Box skipped for", cat, "(too few residuals)\n")
  }
  
  # Forecast future values
  fcast <- forecast(model, h = years_to_forecast)
  forecast_years <- (end_year + 1):(end_year + years_to_forecast)
  
  # Create forecast dataframe
  cat_forecast <- data.frame(
    Year = forecast_years,
    Main_Category = rep(cat, years_to_forecast),
    policy_count = round(as.numeric(fcast$mean)),
    type = "Forecast"
  )
  
  # Create actuals dataframe
  actuals <- data.frame(
    Year = policy_wide$Year,
    Main_Category = rep(cat, length(ts_data)),
    policy_count = policy_wide[[cat]],
    type = "Actual"
  )
  
  all_forecasts[[cat]] <- cat_forecast
  all_actuals[[cat]] <- actuals
}

# Combine actual and forecast data
combined_df <- bind_rows(all_forecasts, all_actuals)

# Sanity check column names
required_cols <- c("Year", "Main_Category", "policy_count", "type")
stopifnot(all(required_cols %in% names(combined_df)))

# Full year-category grid (2015–2030)
full_years <- 2015:2030
all_categories <- unique(combined_df$Main_Category)
full_grid <- expand.grid(Year = full_years, Main_Category = all_categories)

# Merge and complete missing values
full_combined <- full_grid %>%
  left_join(combined_df, by = c("Year", "Main_Category")) %>%
  mutate(
    policy_count = replace_na(policy_count, 0),
    type = case_when(
      is.na(type) & Year <= end_year ~ "Actual",
      is.na(type) & Year > end_year ~ "Forecast",
      TRUE ~ type
    )
  )

# Final Plot
ggplot(full_combined, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(
    title = "Policy Forecasts (Actuals + ETS-based Projections)",
    y = "Policy Count", x = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )





# Load required libraries
library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tseries)  # for adf.test
library(readr)    # for writing CSV

# Forecast settings
years_to_forecast <- 5
start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)
categories <- names(policy_wide)[-1]

# Initialize storage
all_forecasts <- list()
all_actuals <- list()
diagnostics <- list()

# Loop through each category
for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  
  # 1. ADF Test
  adf_p <- tryCatch(adf.test(ts_data)$p.value, error = function(e) NA)
  
  # 2. Fit ETS model (prefer AAN, fallback to ANN)
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  # 3. Residual diagnostics
  res <- residuals(model)
  
  lb_p <- tryCatch({
    if (length(res) >= 10) {
      Box.test(res, lag = 10, type = "Ljung-Box")$p.value
    } else {
      NA
    }
  }, error = function(e) NA)
  
  # 4. Shapiro-Wilk test for normality of residuals
  shapiro_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA)
  
  # 5. Accuracy metrics
  acc <- tryCatch(accuracy(model), error = function(e) matrix(NA, 1, 8))
  mae <- acc[1, "MAE"]
  rmse <- acc[1, "RMSE"]
  mape <- acc[1, "MAPE"]
  
  # Store diagnostics
  diagnostics[[cat]] <- data.frame(
    Main_Category = cat,
    ADF_p_value = adf_p,
    Ljung_Box_p_value = lb_p,
    Shapiro_p_value = shapiro_p,
    MAE = mae,
    RMSE = rmse,
    MAPE = mape
  )
  
  # Forecast
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
}

# Combine actuals + forecasts
combined_df <- bind_rows(all_forecasts, all_actuals)

# Sanity check
stopifnot(all(c("Year", "Main_Category", "policy_count", "type") %in% names(combined_df)))

# Export diagnostics table
diagnostics_df <- bind_rows(diagnostics)
write_csv(diagnostics_df, "diagnostics_summary.csv")

# Optional: print it
print(diagnostics_df)





# Load required packages
library(forecast)
library(tseries)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Set forecast horizon
years_to_forecast <- 5

# Prepare year range
start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)

# Get categories (excluding Year column)
categories <- names(policy_wide)[-1]

# Initialize storage
all_forecasts <- list()
all_actuals <- list()
all_diagnostics <- list()

for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  
  # Automatically choose Box-Cox lambda
  lambda <- BoxCox.lambda(ts_data, lower = 0)
  
  # Fit ARIMA with transformation and differencing
  model <- auto.arima(ts_data, lambda = lambda, stepwise = FALSE, approximation = FALSE)
  
  # Forecast
  fcast <- forecast(model, h = years_to_forecast)
  forecast_years <- (end_year + 1):(end_year + years_to_forecast)
  
  # Forecast DataFrame
  cat_forecast <- data.frame(
    Year = forecast_years,
    Main_Category = cat,
    policy_count = round(as.numeric(fcast$mean)),
    type = "Forecast"
  )
  
  # Actuals
  actuals <- data.frame(
    Year = policy_wide$Year,
    Main_Category = cat,
    policy_count = as.numeric(ts_data),
    type = "Actual"
  )
  
  # Residuals for diagnostics
  resids <- residuals(model)
  
  # ADF Test
  adf_p <- tryCatch({
    adf.test(ts_data)$p.value
  }, error = function(e) NA)
  
  # Ljung-Box (lag = 3 for short series)
  lb_p <- tryCatch({
    Box.test(resids, lag = 3, type = "Ljung-Box")$p.value
  }, error = function(e) NA)
  
  # Shapiro-Wilk
  shapiro_p <- tryCatch({
    shapiro.test(resids)$p.value
  }, error = function(e) NA)
  
  # Accuracy metrics (train only)
  acc <- tryCatch({
    acc_vals <- accuracy(model)
    list(MAE = acc_vals["Test set", "MAE"],
         RMSE = acc_vals["Test set", "RMSE"],
         MAPE = acc_vals["Test set", "MAPE"])
  }, error = function(e) {
    list(MAE = NA, RMSE = NA, MAPE = NA)
  })
  
  # Collect diagnostics
  diag_df <- data.frame(
    Main_Category = cat,
    ADF_p_value = adf_p,
    Ljung_Box_p_value = lb_p,
    Shapiro_p_value = shapiro_p,
    MAE = acc$MAE,
    RMSE = acc$RMSE,
    MAPE = acc$MAPE
  )
  
  all_diagnostics[[cat]] <- diag_df
  all_forecasts[[cat]] <- cat_forecast
  all_actuals[[cat]] <- actuals
}

# Combine all data
combined_df <- bind_rows(all_forecasts, all_actuals)

# Create full grid for complete plots
full_years <- seq(min(combined_df$Year), max(combined_df$Year))
full_grid <- expand.grid(Year = full_years, Main_Category = unique(combined_df$Main_Category))

# Merge and fill missing
full_combined <- left_join(full_grid, combined_df, by = c("Year", "Main_Category")) %>%
  mutate(
    policy_count = replace_na(policy_count, 0),
    type = case_when(
      is.na(type) & Year <= end_year ~ "Actual",
      is.na(type) & Year > end_year ~ "Forecast",
      TRUE ~ type
    )
  )

# Plot
ggplot(full_combined, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ Main_Category, scales = "free_y") +
  labs(
    title = "Policy Forecasts by Category (ARIMA with Diagnostics)",
    y = "Policy Count", x = "Year"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )

# Final diagnostics table
diagnostics_df <- bind_rows(all_diagnostics)
print(diagnostics_df)





holdout_years <- 3
train_length <- length(ts_data) - holdout_years
train_ts <- window(ts_data, end = start_year + train_length - 1)
test_ts  <- window(ts_data, start = start_year + train_length)
model <- auto.arima(train_ts, lambda = lambda)
fcast <- forecast(model, h = holdout_years)

model <- auto.arima(train_ts, lambda = lambda)
fcast <- forecast(model, h = holdout_years)

acc_vals <- accuracy(fcast, test_ts)
print(acc_vals)







# Load required libraries
library(forecast)
library(tseries)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Metrics)

# Assume `policy_wide` is your wide-format data
years_to_forecast <- 3
start_year <- min(policy_wide$Year)
end_year <- max(policy_wide$Year)
categories <- names(policy_wide)[-1]

# Initialize result containers
all_forecasts <- list()
all_actuals <- list()
diagnostics_list <- list()

# Loop through each category
for (cat in categories) {
  ts_data <- ts(policy_wide[[cat]], start = start_year, frequency = 1)
  
  # Perform ADF test for stationarity
  adf_p <- tryCatch(adf.test(ts_data)$p.value, error = function(e) NA)
  
  # Fit ETS (AAN preferred, fallback to ANN)
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  # Residual diagnostics
  res <- residuals(model)
  lb_p <- tryCatch(Box.test(res, lag = 10, type = "Ljung-Box")$p.value, error = function(e) NA)
  shap_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA)
  
  # Split into training/test for accuracy (last 3 years as test)
  train_ts <- window(ts_data, end = end_year - years_to_forecast)
  test_ts <- window(ts_data, start = end_year - years_to_forecast + 1)
  
  # Accuracy evaluation using Box-Cox transformation
  acc_vals <- tryCatch({
    train_ts_bc <- train_ts + 1e-3  # ensure positivity
    lambda <- BoxCox.lambda(train_ts_bc, method = "loglik", lower = 0)
    model_acc <- auto.arima(train_ts_bc, lambda = lambda)
    fcast_acc <- forecast(model_acc, h = length(test_ts))
    acc <- accuracy(fcast_acc, test_ts)
    list(MAE = acc[2, "MAE"], RMSE = acc[2, "RMSE"], MAPE = acc[2, "MAPE"])
  }, error = function(e) list(MAE = NA, RMSE = NA, MAPE = NA))
  
  # Forecast future
  fcast <- forecast(model, h = years_to_forecast)
  forecast_years <- (end_year + 1):(end_year + years_to_forecast)
  cat_forecast <- data.frame(
    Year = forecast_years,
    Main_Category = cat,
    policy_count = round(as.numeric(fcast$mean)),
    type = "Forecast"
  )
  
  # Actuals
  actuals <- data.frame(
    Year = policy_wide$Year,
    Main_Category = cat,
    policy_count = policy_wide[[cat]],
    type = "Actual"
  )
  
  all_forecasts[[cat]] <- cat_forecast
  all_actuals[[cat]] <- actuals
  
  # Collect diagnostics
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

# Combine all actuals and forecasts
combined_df <- bind_rows(all_actuals, all_forecasts)

# Ensure all categories and years are present
full_years <- seq(min(combined_df$Year), max(combined_df$Year))
all_categories <- unique(combined_df$Main_Category)
full_grid <- expand.grid(Year = full_years, Main_Category = all_categories)

full_combined <- full_grid %>%
  left_join(combined_df, by = c("Year", "Main_Category")) %>%
  mutate(
    policy_count = replace_na(policy_count, 0),
    type = case_when(
      is.na(type) & Year <= end_year ~ "Actual",
      is.na(type) & Year > end_year ~ "Forecast",
      TRUE ~ type
    )
  )

# Combine diagnostics
diagnostics_df <- bind_rows(diagnostics_list)
print(diagnostics_df)

# Plot
ggplot(full_combined, aes(x = Year, y = policy_count, color = type, group = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Main_Category, scales = "free_y") +
  theme_minimal() +
  labs(title = "Forecasts by Policy Category", y = "Policy Count")




# Load required libraries
library(forecast)
library(tseries)
library(dplyr)
library(tidyr)
library(ggplot2)
library(Metrics)

# Parameters
years_to_forecast <- 3
start_year <- min(policy_wide$Year, na.rm = TRUE)
end_year <- max(policy_wide$Year, na.rm = TRUE)
categories <- names(policy_wide)[-1]  # exclude Year column

# Initialize containers
all_forecasts <- list()
all_actuals <- list()
diagnostics_list <- list()

# Loop through each category
for (cat in categories) {
  ts_values <- policy_wide[[cat]]
  
  # Skip if all values are NA or 0
  if (all(is.na(ts_values)) || sum(ts_values, na.rm = TRUE) == 0) next
  
  ts_data <- ts(ts_values, start = start_year, frequency = 1)
  
  # ADF test
  adf_p <- tryCatch(adf.test(ts_data)$p.value, error = function(e) NA)
  
  # ETS model (AAN preferred)
  model <- tryCatch({
    ets(ts_data, model = "AAN")
  }, error = function(e) {
    ets(ts_data, model = "ANN")
  })
  
  # Residual diagnostics
  res <- residuals(model)
  lb_p <- tryCatch(Box.test(res, lag = 10, type = "Ljung-Box")$p.value, error = function(e) NA)
  shap_p <- tryCatch(shapiro.test(res)$p.value, error = function(e) NA)
  
  # Train/test split
  train_ts <- window(ts_data, end = end_year - years_to_forecast)
  test_ts <- window(ts_data, start = end_year - years_to_forecast + 1)
  
  # Accuracy metrics
  acc_vals <- tryCatch({
    train_ts_bc <- train_ts + 1e-3
    lambda <- BoxCox.lambda(train_ts_bc, method = "loglik", lower = 0)
    model_acc <- auto.arima(train_ts_bc, lambda = lambda)
    fcast_acc <- forecast(model_acc, h = length(test_ts))
    acc <- accuracy(fcast_acc, test_ts)
    list(MAE = acc[2, "MAE"], RMSE = acc[2, "RMSE"], MAPE = acc[2, "MAPE"])
  }, error = function(e) list(MAE = NA, RMSE = NA, MAPE = NA))
  
  # Forecast future
  fcast <- forecast(model, h = years_to_forecast)
  forecast_years <- (end_year + 1):(end_year + years_to_forecast)
  cat_forecast <- data.frame(
    Year = forecast_years,
    Main_Category = cat,
    policy_count = round(as.numeric(fcast$mean)),
    type = "Forecast"
  )
  
  # Actuals
  actuals <- data.frame(
    Year = policy_wide$Year,
    Main_Category = cat,
    policy_count = ts_values,
    type = "Actual"
  )
  
  all_forecasts[[cat]] <- cat_forecast
  all_actuals[[cat]] <- actuals
  
  # Diagnostics
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

# Combine and check
combined_df <- bind_rows(all_actuals, all_forecasts)

# Check types and values
combined_df$Year <- as.numeric(combined_df$Year)
combined_df$Main_Category <- as.character(combined_df$Main_Category)

# Ensure complete data across categories and years
full_years <- seq(min(combined_df$Year, na.rm = TRUE), max(combined_df$Year, na.rm = TRUE))
all_categories <- unique(combined_df$Main_Category)

full_grid <- expand.grid(Year = full_years, Main_Category = all_categories)

full_combined <- full_grid %>%
  left_join(combined_df, by = c("Year", "Main_Category")) %>%
  mutate(
    policy_count = replace_na(policy_count, 0),
    type = case_when(
      is.na(type) & Year <= end_year ~ "Actual",
      is.na(type) & Year > end_year ~ "Forecast",
      TRUE ~ type
    )
  )

# View diagnostics
diagnostics_df <- bind_rows(diagnostics_list)
print(diagnostics_df)

# Plot
ggplot(full_combined, aes(x = Year, y = policy_count, color = type)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Main_Category, scales = "free_y") +
  theme_minimal() +
  labs(title = "Forecasts by Policy Category", y = "Policy Count")

# Save result
write.csv(full_combined, "final_clean_crypto_data.csv", row.names = FALSE)
