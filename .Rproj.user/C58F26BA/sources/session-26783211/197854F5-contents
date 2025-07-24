library(tidyverse)
library(forecast)
library(tseries)
library(ggplot2)
library(lubridate)

# Count documents per year
doc_per_year <- crypto_data %>%
  count(Year) %>%
  arrange(Year)

# Convert to time series object
ts_data <- ts(doc_per_year$n, start = min(doc_per_year$Year), frequency = 1)

# Augmented Dickey-Fuller test
adf.test(ts_data)

# If not stationary, difference it
diff_ts <- diff(ts_data)
adf.test(diff_ts)  # Should now be stationary


model <- auto.arima(ts_data, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
summary(model)



forecast_values <- forecast(model, h = 5)

autoplot(forecast_values) +
  labs(
    title = "Forecast of Crypto & Blockchain Policy Documents in Africa",
    subtitle = "Based on ARIMA(0,1,0) model",
    x = "Year",
    y = "Number of Documents"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )



forecast_df <- as.data.frame(forecast_values)
forecast_df$Year <- seq(max(doc_per_year$Year) + 1, by = 1, length.out = 5)

forecast_df %>%
  select(Year, `Point Forecast`, `Lo 80`, `Hi 80`, `Lo 95`, `Hi 95`)




library(tidyr)
library(dplyr)

# Aggregate counts of documents by Year and Main_Category
category_trend <- crypto_data %>%
  count(Year, Main_Category) %>%
  complete(Year, Main_Category, fill = list(n = 0)) %>%  # Fill missing combos with 0
  arrange(Main_Category, Year)

category_matrix <- category_trend %>%
  pivot_wider(names_from = Main_Category, values_from = n) %>%
  arrange(Year)


library(forecast)

# Remove year column and convert to time series matrix
ts_matrix <- ts(category_matrix[-1], start = min(category_matrix$Year), frequency = 1)

# Forecast each category individually
category_forecasts <- lapply(1:ncol(ts_matrix), function(i) {
  fit <- auto.arima(ts_matrix[, i], seasonal = FALSE)
  forecast(fit, h = 5)
})

# Combine into a tidy data frame
forecast_years <- (max(category_matrix$Year) + 1):(max(category_matrix$Year) + 5)
forecast_df <- data.frame(Year = forecast_years)

for (i in seq_along(category_forecasts)) {
  forecast_df[[colnames(ts_matrix)[i]]] <- category_forecasts[[i]]$mean
}


library(tidyr)
library(ggplot2)

forecast_long <- forecast_df %>%
  pivot_longer(-Year, names_to = "Main_Category", values_to = "Predicted_Count")

# Visualize trends
ggplot(forecast_long, aes(x = Year, y = Predicted_Count, color = Main_Category)) +
  geom_line(size = 1.2) +
  labs(title = "Forecasted Dominant Crypto & Blockchain Policy Categories (2026–2030)",
       x = "Year", y = "Predicted Document Count") +
  theme_minimal(base_size = 13)





library(forecast)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1. Count documents by year and main category
category_year_data <- crypto_data %>%
  count(Year, Main_Category) %>%
  pivot_wider(names_from = Main_Category, values_from = n, values_fill = 0)

# 2. Convert to time series matrix
ts_matrix <- ts(category_year_data[,-1], start = min(category_year_data$Year), frequency = 1)

# 3. Forecast using ETS
category_forecasts <- lapply(1:ncol(ts_matrix), function(i) {
  fit <- ets(ts_matrix[, i])
  forecast(fit, h = 5)
})

# 4. Combine into a long dataframe for plotting
years_future <- (max(category_year_data$Year) + 1):(max(category_year_data$Year) + 5)
forecast_long <- do.call(rbind, lapply(1:length(category_forecasts), function(i) {
  data.frame(
    Year = years_future,
    Predicted_Count = as.numeric(category_forecasts[[i]]$mean),
    Main_Category = colnames(ts_matrix)[i]
  )
}))

# 5. Plot
ggplot(forecast_long, aes(x = Year, y = Predicted_Count, color = Main_Category)) +
  geom_line(size = 1.2) +
  labs(title = "Forecast of Policy Document Trends by Category (ETS Model)",
       y = "Predicted Document Count", x = "Year") +
  theme_minimal(base_size = 14)




# Combine historical and forecast data
historical <- crypto_data %>%
  count(Year, Main_Category)

forecast_long$Type <- "Forecast"
historical$Type <- "Historical"
colnames(historical)[3] <- "Predicted_Count"

combined <- bind_rows(historical, forecast_long)

# Calculate percentage share per year
share_df <- combined %>%
  group_by(Year) %>%
  mutate(Share = Predicted_Count / sum(Predicted_Count))

# Plot share over time
ggplot(share_df, aes(x = Year, y = Share, fill = Main_Category)) +
  geom_area(alpha = 0.8) +
  labs(title = "Relative Share of Policy Categories Over Time",
       y = "Proportion of Documents", x = "Year") +
  theme_minimal(base_size = 14)



crypto_data %>%
  count(Year, Main_Category) %>%
  ggplot(aes(x = Year, y = n, color = Main_Category)) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) +
  labs(title = "Smoothed Trends in Policy Documents by Category",
       y = "Number of Documents", x = "Year") +
  theme_minimal(base_size = 14)

