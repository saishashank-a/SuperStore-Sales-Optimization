library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)

# Read the dfset
df <- read.csv("C:/Users/akhil/OneDrive/Desktop/CBIT college files/VI SEM/DSR/sales.csv")

dim(df)
# Summary of the dfset
summary(df)

# Identify missing values
missing_values <- colSums(is.na(df))
print(missing_values[missing_values > 0])

# Imputation for numerical variables (e.g., Sales, Quantity, Profit)
mean_sales <- mean(df$Sales, na.rm = TRUE)
df$Sales[is.na(df$Sales)] <- mean_sales
mean_quantity <- mean(df$Quantity, na.rm = TRUE)
df$Quantity[is.na(df$Quantity)] <- mean_quantity
mean_profit <- mean(df$Profit, na.rm = TRUE)
df$Profit[is.na(df$Profit)] <- mean_profit

# Imputation for categorical variables (e.g., Ship.Mode, Segment, Region)
mode_ship_mode <- names(sort(table(df$Ship.Mode), decreasing = TRUE))[1]
df$Ship.Mode[is.na(df$Ship.Mode)] <- mode_ship_mode
mode_segment <- names(sort(table(df$Segment), decreasing = TRUE))[1]
df$Segment[is.na(df$Segment)] <- mode_segment
mode_region <- names(sort(table(df$Region), decreasing = TRUE))[1]
df$Region[is.na(df$Region)] <- mode_region

# Standardize variable names
names(df) <- tolower(names(df))  # Convert variable names to lowercase

# Replace spaces with underscores
names(df) <- gsub(" ", "_", names(df))

# Convert categorical variables to factors
categorical_vars <- c("ship.mode", "segment", "region", "category", "sub.category")
df[categorical_vars] <- lapply(df[categorical_vars], as.factor)

# Define winsorization function
winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  return(x)
}

# Identify and winsorize outliers for numerical variables
numerical_vars <- c("sales", "quantity", "profit")
for (var in numerical_vars) {
  df[[var]] <- winsorize(df[[var]])
}

# Define z-score normalization function
z_score_normalization <- function(x) {
 (x - mean(x)) / sd(x)
}

# Apply z-score normalization to numerical variables
numerical_vars <- c("sales", "quantity", "profit")
for (var in numerical_vars) {
  df[[var]] <- z_score_normalization(df[[var]])
}

# Profit Margin
df$profit_margin <- df$profit / df$sales

# Convert "order.date" and "ship.date" columns to Date objects
df$order.date <- as.Date(df$order.date)
df$ship.date <- as.Date(df$ship.date)

# Handle missing values by replacing them with the most common date in each column
most_common_order_date <- as.Date(names(sort(table(df$order.date), decreasing = TRUE))[1])
most_common_ship_date <- as.Date(names(sort(table(df$ship.date), decreasing = TRUE))[1])

df$order.date[is.na(df$order.date)] <- most_common_order_date
df$ship.date[is.na(df$ship.date)] <- most_common_ship_date

# Extract day, month, and year components for "order.date"
df$order_day <- day(df$order.date)
df$order_month <- month(df$order.date)
df$order_year <- year(df$order.date)

# Extract day, month, and year components for "ship.date"
df$ship_day <- day(df$ship.date)
df$ship_month <- month(df$ship.date)
df$ship_year <- year(df$ship.date)

# Average Sales per Day (Monthly)
df$monthly_sales <- as.yearmon(df$order.date)
df$avg_sales_per_day <- ave(df$sales, df$monthly_sales, FUN = mean) / days_in_month(df$monthly_sales)
df$order_processing_time <- as.numeric(difftime(df$ship.date, df$order.date, units = "days"))
names(df)

df$interaction_term <- df$quantity * df$discount

# Check for outliers in numeric columns
# Define a function to detect outliers using IQR
detect_outliers <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  outliers <- x[x < lower_bound | x > upper_bound]
  return(outliers)
}

# Identify outliers in numeric columns
outliers_sales <- detect_outliers(df$sales)
outliers_quantity <- detect_outliers(df$quantity)
outliers_discount <- detect_outliers(df$discount)
outliers_profit <- detect_outliers(df$profit)
outliers_profit_margin <- detect_outliers(df$profit_margin)

# Remove outliers from the dfset
df <- df[!df$sales %in% outliers_sales, ]
df <- df[!df$quantity %in% outliers_quantity, ]
df <- df[!df$discount %in% outliers_discount, ]
df <- df[!df$profit %in% outliers_profit, ]
df <- df[!df$profit_margin %in% outliers_profit_margin, ]


# Display unique values of each categorical column
unique_values_ship_mode <- unique(df$ship.mode)
unique_values_segment <- unique(df$segment)
unique_values_region <- unique(df$region)
unique_values_category <- unique(df$category)
unique_values_sub_category <- unique(df$sub.category)

#---------------------------------------------------------------------------

# Replot the density graph
ggplot(df, aes(x = sales)) +
  geom_density() +
  labs(title = "Density Plot of Sales")


# Plot time series of sales with attractive colors
ggplot(df, aes(x = order.date, y = sales, color = "Sales")) +
  geom_line() +
  labs(title = "Time Series of Sales Over Time",
       x = "Order Date",
       y = "Sales") +
  theme_minimal() +
  scale_color_manual(values = c("Sales" = "#FF5733"))
library(plotly)


# Box plot of sales by category with attractive colors
ggplot(df, aes(x = category, y = sales, fill = category)) +
  geom_boxplot() +
  labs(title = "Box Plot of Sales by Category",
       x = "Category",
       y = "Sales") +
  scale_fill_manual(values = c("#33FF57", "#FF5733", "#33A7FF"))


# Stacked bar chart of segment distribution with attractive colors
ggplot(df, aes(x = region, fill = segment)) +
  geom_bar(position = "stack") +
  labs(title = "Stacked Bar Chart of Segment Distribution",
       x = "Region",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#33FF57", "#FF5733", "#33A7FF"))


# Plot time series of average sales per day with attractive colors
ggplot(df, aes(x = order.date, y = avg_sales_per_day, color = "Average Sales per Day")) +
  geom_line() +
  labs(title = "Time Series of Average Sales per Day",
       x = "Order Date",
       y = "Average Sales per Day") +
  theme_minimal() +
  scale_color_manual(values = c("Average Sales per Day" = "#33FF57"))


# Select numerical variables for correlation analysis
numerical_vars <- c("sales", "quantity", "discount", "profit", "profit_margin")
# Encode categorical variables (example using one-hot encoding)
df_encoded <- model.matrix(~ . - 1, data = df[, c("ship.mode", "segment", "region", "category", "sub.category")])
# Combine numerical and encoded categorical variables
df_all <- cbind(df[, numerical_vars], df_encoded)
# Calculate the correlation matrix
correlation_matrix_all <- cor(df_all)
print(correlation_matrix_all)
# Plot heatmap
library(corrplot)
corrplot(correlation_matrix_all, method = "color")


# Segment Analysis
segment_summary <- df %>%
  group_by(segment) %>%
  summarise(avg_sales = mean(sales), avg_profit = mean(profit))

print(segment_summary)

product_summary <- df %>%
  group_by(category, sub.category) %>%
  summarise(total_sales = sum(sales), total_profit = sum(profit)) %>%
  arrange(desc(total_sales))

print(product_summary)

# Perform PCA
pca_result <- prcomp(df[, c("sales", "quantity", "discount", "profit")], scale. = TRUE)

# Summary of PCA
print(summary(pca_result))

# Scree Plot
screeplot(pca_result, type = "line", main = "Scree Plot")

# Biplot
biplot(pca_result, scale = 0)


#--------------------- MODEL DEVELOPMENT --------------------------#

# Load required libraries
library(forecast)

# Assuming 'order.date' is the date column
df$order.date <- as.Date(df$order.date)

# Create a time series object
ts_df <- ts(df$sales, start = c(min(df$order_year), 1), frequency = 12)

# Split df into training and test sets
train_size <- 0.8  # 80% training df, 20% test df
train_index <- 1:round(train_size * length(ts_df))
train_df <- ts_df[train_index]
test_df <- ts_df[-train_index]

# ARIMA Model
arima_model <- auto.arima(train_df)
arima_forecast <- forecast(arima_model, h = length(test_df))
arima_accuracy <- accuracy(arima_forecast, test_df)

# Output ARIMA Model Accuracy
print("ARIMA Model Accuracy:")
print(arima_accuracy)

# Output ARIMA Model Coefficients and Summary Statistics
print(summary(arima_model))

#-----------#
# TBATS Model
tbats_model <- tbats(train_df)
tbats_forecast <- forecast(tbats_model, h = length(test_df))
tbats_accuracy <- accuracy(tbats_forecast, test_df)

print("TBATS Model Accuracy:")
print(tbats_accuracy)
print(summary(tbats_model))

# Check Model Fit and Assess Residuals for ARIMA Model
print("ARIMA Model Residuals Analysis:")
checkresiduals(arima_model)

# Check Model Fit and Assess Residuals for TBATS Model
print("TBATS Model Residuals Analysis:")
checkresiduals(tbats_model)

#-----------#

  # Prepare the data--splitting the region from the data
  df_region <- split(df, df$region)
  
  # Define a function to train ARIMA model and forecast sales for each region
  forecast_sales <- function(region_data) {
    # Prepare time series data
    ts_data <- ts(region_data$sales, frequency = 7)
    
    # Train ARIMA model
    arima_model <- auto.arima(ts_data)
    
    # Forecast sales for the next 30 days
    sales_forecast <- forecast(arima_model, h = 30)
    
    # Return the forecasted sales
    return(sales_forecast)
  }
  
  # Forecast sales for each region
  region_forecasts <- lapply(df_region, forecast_sales)
  
  # Output forecasted sales for each region
  for (i in 1:length(region_forecasts)) {
    region <- names(region_forecasts)[i]
    cat("Region:", region, "\n")
    print(region_forecasts[[i]])
  }
  # Extract forecasted values for each region
  forecast_values <- lapply(region_forecasts, function(f) f$mean)
  
  # Combine forecasted values into a single time series
  combined_forecast <- ts(do.call(cbind, forecast_values), start = start(ts_df), frequency = frequency(ts_df))
  
  # Set up the plotting layout with 2 rows and 2 columns
  par(mfrow = c(2, 2))
  
  # Loop through each region and plot its forecast
  for (i in 1:length(region_forecasts)) {
    region <- names(region_forecasts)[i]
    sales_forecast <- region_forecasts[[i]]
    
    # Plot the forecast for the current region
    plot(sales_forecast, main = paste("Forecast for", region), xlab = "Date", ylab = "Sales")
  }
  
  
  # Fit a TBATS model
  ts_sales <- ts(df$sales, frequency = 7)  # Assuming daily data
  tbats_model <- tbats(ts_sales)
  
  # Forecast with TBATS for the next 30 days
  forecast_tbats <- forecast(tbats_model, h = 30)
  
  # Output TBATS Model Summary
  print(summary(tbats_model))
  
  # Plot the TBATS forecast
  plot(forecast_tbats, main = "TBATS Sales Forecast", xlab = "Days", ylab = "Sales", col = c("blue", "red"))

  
  # Diagnostic plots for ARIMA Model
  print("ARIMA Model Diagnostic Plots:")
  par(mar = c(4, 4, 2, 2))
  tsdiag(arima_model)
  
  # Diagnostic plots for TBATS Model
  print("TBATS Model Diagnostic Plots:")
  plot(tbats_model)
  
  # Compare Model Accuracy Metrics
  # Calculate accuracy metrics for ARIMA Model
  arima_accuracy <- accuracy(arima_forecast, test_df)
  
  # Calculate accuracy metrics for TBATS Model
  tbats_accuracy <- accuracy(tbats_forecast, test_df)
  
  accuracy_comparison <- data.frame(Model = c("ARIMA", "TBATS"),
                                    RMSE = c(arima_accuracy[2], tbats_accuracy[2]),
                                    MAE = c(arima_accuracy[3], tbats_accuracy[3]),
                                    MAPE = c(arima_accuracy[5], tbats_accuracy[5]))
  
  print(accuracy_comparison)
  
  library(ggplot2)
  
  # Create a dataframe for accuracy comparison
  accuracy_comparison <- data.frame(
    Model = c("ARIMA", "TBATS"),
    RMSE = c(arima_accuracy[2], tbats_accuracy[2]),
    MAE = c(arima_accuracy[3], tbats_accuracy[3]),
    MAPE = c(arima_accuracy[5], tbats_accuracy[5])
  )
  
  # Reshape the data for plotting
  accuracy_comparison_long <- tidyr::gather(accuracy_comparison, Metric, Value, -Model)
  
  # Create a colorful visualization
  ggplot(accuracy_comparison_long, aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Model Accuracy Comparison",
         y = "Value") +
    scale_fill_manual(values = c("RMSE" = "blue", "MAE" = "green", "MAPE" = "red")) +
    theme_minimal()
  
   # RMSE Comparison
  ggplot(accuracy_comparison, aes(x = Model, y = RMSE, fill = Model)) +
    geom_bar(stat = "identity") +
    labs(title = "RMSE Comparison",
         y = "RMSE") +
    theme_minimal()
  
  # MAE Comparison
  ggplot(accuracy_comparison, aes(x = Model, y = MAE, fill = Model)) +
    geom_bar(stat = "identity") +
    labs(title = "MAE Comparison",
         y = "MAE") +
    theme_minimal()
  
  # MAPE Comparison
  ggplot(accuracy_comparison, aes(x = Model, y = MAPE, fill = Model)) +
    geom_bar(stat = "identity") +
    labs(title = "MAPE Comparison",
         y = "MAPE") +
    theme_minimal()
  
  # Extract model coefficients, standard errors, and confidence intervals
  arima_coefficients <- coef(arima_model)
  arima_standard_errors <- sqrt(diag(vcov(arima_model)))
  arima_confidence_intervals <- confint(arima_model)
  
  # Create a dataframe to store the results
  arima_parameters <- data.frame(
    Coefficient = arima_coefficients,
    Standard_Error = arima_standard_errors,
    Lower_CI = arima_confidence_intervals[, 1],
    Upper_CI = arima_confidence_intervals[, 2]
  )
  
  # Print the parameter estimates
  print("ARIMA Model Parameter Estimates:")
  print(arima_parameters)
  
  # Extract ARIMA model coefficients
  arima_coefficients <- coef(arima_model)
  
  # Create a dataframe for the coefficients
  coefficients_table <- data.frame(
    Coefficient = names(arima_coefficients),
    Value = arima_coefficients
  )
  
  # Print the coefficients table
  print("ARIMA Model Coefficients:")
  print(coefficients_table)
    
#---------------------------------------------------------------#
  # ARIMA Model
  arima_model <- auto.arima(train_df)
  arima_forecast <- forecast(arima_model, h = length(test_df))
  arima_accuracy <- accuracy(arima_forecast, test_df)
  
  # Output ARIMA Model Accuracy
  print("ARIMA Model Accuracy:")
  print(arima_accuracy)
  
  # TBATS Model
  tbats_model <- tbats(train_df)
  tbats_forecast <- forecast(tbats_model, h = length(test_df))
  tbats_accuracy <- accuracy(tbats_forecast, test_df)
  
  # Output TBATS Model Accuracy
  print("TBATS Model Accuracy:")
  print(tbats_accuracy)

  # Forecast for ARIMA Model
  arima_forecast_1day <- forecast(arima_model, h = 1)
  arima_forecast_7days <- forecast(arima_model, h = 7)
  arima_forecast_30days <- forecast(arima_model, h = 30)
  
  # Forecast for TBATS Model
  tbats_forecast_1day <- forecast(tbats_model, h = 1)
  tbats_forecast_7days <- forecast(tbats_model, h = 7)
  tbats_forecast_30days <- forecast(tbats_model, h = 30)

  # Calculate accuracy metrics for ARIMA Model 
  arima_accuracy_1day <- accuracy(arima_forecast_1day, test_df[1])
  arima_accuracy_7days <- accuracy(arima_forecast_7days, test_df[1:7])
  arima_accuracy_30days <- accuracy(arima_forecast_30days, test_df[1:30])
  
  # Calculate accuracy metrics for TBATS Model 
  tbats_accuracy_1day <- accuracy(tbats_forecast_1day, test_df[1])
  tbats_accuracy_7days <- accuracy(tbats_forecast_7days, test_df[1:7])
  tbats_accuracy_30days <- accuracy(tbats_forecast_30days, test_df[1:30])
  
  
  # Output accuracy metrics
  print("ARIMA Model Accuracy - 1 Day Forecast:")
  print(arima_accuracy_1day)
  print("ARIMA Model Accuracy - 7 Days Forecast:")
  print(arima_accuracy_7days)
  print("ARIMA Model Accuracy - 30 Days Forecast:")
  print(arima_accuracy_30days)
  
  print("TBATS Model Accuracy - 1 Day Forecast:")
  print(tbats_accuracy_1day)
  print("TBATS Model Accuracy - 7 Days Forecast:")
  print(tbats_accuracy_7days)
  print("TBATS Model Accuracy - 30 Days Forecast:")
  print(tbats_accuracy_30days)

  # Calculate accuracy metrics for ARIMA Model - 3 months forecast
  arima_accuracy_3months <- accuracy(arima_forecast_3months, test_df[1:90])
  
  # Calculate accuracy metrics for ARIMA Model - 6 months forecast
  arima_accuracy_6months <- accuracy(arima_forecast_6months, test_df[1:180])
  
  # Calculate accuracy metrics for ARIMA Model - 1 year forecast
  arima_accuracy_1year <- accuracy(arima_forecast_1year, test_df[1:365])
  # Calculate accuracy metrics for TBATS Model - 3 months forecast
  tbats_accuracy_3months <- accuracy(tbats_forecast_3months, test_df[1:90])
  
  # Calculate accuracy metrics for TBATS Model - 6 months forecast
  tbats_accuracy_6months <- accuracy(tbats_forecast_6months, test_df[1:180])
  
  # Calculate accuracy metrics for TBATS Model - 1 year forecast
  tbats_accuracy_1year <- accuracy(tbats_forecast_1year, test_df[1:365])
  
  

  comparison_df <- data.frame(
    Model = c("ARIMA", "TBATS"),
    Training_RMSE = c(arima_accuracy[2], tbats_accuracy[2]),
    Training_MAE = c(arima_accuracy[3], tbats_accuracy[3]),
    Training_MAPE = c(arima_accuracy[5], tbats_accuracy[5]),
    Forecast_1day_RMSE = c(arima_accuracy_1day[2], tbats_accuracy_1day[2]),
    Forecast_7days_RMSE = c(arima_accuracy_7days[2], tbats_accuracy_7days[2]),
    Forecast_30days_RMSE = c(arima_accuracy_30days[2], tbats_accuracy_30days[2]),
    Forecast_3months_RMSE = c(accuracy(arima_forecast_3months, test_df[1:90])[2], 
                              accuracy(tbats_forecast_3months, test_df[1:90])[2]),
    Forecast_6months_RMSE = c(accuracy(arima_forecast_6months, test_df[1:180])[2], 
                              accuracy(tbats_forecast_6months, test_df[1:180])[2]),
    Forecast_1year_RMSE = c(accuracy(arima_forecast_1year, test_df[1:365])[2], 
                            accuracy(tbats_forecast_1year, test_df[1:365])[2])
  )
  
  
  # Print the comparison dataframe
  print(comparison_df)
  
  # Calculate accuracy metrics for ARIMA Model on testing data
  arima_accuracy_test <- accuracy(arima_forecast, test_df)
  
  # Calculate accuracy metrics for TBATS Model on testing data
  tbats_accuracy_test <- accuracy(tbats_forecast, test_df)
  
  # Create a dataframe for testing data accuracy comparison
  testing_comparison <- data.frame(
    Model = c("ARIMA", "TBATS"),
    RMSE = c(arima_accuracy_test[2], tbats_accuracy_test[2]),
    MAE = c(arima_accuracy_test[3], tbats_accuracy_test[3]),
    MAPE = c(arima_accuracy_test[5], tbats_accuracy_test[5])
  )
  
  # Print testing data accuracy comparison
  print("Testing Data Accuracy Comparison:")
  print(testing_comparison)
  
  # ARIMA Model Residuals Plot
  print("ARIMA Model Residuals Analysis:")
  checkresiduals(arima_model)
  
  # TBATS Model Residuals Plot
  print("TBATS Model Residuals Analysis:")
  checkresiduals(tbats_model)
  
  library(ggplot2)
  
  # Function to plot forecast
  plot_forecast <- function(model_forecast, model_name) {
    forecast_data <- data.frame(date = as.Date(time(model_forecast$mean)),
                                sales = model_forecast$mean,
                                lower_bound = model_forecast$lower,
                                upper_bound = model_forecast$upper)
    
    ggplot(forecast_data, aes(x = date, y = sales)) +
      geom_line(color = "blue") +
      geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "lightblue", alpha = 0.3) +
      labs(title = paste(model_name, "Sales Forecast"),
           x = "Date", y = "Sales") +
      theme_minimal()
  }
  
  
  
  # Plot ARIMA Forecast
  arima_plot <- plot_forecast(arima_forecast, "ARIMA")
  
  
  # Plot TBATS Forecast
  tbats_plot <- plot_forecast(tbats_forecast, "TBATS")
  
  install.packages("gridExtra")
  library(gridExtra)
  
  # Arrange plots in a grid
  grid.arrange(arima_plot, tbats_plot, ncol = 2)
  
  #------------------------------------------------------###
  # Load required libraries
  library(forecast)
  
  # Function to train TBATS model and forecast sales for each region
  forecast_sales_tbats <- function(region_data) {
    # Prepare time series data
    ts_data <- ts(region_data$sales, frequency = 7)
    
    # Train TBATS model
    tbats_model <- tbats(ts_data)
    
    # Forecast sales for the next 30 days
    sales_forecast <- forecast(tbats_model, h = 30)
    
    # Return the forecasted sales
    return(sales_forecast)
  }
  
  # Split df into regions
  df_region <- split(df, df$region)
  
  # Forecast sales for each region using TBATS model
  region_forecasts_tbats <- lapply(df_region, forecast_sales_tbats)
  
  # Output forecasted sales for each region
  for (i in 1:length(region_forecasts_tbats)) {
    region <- names(region_forecasts_tbats)[i]
    cat("Region:", region, "\n")
    print(region_forecasts_tbats[[i]])
  }
  
  # Extract forecasted values for each region
  forecast_values_tbats <- lapply(region_forecasts_tbats, function(f) f$mean)
  
  # Combine forecasted values into a single time series
  combined_forecast_tbats <- ts(do.call(cbind, forecast_values_tbats), start = start(ts_df), frequency = frequency(ts_df))
  
  # Plot forecasted sales for each region
  par(mfrow = c(2, 2))
  for (i in 1:length(region_forecasts_tbats)) {
    region <- names(region_forecasts_tbats)[i]
    sales_forecast_tbats <- region_forecasts_tbats[[i]]
    
    # Plot the forecast for the current region
    plot(sales_forecast_tbats, main = paste("Forecast for", region), xlab = "Date", ylab = "Sales")
  }

  # Load required libraries
  library(forecast)
  
  # Assuming 'order.date' is the date column
  df$order.date <- as.Date(df$order.date)
  
  # Create a time series object
  ts_df <- ts(df$sales, start = c(min(df$order_year), 1), frequency = 12)
  
  # Split df into training and test sets
  train_size <- 0.8  # 80% training df, 20% test df
  train_index <- 1:round(train_size * length(ts_df))
  train_df <- ts_df[train_index]
  test_df <- ts_df[-train_index]
  
  # ARIMA Model
  arima_model <- auto.arima(train_df)
  arima_forecast <- forecast(arima_model, h = length(test_df))
  arima_accuracy <- accuracy(arima_forecast, test_df)
  
  # TBATS Model
  tbats_model <- tbats(train_df)
  tbats_forecast <- forecast(tbats_model, h = length(test_df))
  tbats_accuracy <- accuracy(tbats_forecast, test_df)
  
  # Calculate MAPE function
  mape <- function(actual, forecast) {
    mean(abs((actual - forecast)/actual)) * 100
  }
  
  # Calculate MSE, RMSE, and MAPE for ARIMA
  arima_mse <- mean((test_df - arima_forecast$mean)^2)
  arima_rmse <- sqrt(arima_mse)
  arima_mape <- mape(test_df, arima_forecast$mean)
  
  # Calculate MSE, RMSE, and MAPE for TBATS
  tbats_mse <- mean((test_df - tbats_forecast$mean)^2)
  tbats_rmse <- sqrt(tbats_mse)
  tbats_mape <- mape(test_df, tbats_forecast$mean)
  
  # Create a dataframe for error comparison
  error_comparison <- data.frame(
    Model = c("ARIMA", "TBATS"),
    MSE = c(arima_mse, tbats_mse),
    RMSE = c(arima_rmse, tbats_rmse),
    MAPE = c(arima_mape, tbats_mape)
  )
  
  # Print the error comparison dataframe
  print("Error Comparison:")
  print(error_comparison)
  
  
    
  