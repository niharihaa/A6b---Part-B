# Set working directory and load necessary libraries
setwd('C:\\Users\\nihar\\OneDrive\\Desktop\\Bootcamp\\SCMA 632\\DataSet')
getwd()

# Load necessary libraries
library(readxl)
library(dplyr)
library(janitor)
library(urca)
library(vars)
library(ggplot2)

# Clear all graphics devices
graphics.off()

# Load the dataset
df <- read_excel('pinksheet.xlsx', sheet = "Monthly Prices", skip = 6)

# Rename the first column to "Date"
colnames(df)[1] <- 'Date'

# Convert the Date column to Date format
df$Date <- as.Date(paste0(df$Date, "01"), format = "%YM%m%d")
str(df)

# Select specific columns (Date and selected commodities)
commodity <- df[,c(1,3,25,70,72,61,31)] %>%
  clean_names()

str(commodity)

# Check column names
colnames(commodity)

# Check for missing values
missing_values <- sapply(commodity, function(x) sum(is.na(x)))
missing_values

# Since there are no missing values, proceed to the next part

# Mapping of column names to more readable commodity names
commodity_names <- c(
  crude_brent = "Crude Brent",
  soybeans = "Soybeans",
  gold = "Gold",
  silver = "Silver",
  urea_ee_bulk = "Urea EE Bulk",
  maize = "Maize"
)

# Print column names and corresponding readable names for debugging
print("Column names and corresponding readable names:")
for (col in names(commodity)[-1]) {
  print(paste(col, ":", commodity_names[[col]]))
}

# Visualize data directly
for (col in names(commodity)[-1]) { # Skip the date column
  print(col)  # Print column name for debugging
  p <- ggplot(commodity, aes_string(x = "date", y = col)) +
    geom_line() +
    labs(title = paste("Price of", commodity_names[[col]]), x = "Date", y = "Price") +
    theme_minimal()
  
  # Print the plot to display it
  print(p)
}

# Prepare data for VAR and VECM analysis
commodity_data <- dplyr::select(commodity, -date)
columns_to_test <- names(commodity_data)

# Stationary test
non_stationary_count <- 0
stationary_columns <- c()
non_stationary_columns <- c()

for (col in columns_to_test) {
  adf_result <- ur.df(commodity_data[[col]], type = "none", selectlags = "AIC")
  p_value <- adf_result@testreg$coefficients[2, 4]
  cat("\nADF test result for column:", col, "\n")
  print(summary(adf_result))
  
  if (p_value > 0.05) {
    non_stationary_count <- non_stationary_count + 1
    non_stationary_columns <- c(non_stationary_columns, col)
  } else {
    stationary_columns <- c(stationary_columns, col)
  }
}

cat("\nNumber of non-stationary columns:", non_stationary_count, "\n")
cat("Non-stationary columns:", paste(non_stationary_columns, collapse=", "), "\n")
cat("Stationary columns:", paste(stationary_columns, collapse=", "), "\n")

# Co-Integration Test (Johansen's Test)
lags <- VARselect(commodity_data, lag.max = 10, type = "const")
lag_length <- lags$selection[1]

vecm_model <- ca.jo(commodity_data, ecdet = 'const', type = 'eigen', K = lag_length, spec = 'transitory')
summary(vecm_model)
r <- 3 # Replace with the actual number from the test results

if (r > 0) {
  vecm <- cajorls(vecm_model, r = r)
  summary(vecm)
  vecm_coefs <- vecm$rlm$coefficients
  print(vecm_coefs)
  vecm_pred <- vec2var(vecm_model, r = r)
  forecast <- predict(vecm_pred, n.ahead = 24)
  par(mar = c(4, 4, 2, 2))
  plot(forecast)
  
} else {
  var_model <- VAR(commodity_data, p = lag_length, type = "const")
  summary(var_model)
  causality_results <- causality(var_model)
  print(causality_results)
  forecast <- predict(var_model, n.ahead = 24)
  par(mar = c(4, 4, 2, 2))
  plot(forecast)
}

forecast

# Load the dataset
df <- read_excel('pinksheet.xlsx', sheet = "Monthly Prices", skip = 6)

# Rename the first column to "Date"
colnames(df)[1] <- 'Date'

# Convert the Date column to Date format
df$Date <- as.Date(paste0(df$Date, "01"), format = "%YM%m%d")
str(df)

# Select metal commodities columns (Date and selected commodities)
commodity2 <- df[,c(1, 64, 65, 66, 67, 68, 69)] %>%
  clean_names()

str(commodity2)

# Check column names
colnames(commodity2)

# Check for missing values in the commodity data excluding the Date column
missing_values <- sapply(commodity2[-1], function(x) sum(is.na(x)))
missing_values

# Check the first few rows to ensure data integrity
head(commodity2)

# Mapping of new column names to more readable commodity names
commodity2_names <- c(
  iron_ore = "Iron Ore",
  copper = "Copper",
  lead = "Lead",
  tin = "Tin",
  nickel = "Nickel",
  zinc = "Zinc"
)

# Print column names and corresponding readable names for debugging
print("Column names and corresponding readable names:")
for (col in names(commodity2)[-1]) {
  print(paste(col, ":", commodity2_names[[col]]))
}

# Visualize data directly
for (col in names(commodity2)[-1]) { # Skip the date column
  print(col)  # Print column name for debugging
  p <- ggplot(commodity2, aes_string(x = "date", y = col)) +
    geom_line() +
    labs(title = paste("Price of", commodity2_names[[col]]), x = "Date", y = "Price") +
    theme_minimal()
  
  # Print the plot to display it
  print(p)
}

# Prepare data for VAR and VECM analysis
commodity2_data <- dplyr::select(commodity2, -date)
columns_to_test2 <- names(commodity2_data)

# Stationarity test
non_stationary_count2 <- 0
stationary_columns2 <- c()
non_stationary_columns2 <- c()

for (col in columns_to_test2) {
  adf_result2 <- ur.df(commodity2_data[[col]], type = "none", selectlags = "AIC")
  p_value2 <- adf_result2@testreg$coefficients[2, 4]
  cat("\nADF test result for column:", col, "\n")
  print(summary(adf_result2))
  
  if (p_value2 > 0.05) {
    non_stationary_count2 <- non_stationary_count2 + 1
    non_stationary_columns2 <- c(non_stationary_columns2, col)
  } else {
    stationary_columns2 <- c(stationary_columns2, col)
  }
}

cat("\nNumber of non-stationary columns:", non_stationary_count2, "\n")
cat("Non-stationary columns:", paste(non_stationary_columns2, collapse=", "), "\n")
cat("Stationary columns:", paste(stationary_columns2, collapse=", "), "\n")

# Co-Integration Test (Johansen's Test)
lags2 <- VARselect(commodity2_data, lag.max = 10, type = "const")
lag_length2 <- lags2$selection[1]

vecm_model2 <- ca.jo(commodity2_data, ecdet = 'const', type = 'eigen', K = lag_length2, spec = 'transitory')
summary(vecm_model2)
r2 <- 3 # Replace with the actual number from the test results

if (r2 > 0) {
  vecm2 <- cajorls(vecm_model2, r = r2)
  summary(vecm2)
  vecm_coefs2 <- vecm2$rlm$coefficients
  print(vecm_coefs2)
  vecm_pred2 <- vec2var(vecm_model2, r = r2)
  forecast2 <- predict(vecm_pred2, n.ahead = 24)
  par(mar = c(4, 4, 2, 2))
  plot(forecast2)
  
} else {
  var_model2 <- VAR(commodity2_data, p = lag_length2, type = "const")
  summary(var_model2)
  causality_results2 <- causality(var_model2)
  print(causality_results2)
  forecast2 <- predict(var_model2, n.ahead = 24)
  par(mar = c(4, 4, 2, 2))
  plot(forecast2)
}

forecast2
