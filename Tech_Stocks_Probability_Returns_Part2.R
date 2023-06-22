library(quantmod)
library(dplyr)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')
dfs <- list()

# Fetching stock data for each symbol and storing in dfs list
for (symbol in symbols) {
  data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  dfs[[symbol]] <- data
}

# Calculate the mean and standard deviation of returns for each symbol
returns <- lapply(dfs, function(data) dailyReturn(Ad(data)))
mean_return <- sapply(returns, mean)
std_return <- sapply(returns, sd)

# Calculate the probability of positive returns for each symbol
prob_positive_return <- sapply(returns, function(ret) mean(ret > 0))

# Calculate the probability of negative returns for each symbol
prob_negative_return <- sapply(returns, function(ret) mean(ret < 0))

# Calculate the probability of returns within a certain range for each symbol
lower_bound <- -0.01
upper_bound <- 0.01
prob_within_range <- sapply(returns, function(ret) mean(ret >= lower_bound & ret <= upper_bound))

# Print the results for each symbol
for (symbol in symbols) {
  cat("Symbol:", symbol, "\n")
  cat("Mean return:", mean_return[symbol], "\n")
  cat("Standard deviation of return:", std_return[symbol], "\n")
  cat("Probability of positive return:", prob_positive_return[symbol], "\n")
  cat("Probability of negative return:", prob_negative_return[symbol], "\n")
  cat("Probability of return within range [", lower_bound, ", ", upper_bound, "]:", prob_within_range[symbol], "\n\n")
}


  