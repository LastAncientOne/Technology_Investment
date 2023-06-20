library(quantmod)
library(dplyr)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')
dfs <- list()

# Calculate the mean and standard deviation of returns for each symbol
for (symbol in symbols) {
  data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  returns <- dailyReturn(data, type = "log")
  mean_return <- mean(returns, na.rm = TRUE)
  std_dev <- sd(returns, na.rm = TRUE)
  
  # Calculate the probability of positive returns for each symbol
  prob_positive <- sum(returns > 0, na.rm = TRUE) / length(returns)
  
  # Calculate the probability of negative returns for each symbol
  prob_negative <- sum(returns < 0, na.rm = TRUE) / length(returns)
  
  # Calculate the probability of returns within a certain range for each symbol
  lower_bound <- -0.02
  upper_bound <- 0.02
  prob_within_range <- sum(returns >= lower_bound & returns <= upper_bound, na.rm = TRUE) / length(returns)
  
  # Store the results in a data frame
  result <- data.frame(Symbol = symbol, Mean_Return = mean_return, Std_Deviation = std_dev,
                       Prob_Positive = prob_positive, Prob_Negative = prob_negative, Prob_Within_Range = prob_within_range)
  dfs[[symbol]] <- result
}

# Combine the results for all symbols into a single data frame
df <- do.call(rbind, dfs)

# Print the results for each symbol
print(df)
