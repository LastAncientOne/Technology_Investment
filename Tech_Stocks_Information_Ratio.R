library(quantmod)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Create an empty dataframe to store the information ratios
information_ratios <- data.frame(Symbol = character(), InformationRatio = numeric(), stringsAsFactors = FALSE)

# Calculate the information ratio for each symbol
for (symbol in symbols) {
  # Get the adjusted closing prices for the symbol
  prices <- Ad(get(symbol))
  
  # Calculate the daily returns
  returns <- dailyReturn(prices)
  
  # Calculate the mean return
  mean_return <- mean(returns)
  
  # Calculate the standard deviation of returns
  std_dev <- sd(returns)
  
  # Calculate the information ratio
  information_ratio <- mean_return / std_dev
  
  # Add the symbol and information ratio to the dataframe
  information_ratios <- rbind(information_ratios, data.frame(Symbol = symbol, InformationRatio = information_ratio))
}

# Print the information ratios
print(information_ratios)