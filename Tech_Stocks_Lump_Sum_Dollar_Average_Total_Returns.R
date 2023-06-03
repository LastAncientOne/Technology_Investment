library(quantmod)

# Set the symbols, start date, and end date
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Retrieve the stock data for the specified symbols and time period
getSymbols(symbols, from = start_date, to = end_date)

# Create an empty data frame to store the total returns
total_returns <- data.frame(Symbol = character(), `3 Years` = numeric(), `5 Years` = numeric(), `7 Years` = numeric(), stringsAsFactors = FALSE)

# Calculate total returns for each stock
for (symbol in symbols) {
  # Extract the stock's adjusted closing prices
  prices <- Ad(get(symbol))
  
  # Calculate total returns for 3, 5, and 7 years
  returns_3yrs <- periodReturn(prices, period = "yearly", type = "log")['2016::2018']
  returns_5yrs <- periodReturn(prices, period = "yearly", type = "log")['2016::2020']
  returns_7yrs <- periodReturn(prices, period = "yearly", type = "log")['2016::2022']
  
  # Calculate the total return percentage
  total_return_3yrs <- sum(returns_3yrs)
  total_return_5yrs <- sum(returns_5yrs)
  total_return_7yrs <- sum(returns_7yrs)
  
  # Add the results to the data frame
  total_returns <- rbind(total_returns, data.frame(Symbol = symbol, `3 Years` = total_return_3yrs, `5 Years` = total_return_5yrs, `7 Years` = total_return_7yrs))
}

# Print the total returns for each stock
print(total_returns)