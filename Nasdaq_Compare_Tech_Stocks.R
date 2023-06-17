library(quantmod)
nasdaq <- '^IXIC'
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Function to normalize a vector
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Loop through each symbol
for (symbol in symbols) {
  # Get stock data
  stock_data <- getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  nasdaq_data <- getSymbols(nasdaq, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  
  # Extract adjusted close prices
  stock_prices <- Ad(stock_data)
  nasdaq_prices <- Ad(nasdaq_data)
  
  # Normalize stock prices
  normalized_prices <- normalize(as.numeric(stock_prices))
  normalized_nasdaq <- normalize(as.numeric(nasdaq_prices))
  
  # Create a data frame with normalized prices
  df <- data.frame(Date = index(stock_data), Stock = normalized_prices, NASDAQ = normalized_nasdaq)
  
  # Plot the normalized stock chart with NASDAQ
  plot(df$Date, df$Stock, type = "l", col = "blue", xlab = "Date", ylab = "Normalized Price", main = paste(symbol, ' vs Nasdaq'),)
  lines(df$Date, df$NASDAQ, col = "red")
  legend("bottomright", legend = c(symbol, "NASDAQ"), col = c("blue", "red"), lty = 1)
}