library(quantmod)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Function to retrieve stock data and plot the line chart
plot_stock_chart <- function(symbol, start_date, end_date) {
  # Retrieve stock data
  stock_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  
  # Extract the closing prices
  closing_prices <- Cl(stock_data)
  
  # Create a time series object with the closing prices
  stock_ts <- as.ts(closing_prices)
  
  # Plot the line chart
  plot(stock_ts, type = 'l', main = paste('Stock Chart for', symbol),
       xlab = 'Date', ylab = 'Closing Price')
}
