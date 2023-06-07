library(quantmod)

# Define the symbols
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')

# Define the start and end dates
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Create an empty list to store monthly returns for each stock
monthly_returns <- list()

# Loop through each symbol
for (symbol in symbols) {
  # Download the stock price data
  getSymbols(symbol, from = start_date, to = end_date)
  
  # Extract the adjusted closing prices
  prices <- Ad(get(symbol))
  
  # Calculate monthly returns
  monthly_returns[[symbol]] <- monthlyReturn(prices)
}

# Create a bar chart for each month
for (i in 1:12) {
  # Create a new plot for each month
  plot.new()
  
  # Set the plot title and axis labels
  month_label <- format(as.Date(paste('2000', i, '01', sep = '-')), '%B')
  title(main = paste('Stock Returns -', month_label), xlab = 'Stocks', ylab = 'Returns')
  
  # Calculate the returns for the month
  returns <- sapply(monthly_returns, function(x) x[i])
  
  # Plot the bar chart
  barplot(returns, main = paste('Stock Returns -', month_label), xlab = 'Stocks', ylab = 'Returns')
  
  # Add labels to the bars
  text(x = 1:length(symbols), y = returns, labels = round(returns, 4), pos = 3)
}