library(quantmod)

# Define the symbols
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')

# Define the start and end dates
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Loop through each symbol
for (symbol in symbols) {
  # Download the stock data
  getSymbols(symbol, src = "yahoo", from = start_date, to = end_date)
  
  # Calculate the monthly returns
  returns <- monthlyReturn(Cl(get(symbol)))
  
  # Plot the bar chart for each month
  barplot(returns, main = paste("Monthly Returns for", symbol), xlab = "Date", ylab = "Returns")
}