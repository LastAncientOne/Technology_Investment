# Load required packages
library(quantmod)

# Define the symbols and date range
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start <- '2016-01-01'
end <- '2023-01-01'

# Retrieve the stock data
getSymbols(symbols, from = start, to = end)

# Create an empty list to store the statistics
statistics <- list()

# Calculate returns and statistics for each stock
for (symbol in symbols) {
  # Extract the stock's price series
  stock_prices <- Ad(get(symbol))
  
  # Calculate the daily returns
  stock_returns <- dailyReturn(stock_prices)
  
  # Calculate statistics for the returns
  returns_stats <- data.frame(
    Symbol = symbol,
    Mean_Return = mean(stock_returns, na.rm = TRUE),
    SD_Return = sd(stock_returns, na.rm = TRUE),
    Min_Return = min(stock_returns, na.rm = TRUE),
    Max_Return = max(stock_returns, na.rm = TRUE),
    Median_Return = median(stock_returns, na.rm = TRUE)
  )
  
  # Append the statistics to the list
  statistics[[symbol]] <- returns_stats
}

# Combine the statistics for all stocks into a single data frame
all_stats <- do.call(rbind, statistics)

# Print the dataframe with statistics
print(all_stats)