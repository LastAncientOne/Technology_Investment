# Load Library
library(quantmod)

# Define the symbols and date range
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start <- as.Date('2016-01-01')
end <- as.Date('2023-01-01')

# Fetch historical stock data for each symbol
getSymbols(symbols, from = start, to = end)

# Create an empty data frame to store the statistics
stats_df <- data.frame(Symbol = character(),
                       Min = numeric(),
                       Max = numeric(),
                       Median = numeric(),
                       stringsAsFactors = FALSE)

# Iterate over each symbol
for (symbol in symbols) {
  # Extract the stock's closing prices
  prices <- Cl(get(symbol))
  
  # Calculate statistics
  min_price <- min(prices)
  max_price <- max(prices)
  median_price <- median(prices)
  mode_price <- mode(prices)
  mean_price <- mean(prices)
  
  # Add the statistics to the data frame
  stats_df <- rbind(stats_df, data.frame(Symbol = symbol,
                                         Min = min_price,
                                         Max = max_price,
                                         Median = median_price,
                                         Mode = mode_price,
                                         Mean = mean_price))
}

# Print the resulting data frame
print(stats_df)