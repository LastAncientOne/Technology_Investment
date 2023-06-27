library(quantmod)

nasdaq <- '^IXIC'
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')

start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Fetch the NASDAQ data
price <- getSymbols(nasdaq, from = start_date, to = end_date, src = 'yahoo', warnings = FALSE)

# Fetch the symbols data
getSymbols(symbols, from = start_date, to = end_date, src = 'yahoo', warnings = FALSE)

# Extract the adjusted closing prices
nasdaq_prices <- Ad(get(price))

symbol_prices <- lapply(symbols, function(symbol) Ad(get(symbol)))

# Create a data frame to store the correlation values
correlation_df <- data.frame(Symbol = symbols, Correlation = NA)

# Calculate the correlation between NASDAQ and each symbol
for (i in 1:length(symbols)) {
  symbol_correlation <- cor(nasdaq_prices, symbol_prices[[i]])
  correlation_df[i, "Correlation"] <- symbol_correlation
}

# Print the correlation values
print(correlation_df)