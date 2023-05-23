library(quantmod)
library(TTR)
library(zoo)

# Define the stock symbols and date range
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- '2016-01-01'
end_date <- '2023-01-01'

# Function to normalize stock prices
normalize_prices <- function(data) {
  return(data / data[1])
}

# Fetch stock prices and normalize
getSymbols(symbols, from = start_date, to = end_date)
prices <- merge(Ad(AMD), Ad(NVDA), Ad(MSFT), Ad(GOOGL), Ad(QCOM), Ad(BABA), Ad(TSM), Ad(META), Ad(NFLX), Ad(ADBE), Ad(PYPL), Ad(AVGO), Ad(MU), Ad(AMAT), Ad(INTC), Ad(STX), Ad(LUMN), Ad(ETSY), Ad(LRCX), Ad(CSCO))
normalized_prices <- apply(prices, 2, normalize_prices)

# Plot normalized stock prices
matplot(index(normalized_prices), normalized_prices, type = "l", lty = 1, col = 1:length(symbols),
        main = "Normalized Technology Stock Prices",
        xlab = "Date", ylab = "Normalized Price",
        legend = symbols, cex.legend = 0.7)