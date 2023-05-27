library(quantmod)
library(ggplot2)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

for (symbol in symbols) {
  data <- Cl(get(symbol))
  ma <- SMA(data, n = 200)
  
  chartSeries(data, name = symbol, subset = "last 5 years")
  addSMA(n = 200, on = 1)
}


# Plot ggplot 
# Create a new data frame to store the closing prices and moving averages
data <- data.frame(Date = index(AMD))
for (symbol in symbols) {
  data[paste(symbol, "Close", sep = "_")] <- Ad(get(symbol))
  data[paste(symbol, "MA200", sep = "_")] <- SMA(Ad(get(symbol)), n = 200)
}

# Plotting each symbol and its 200-day moving average
for (symbol in symbols) {
  p <- ggplot(data, aes(x = Date)) +
    geom_line(aes(y = get(paste(symbol, "Close", sep = "_"))), color = "blue") +
    geom_line(aes(y = get(paste(symbol, "MA200", sep = "_"))), color = "red") +
    labs(title = paste(symbol, "Stock Price and 200-day Moving Average")) +
    theme_minimal()
  
  print(p)
}