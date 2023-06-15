library(ggplot2)
library(quantmod)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Store the adjusted closing prices for each stock in a list
stock_prices <- lapply(symbols, function(symbol) {
  prices <- Ad(get(symbol))
  colnames(prices) <- symbol
  prices
})

# Combine the stock prices into a single data frame
all_prices <- do.call(merge, stock_prices)

# Calculate the buy and hold returns for each stock
buy_hold_returns <- apply(all_prices, 2, function(prices) {
  initial_price <- prices[1]
  final_price <- prices[length(prices)]
  return((final_price - initial_price) / initial_price)
})

# Create a data frame to store the results
results <- data.frame(Symbol = symbols, BuyHoldReturns = buy_hold_returns)
results
# Create a new dataframe for plotting
plot_data <- data.frame(Date = index(all_prices), all_prices)

# Reshape the data from wide to long format
plot_data_long <- reshape2::melt(plot_data, id.vars = "Date", variable.name = "Symbol", value.name = "Price")

# Convert the Date column to a date object
plot_data_long$Date <- as.Date(plot_data_long$Date)

# Plot line charts separately for each symbol
plots <- lapply(unique(plot_data_long$Symbol), function(symbol) {
  data <- plot_data_long[plot_data_long$Symbol == symbol, ]
  
  p <- ggplot(data, aes(x = Date, y = Price)) +
    geom_line() +
    labs(title = paste("Buy and Hold Strategy -", symbol),
         x = "Date",
         y = "Price")
  
  print(p)
})

# Arrange the plots in a grid
gridExtra::grid.arrange(grobs = plots)