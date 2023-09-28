# Load required libraries
library(quantmod)
library(ggplot2)

# Define the stock symbols and date range
symbols <- c('BABA', 'JD', 'BIDU', 'TCEHY', 'NTES', 'VIPS', 'TCOM', 'BILI')
start_date <- as.Date('2020-01-01')
end_date <- as.Date('2023-01-01')

# Download stock data
getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Create a loop to plot trendline for each stock
for (symbol in symbols) {
  # Extract stock data
  stock_data <- get(symbol)
  
  # Calculate the trendline using linear regression
  trendline <- lm(Cl(stock_data) ~ index(stock_data))
  
  # Create a data frame with x and y values for the trendline
  trendline_data <- data.frame(
    Date = index(stock_data),
    Trendline = fitted.values(trendline)
  )
  
  # Plot the stock's closing prices and the trendline
  p <- ggplot(stock_data, aes(x = index(stock_data))) +
    geom_line(aes(y = Cl(stock_data)), color = "blue") +
    geom_line(data = trendline_data, aes(y = Trendline), color = "red") +
    labs(title = paste("Trendline for", symbol),
         x = "Date", y = "Closing Price") +
    theme_minimal()
  
  # Print the plot for each stock
  print(p)
}
