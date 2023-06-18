# Load required libraries
library(quantmod)

# Define the symbols and dates
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Function to plot kernel density
plot_kernel_density <- function(symbol) {
  # Get the stock data
  stock_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  
  # Calculate the percent change
  percent_change <- diff(log(Cl(stock_data)))
  
  # Plot the kernel density
  plot(density(percent_change), main = paste("Kernel Density Plot -", symbol), xlab = "Percent Change")
}

# Loop through each symbol and plot the kernel density
for (symbol in symbols) {
  plot_kernel_density(symbol)
  