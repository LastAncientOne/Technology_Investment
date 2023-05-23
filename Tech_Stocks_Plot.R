library(quantmod)

# Define the stock symbols and date range
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start <- as.Date('2016-01-01')
end <- as.Date('2023-01-01')

# Function to fetch stock data
getStockData <- function(symbol) {
  tryCatch(
    {
      # Download the stock data
      stockData <- getSymbols(symbol, from = start, to = end, auto.assign = FALSE)
      # Extract the closing prices
      close <- Cl(stockData)
      # Return a data frame with date and closing prices
      data.frame(Date = index(close), Close = as.vector(close))
    },
    error = function(e) {
      # If there's an error, return NULL
      return(NULL)
    }
  )
}

# Fetch stock data for all symbols
stockData <- lapply(symbols, getStockData)

# Remove NULL elements
stockData <- stockData[!sapply(stockData, is.null)]

# Create an empty plot
plot(NULL, main="Technology Stocks", xlim = c(start, end), ylim = c(0, 2000), xlab = "Date", ylab = "Closing Price")

# Plot the closing prices for each stock
colors <- rainbow(length(stockData))
legendItems <- c()
for (i in 1:length(stockData)) {
  stock <- stockData[[i]]
  symbol <- symbols[i]
  color <- colors[i]
  lines(stock$Date, stock$Close, col = color)
  legendItems <- c(legendItems, paste(symbol, "-", colnames(stock)[2]))
}

# Add a legend
legend("topleft", legend = legendItems, col = colors, lwd = 1, cex = 0.8)