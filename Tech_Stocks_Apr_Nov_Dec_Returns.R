library(quantmod)

# Define the symbols
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')

# Define the start and end dates
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Function to calculate monthly returns
calculate_monthly_returns <- function(symbol) {
  # Get the stock data
  data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  
  # Calculate monthly returns
  monthly_returns <- monthlyReturn(Ad(data))
  
  # Convert to data frame and add symbol column
  monthly_returns <- data.frame(Date = index(monthly_returns), monthly_returns)
  monthly_returns$Symbol <- symbol
  
  return(monthly_returns)
}

# Calculate monthly returns for each symbol
returns <- lapply(symbols, calculate_monthly_returns)

# Combine returns for all symbols into a single data frame
all_returns <- do.call(rbind, returns)

# Filter returns for April, November, and December
april_returns <- all_returns[format(all_returns$Date, "%m") == "04", ]
november_returns <- all_returns[format(all_returns$Date, "%m") == "11", ]
december_returns <- all_returns[format(all_returns$Date, "%m") == "12", ]

# Plot bar charts for April, November, and December returns
par(mfrow = c(3, 1))  # Set up a 3-row grid of plots
barplot(april_returns$monthly.returns, names.arg = april_returns$Symbol, main = "April Returns")
barplot(november_returns$monthly.returns, names.arg = november_returns$Symbol, main = "November Returns")
barplot(december_returns$monthly.returns, names.arg = december_returns$Symbol, main = "December Returns")
