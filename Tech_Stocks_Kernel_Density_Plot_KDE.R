# Load required libraries
library(quantmod)

# Define the symbols and dates
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Function to retrieve stock data and calculate percent change
get_percent_change <- function(symbol, start_date, end_date) {
  stock_data <- getSymbols(symbol, from = start_date, to = end_date, auto.assign = FALSE)
  returns <- diff(log(Cl(stock_data)))
  return(returns)
}

# Retrieve percent change data for each stock
percent_changes <- lapply(symbols, get_percent_change, start_date = start_date, end_date = end_date)

# Combine percent changes into a single data frame
percent_changes_df <- data.frame(symbol = rep(symbols, times = sapply(percent_changes, length)),
                                 percent_change = unlist(percent_changes))

# Create kernel density plot
library(ggplot2)
ggplot(percent_changes_df, aes(x = percent_change)) +
  geom_density(fill = "blue", alpha = 0.5) +
  facet_wrap(~ symbol, ncol = 4) +
  labs(title = "Kernel Density Plot of Percent Change for Stocks",
       x = "Percent Change", y = "Density")
