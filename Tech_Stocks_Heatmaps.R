library(quantmod)
library(ggplot2)
library(reshape2)

# Define the stock symbols and index symbol
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
nasdaq <- '^IXIC'

# Set the start and end dates
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Download the historical stock prices
getSymbols(c(nasdaq, symbols), from = start_date, to = end_date, warnings = FALSE)

# Combine the closing prices of the stocks into a single data frame
prices <- do.call(merge, lapply(symbols, function(sym) Cl(get(sym))))

# Calculate the correlation matrix
cor_matrix <- cor(prices)

# Convert correlation matrix to a tidy data frame
cor_df <- melt(cor_matrix)

# Plot the correlation heat map using ggplot2
ggplot(cor_df, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
  labs(title = "Stock Correlation Heat Map",
       x = "Stock Symbols",
       y = "Stock Symbols")