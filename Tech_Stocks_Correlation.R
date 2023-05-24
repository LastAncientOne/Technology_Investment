library(quantmod)
library(corrplot)

# Define the symbols and date range
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start <- as.Date('2016-01-01')
end <- as.Date('2023-01-01')

# Fetch historical stock data for each symbol
getSymbols(symbols, from = start, to = end)

# Combine Close Price for each symbols 
closing_prices <- NULL
for (symbol in symbols) {
  closing_prices <- cbind(closing_prices, Cl(get(symbol)))
}
colnames(closing_prices) <- symbols

# Correlation 
correlation_matrix <- cor(closing_prices)

# Plot Heatmap
heatmap(correlation_matrix, col = colorRampPalette(c("red", "white", "blue"))(50))

# Plot Correlation 
corrplot(correlation_matrix, method = "color")

# Plot Correlation 
corrplot(correlation_matrix, method = "number")

# Correlation Dataframe
correlation_df <- as.data.frame(correlation_matrix)
print(correlation_df)