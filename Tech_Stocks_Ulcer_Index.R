library(quantmod)
library(PerformanceAnalytics)

symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

getSymbols(symbols, from = start_date, to = end_date, adjust = TRUE)

# Calculate the daily returns for each stock
returns <- list()
for (symbol in symbols) {
  returns[[symbol]] <- dailyReturn(Ad(get(symbol)))
}

# Calculate the UlcerIndex for each stock
UlcerIndex_ratios <- data.frame()
for (symbol in symbols) {  
  UlcerIndex_ratio <- UlcerIndex(returns[[symbol]])
  UlcerIndex_ratios <- rbind(UlcerIndex_ratios, data.frame(Symbol = symbol, UlcerIndex = UlcerIndex_ratio))
}

# Print the table of UlcerIndex
print(UlcerIndex_ratios)