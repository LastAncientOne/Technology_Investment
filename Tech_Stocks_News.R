library(newsanchor)
library(tidyverse)

# Define the symbols
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')

# Define the bullish and bearish keywords
bullish_keywords <- c('bullish', 'positive', 'upward', 'up')
bearish_keywords <- c('bearish', 'negative', 'downward', 'down')

# Initialize data frames to store results
bullish_counts <- data.frame(Symbol = symbols, Count = 0)
bearish_counts <- data.frame(Symbol = symbols, Count = 0)

# Loop over each symbol
for (symbol in symbols) {
  # Search for news using the symbol as a query
  news <- news_search(query = symbol, start_date = start_date, end_date = end_date)
  
  # Count the number of bullish news
  bullish_count <- sum(str_count(news$title, regex(paste(bullish_keywords, collapse = '|'), ignore_case = TRUE)))
  bullish_counts[bullish_counts$Symbol == symbol, "Count"] <- bullish_count
  
  # Count the number of bearish news
  bearish_count <- sum(str_count(news$title, regex(paste(bearish_keywords, collapse = '|'), ignore_case = TRUE)))
  bearish_counts[bearish_counts$Symbol == symbol, "Count"] <- bearish_count
}

# View the results
bullish_counts
bearish_counts
