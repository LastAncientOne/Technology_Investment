library(tidyquant)
library(ggplot2)

# Define the symbols
symbols <- c('AMD', 'NVDA', 'MSFT', 'GOOGL', 'QCOM', 'BABA', 'TSM', 'META', 'NFLX', 'ADBE', 'PYPL', 'AVGO', 'MU', 'AMAT', 'INTC', 'STX', 'LUMN', 'ETSY', 'LRCX', 'CSCO')

# Define the start and end dates
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Retrieve stock data
stocks <- tq_get(symbols, from = start_date, to = end_date)

# Filter data for April, November, and December
months_of_interest <- c("04", "11", "12")
filtered_stocks <- stocks %>%
  filter(lubridate::month(date) %in% months_of_interest)

# Plot bar chart for each month
bar_plot <- filtered_stocks %>%
  ggplot(aes(x = symbol, y = close)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  facet_wrap(~ lubridate::month(date, label = TRUE), nrow = 1) +
  labs(title = "Stock Performance in April, November, and December",
       x = "Symbol", y = "Closing Price") +
  theme_bw()

# Display the bar chart
print(bar_plot)

# Extract the stock data for April, November, and December
apr_data <- do.call(merge, lapply(symbols, function(sym) Ad(get(sym))[format(index(Ad(get(sym))), "%m") == "04"]))
nov_data <- do.call(merge, lapply(symbols, function(sym) Ad(get(sym))[format(index(Ad(get(sym))), "%m") == "11"]))
dec_data <- do.call(merge, lapply(symbols, function(sym) Ad(get(sym))[format(index(Ad(get(sym))), "%m") == "12"]))

# Calculate the mean closing price for each stock in April, November, and December
apr_mean <- colMeans(apr_data)
nov_mean <- colMeans(nov_data)
dec_mean <- colMeans(dec_data)

# Create bar charts for each month
par(mfrow = c(3, 1))  # Set the layout to 3 rows and 1 column
barplot(apr_mean, main = "April Stock Performance", xlab = "Symbols", ylab = "Mean Closing Price")
barplot(nov_mean, main = "November Stock Performance", xlab = "Symbols", ylab = "Mean Closing Price")
barplot(dec_mean, main = "December Stock Performance", xlab = "Symbols", ylab = "Mean Closing Price")
