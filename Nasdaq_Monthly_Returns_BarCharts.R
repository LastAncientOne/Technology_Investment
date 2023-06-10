# Load the required libraries
library(quantmod)
library(dplyr)
library(ggplot2)

# Define the symbols
symbol <- '^IXIC' 

# Define the start and end dates
start_date <- as.Date('2016-01-01')
end_date <- as.Date('2023-01-01')

# Get the historical data
data <- getSymbols(symbol, src = 'yahoo', from = start_date, to = end_date, auto.assign = FALSE)[, 6]

# Calculate daily returns
daily_returns <- diff(log(data))

# Convert daily returns to monthly returns
monthly_returns <- periodReturn(data, period = "monthly", type = "log")

# Calculate yearly returns
yearly_returns <- periodReturn(data, period = "yearly", type = "log")

# Calculate total returns for each month
monthly_total_returns <- data.frame(Date = index(monthly_returns), Total_Returns = rowSums(monthly_returns, na.rm = TRUE))

# Print the monthly returns and yearly returns
print(monthly_returns)
print(yearly_returns)
print(monthly_total_returns)

# Fix the month order
month_order <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
monthly_total_returns$Month <- factor(format(monthly_total_returns$Date, "%b"), levels = month_order)

# Loop for each year and plot monthly charts
years <- unique(format(index(data), "%Y"))
for (year in years) {
  # Subset the monthly returns for the current year
  monthly_returns_year <- subset(monthly_total_returns, format(Date, "%Y") == year)
  
  # Create a bar chart for the current year
  p <- ggplot(data = monthly_returns_year, aes(x = Month, y = Total_Returns)) +
    geom_bar(stat = "identity", fill = ifelse(monthly_returns_year$Total_Returns >= 0, "green", "red")) +
    xlab("Month") +
    ylab("Monthly Returns") +
    ggtitle(paste("Monthly Returns -", year))
  
  print(p)
}

