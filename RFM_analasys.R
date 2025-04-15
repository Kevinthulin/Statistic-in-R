# RFM Analysis on Online Retail Dataset

# Load required libraries
library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(lubridate)   # For date handling
library(ggplot2)     # For visualization
library(tidyr)       # For data reshaping

# 1. Import the Online Retail dataset
# Note: You'll need to adjust the path to where your file is stored
retail_data <- read_excel("data/Online_Retail.xlsx")

# 2. Data exploration and cleaning
# Display structure and summary
str(retail_data)
summary(retail_data)

# Basic cleaning
clean_retail <- retail_data %>%
  # Remove missing customer IDs as we need them for RFM
  filter(!is.na(CustomerID)) %>%
  # Remove cancelled orders (quantity less than 0)
  filter(Quantity > 0) %>%
  # Remove returns and negative prices
  filter(UnitPrice > 0)

# 3. Set the analysis date (typically the day after the last transaction date)
last_date <- max(clean_retail$InvoiceDate)
analysis_date <- last_date + days(1)

# 4. Calculate RFM metrics for each customer
rfm_data <- clean_retail %>%
  # Calculate total monetary value for each transaction
  mutate(TotalPrice = Quantity * UnitPrice) %>%
  group_by(CustomerID) %>%
  summarize(
    # Recency: days since last purchase
    Recency = as.numeric(difftime(analysis_date, max(InvoiceDate), units = "days")),
    # Frequency: count of invoices/transactions
    Frequency = n_distinct(InvoiceNo),
    # Monetary: total amount spent
    Monetary = sum(TotalPrice),
    # Additional information
    First_Purchase = min(InvoiceDate),
    Last_Purchase = max(InvoiceDate)
  )

# 5. Create RFM scores
# Define quantiles for scoring
quantile_recency <- quantile(rfm_data$Recency, probs = c(0.2, 0.4, 0.6, 0.8))
quantile_frequency <- quantile(rfm_data$Frequency, probs = c(0.2, 0.4, 0.6, 0.8))
quantile_monetary <- quantile(rfm_data$Monetary, probs = c(0.2, 0.4, 0.6, 0.8))

# Score each dimension from 1-5 (5 being best)
rfm_data <- rfm_data %>%
  mutate(
    # Recency score (lower is better, so reverse scoring)
    R_Score = case_when(
      Recency <= quantile_recency[1] ~ 5,
      Recency <= quantile_recency[2] ~ 4,
      Recency <= quantile_recency[3] ~ 3,
      Recency <= quantile_recency[4] ~ 2,
      TRUE ~ 1
    ),
    # Frequency score (higher is better)
    F_Score = case_when(
      Frequency >= quantile_frequency[4] ~ 5,
      Frequency >= quantile_frequency[3] ~ 4,
      Frequency >= quantile_frequency[2] ~ 3,
      Frequency >= quantile_frequency[1] ~ 2,
      TRUE ~ 1
    ),
    # Monetary score (higher is better)
    M_Score = case_when(
      Monetary >= quantile_monetary[4] ~ 5,
      Monetary >= quantile_monetary[3] ~ 4,
      Monetary >= quantile_monetary[2] ~ 3,
      Monetary >= quantile_monetary[1] ~ 2,
      TRUE ~ 1
    )
  )

# Calculate RFM combined score
rfm_data <- rfm_data %>%
  mutate(
    RFM_Score = R_Score + F_Score + M_Score,
    RFM_Category = paste0(R_Score, F_Score, M_Score)  # Combined as string
  )

# 6. Define customer segments based on RFM scores
rfm_data <- rfm_data %>%
  mutate(
    Segment = case_when(
      RFM_Score >= 13 ~ "Champions",
      R_Score >= 4 & F_Score >= 3 & M_Score >= 3 ~ "Loyal Customers",
      R_Score >= 3 & F_Score >= 1 & M_Score >= 2 ~ "Potential Loyalists",
      R_Score >= 3 & F_Score >= 3 & M_Score >= 3 ~ "Recent Customers",
      R_Score <= 2 & F_Score >= 3 & M_Score >= 3 ~ "Needing Attention",
      R_Score <= 2 & F_Score >= 2 & M_Score >= 2 ~ "At Risk",
      R_Score <= 2 & F_Score <= 2 & M_Score <= 2 ~ "Hibernating",
      TRUE ~ "Others"
    )
  )

# 7. Basic Analysis and Visualizations

# Summary statistics by segment
segment_summary <- rfm_data %>%
  group_by(Segment) %>%
  summarize(
    Count = n(),
    Count_Percent = round(n() / nrow(rfm_data) * 100, 2),
    Avg_Recency = round(mean(Recency), 2),
    Avg_Frequency = round(mean(Frequency), 2),
    Avg_Monetary = round(mean(Monetary), 2),
    Avg_RFM_Score = round(mean(RFM_Score), 2)
  ) %>%
  arrange(desc(Count))

# RFM Distribution Visualization
r_dist <- ggplot(rfm_data, aes(x = R_Score)) + 
  geom_bar(fill = "cornflowerblue") +
  labs(title = "Recency Score Distribution",
       x = "Recency Score", y = "Number of Customers")

f_dist <- ggplot(rfm_data, aes(x = F_Score)) + 
  geom_bar(fill = "coral") +
  labs(title = "Frequency Score Distribution",
       x = "Frequency Score", y = "Number of Customers")

m_dist <- ggplot(rfm_data, aes(x = M_Score)) + 
  geom_bar(fill = "forestgreen") +
  labs(title = "Monetary Score Distribution",
       x = "Monetary Score", y = "Number of Customers")

# Customer Segments Visualization
segment_plot <- ggplot(rfm_data, aes(x = Segment, fill = Segment)) +
  geom_bar() +
  coord_flip() +
  labs(title = "Customer Segments Distribution", 
       x = "", y = "Number of Customers") +
  theme(legend.position = "none")

# Visualization of RFM metrics by Segment
segment_metrics <- rfm_data %>%
  select(Segment, Recency, Frequency, Monetary) %>%
  group_by(Segment) %>%
  summarize(
    Avg_Recency = mean(Recency),
    Avg_Frequency = mean(Frequency),
    Avg_Monetary = mean(Monetary)
  ) %>%
  gather(key = "Metric", value = "Value", -Segment)

metrics_plot <- ggplot(segment_metrics, aes(x = Segment, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Metric, scales = "free_y") +
  coord_flip() +
  labs(title = "Average RFM Metrics by Customer Segment",
       x = "", y = "Average Value") +
  theme(legend.position = "none")

# Display top 10 customers by RFM score
top_customers <- rfm_data %>%
  arrange(desc(RFM_Score), desc(Monetary)) %>%
  select(CustomerID, RFM_Score, R_Score, F_Score, M_Score, Recency, 
         Frequency, Monetary, Segment) %>%
  head(10)

# Export results
write.csv(rfm_data, "rfm_analysis_results.csv", row.names = FALSE)
write.csv(segment_summary, "customer_segment_summary.csv", row.names = FALSE)

# 8. Print the results
print(paste("Analysis date:", analysis_date))
print(paste("Total customers analyzed:", nrow(rfm_data)))
print("Quantiles used for scoring:")
print(paste("Recency:", paste(round(quantile_recency, 2), collapse = ", ")))
print(paste("Frequency:", paste(round(quantile_frequency, 2), collapse = ", ")))
print(paste("Monetary:", paste(round(quantile_monetary, 2), collapse = ", ")))

print("Segment Summary:")
print(segment_summary)

print("Top 10 Customers by RFM Score:")
print(top_customers)

# Display plots
# print(r_dist)
# print(f_dist)
# print(m_dist)
# print(segment_plot)
# print(metrics_plot)