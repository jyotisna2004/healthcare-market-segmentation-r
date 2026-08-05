

# Load required libraries
library(tidyverse)
library(ggplot2)


# 1. LOAD OR GENERATE DATA

# For real intern data, replace this section with:
# df <- read.csv('your_healthcare_survey_data.csv')

set.seed(42)
n_samples <- 600



df <- data.frame(
  Age = sample(18:80, n_samples, replace = TRUE),
  Annual_Income_kUSD = pmax(20, pmin(160, rnorm(n_samples, mean = 65, sd = 22))),
  Health_Consciousness_Score = sample(1:10, n_samples, replace = TRUE),
  Annual_Healthcare_Spend_USD = pmax(400, pmin(9500, rnorm(n_samples, mean = 2800, sd = 1200))),
  Telehealth_Usage_Freq = sample(0:11, n_samples, replace = TRUE)
)

print("--- Dataset Sample ---")
print(head(df))


# 2. EXPLORATORY DATA ANALYSIS (EDA)

# Select numeric columns for correlation analysis
numeric_cols <- df %>% 
  select(Age, Annual_Income_kUSD, Health_Consciousness_Score, 
         Annual_Healthcare_Spend_USD, Telehealth_Usage_Freq)

cor_matrix <- cor(numeric_cols)
print("\n--- Correlation Matrix ---")
print(round(cor_matrix, 2))


# 3. MARKET SEGMENTATION (K-MEANS CLUSTERING)

# Select key variables for building consumer segments
features <- c("Age", "Annual_Income_kUSD", "Health_Consciousness_Score", "Annual_Healthcare_Spend_USD")

# Standardize features so high-scale values don't dominate the model
X_scaled <- scale(df[, features])

# Apply K-Means clustering to group consumers into 3 distinct segments
set.seed(42)
kmeans_result <- kmeans(X_scaled, centers = 3, nstart = 25)

# Append segment assignments back to the main dataframe
df$Market_Segment <- as.factor(kmeans_result$cluster)


# 4. SEGMENT ANALYSIS & PROFILING

# Calculate average metrics and size for each market segment
segment_summary <- df %>%
  group_by(Market_Segment) %>%
  summarise(
    Segment_Size = n(),
    Mean_Age = round(mean(Age), 1),
    Mean_Income_kUSD = round(mean(Annual_Income_kUSD), 1),
    Mean_Health_Score = round(mean(Health_Consciousness_Score), 1),
    Mean_Spend_USD = round(mean(Annual_Healthcare_Spend_USD), 1)
  )

print("\n--- Market Segment Profiles ---")
print(segment_summary)

# 5. VISUALIZATION

# Scatter plot mapping Income vs Healthcare Spend across segments
ggplot(df, aes(x = Annual_Income_kUSD, y = Annual_Healthcare_Spend_USD, color = Market_Segment)) +
  geom_point(alpha = 0.8, size = 3) +
  labs(
    title = "Healthcare Consumer Market Segments",
    subtitle = "Annual Income vs. Healthcare Spending",
    x = "Annual Income ($k)",
    y = "Annual Healthcare Spend ($)",
    color = "Market Segment"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

ggsave("healthcare_market_segments.png", width = 8, height = 6, dpi = 300)
