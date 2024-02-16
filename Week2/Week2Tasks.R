#~~~~~~~~~~~Task C~~~~~~~~~~~~
ggplot(data, aes(x = income.label, fill = education)) +
  geom_bar(position = "dodge") +
  labs(x = "Income Label", y = "Count", fill = "Education", title = "Distribution of income label by education") +
  theme_minimal()

#~~~~~~~~~~~~TaskD~~~~~~~~~~~~
# Load your dataset
# For example, if your dataset is a CSV file named "data.csv", you can load it using:
 data <- read.csv("income.csv")

# Calculate the z-scores for the column 'Values'
data$z_scores <- scale(data$capital.gain)

# Set a threshold for identifying outliers (e.g., z-score > 2 or z-score > 3)
threshold <- 2

# Identify outliers based on the threshold
outliers <- data[abs(data$z_scores) > threshold, ]

# Remove outliers from the dataset
data_clean <- data[abs(data$z_scores) <= threshold, ]

# Print the cleaned dataset
print(data_clean)

#~~~~~~~~~~~Task E~~~~~~~~~~~~~
str(data)

#~~~~~~~~~~~Task G~~~~~~~~~~~~~
# Plotting age distribution by income level
ggplot(data, aes(x = age, fill = income.label)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(x = "Age", y = "Frequency", fill = "Income") +
  ggtitle("Age Distribution by Income Level")

# Plotting workclass distribution
ggplot(data, aes(x = workclass, fill = income.label)) +
  geom_bar(position = "stack") +
  labs(x = "Workclass", y = "Count", fill = "Income") +
  ggtitle("Distribution of Income by Workclass")

# Plotting hours worked per week and income level
ggplot(data, aes(x = working.hours, fill = income.label)) +
  geom_density(alpha = 0.5) +
  labs(x = "Hours per Week", y = "Density", fill = "Income") +
  ggtitle("Density Plot of Hours Worked per Week by Income Level")





