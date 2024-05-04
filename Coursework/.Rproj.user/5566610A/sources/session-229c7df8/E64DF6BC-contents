# Select only predictor variables (exclude the target variable)
predictor_vars <- subset(trainData_clean, select = -c(Strength))

# Compute the correlation matrix
correlation_matrix <- cor(predictor_vars)

# Print the correlation matrix
print(correlation_matrix)