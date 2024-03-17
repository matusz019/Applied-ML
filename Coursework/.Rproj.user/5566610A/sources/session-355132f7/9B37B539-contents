# Load the training dataset
train <- read.csv("concrete_strength_train.csv")

# Calculate relative frequency
train_freq <- prop.table(table(train$Strength))

# Summary statistics
summary_stats <- summary(train)
mean_value <- mean(train$Strength)
min_value <- min(train$Strength)
max_value <- max(train$Strength)
cov_value <- sd(train$Strength) / mean(train$Strength)  # Coefficient of variation

# Visual exploration of the Strength variable
library(ggplot2)
ggplot(train, aes(x = Strength, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  geom_text(data = data.frame(x = c(0.8 * max(train$Strength)), y = c(0.04),
                              label = paste("Mean:", round(mean_value, 2),
                                            "\nMin:", min_value, "\nMax:",
                                            max_value, "\nCOV:", round(cov_value, 2))),
            aes(x = x, y = y, label = label),
            color = "red", hjust = 0, vjust = 1) +
  labs(title = "Distribution of Concrete Strength in Training Dataset",
       x = "Concrete Strength (MPa)",
       y = "Relative Frequency",
       caption = "Figure 1.1") +
  theme(plot.caption = element_text(hjust = 0))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the testing dataset
test <- read.csv("concrete_strength_test.csv")

# Calculate relative frequency
test_freq <- prop.table(table(test$Strength))

# Summary statistics
summary_stats <- summary(test)
mean_value <- mean(test$Strength)
min_value <- min(test$Strength)
max_value <- max(test$Strength)
cov_value <- sd(test$Strength) / mean(test$Strength)  # Coefficient of variation

# Visual exploration of the Strength variable
ggplot(test, aes(x = Strength, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  geom_text(data = data.frame(x = c(0.8 * max(test$Strength)), y = c(0.04), label = paste("Mean:", round(mean_value, 2), "\nMin:", min_value, "\nMax:", max_value, "\nCOV:", round(cov_value, 2))),
            aes(x = x, y = y, label = label),
            color = "red", hjust = 0, vjust = 1) +
  labs(title = "Distribution of Concrete Strength in Testing Dataset",
       x = "Concrete Strength (MPa)",
       y = "Relative Frequency",
       caption = "Figure 1.1") +
  theme(plot.caption = element_text(hjust = 0))
