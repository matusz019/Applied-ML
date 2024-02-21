# Load necessary packages
library(tidyverse)

# Load the dataset
jobs <- read.csv("fake_job_postings.csv", stringsAsFactors = FALSE)

# Filter out relevant columns and rows
filtered_jobs <- jobs %>%
  filter(trimws(salary_range) != "") %>%
  mutate(salary_range = gsub("[^0-9.-]", "", salary_range)) %>%
  separate(salary_range, into = c("salary_min", "salary_max"), sep = "-") %>%
  mutate(across(c(salary_min, salary_max), as.numeric)) %>%
  filter(salary_min >= 20000) %>%
  filter(salary_min <= 100000) %>%
  select(fraudulent, salary_min)

# Plotting
ggplot(filtered_jobs, aes(x = factor(fraudulent), y = salary_min, fill = factor(fraudulent))) +
  geom_boxplot() +
  labs(title = "Comparison of Fraudulent and Non-Fraudulent Salaries",
       x = "Fraudulent",
       y = "Minimum Salary",
       fill = "Fraudulent") +
  scale_fill_manual(values = c("blue", "red"), labels = c("Non-Fraudulent", "Fraudulent")) +
  theme_minimal()
  scale_y_continuous(labels = scales::comma)
