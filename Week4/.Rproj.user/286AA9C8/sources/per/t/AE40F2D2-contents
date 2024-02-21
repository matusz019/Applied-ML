# Install and load necessary packages
library(tidyverse)

# Load the dataset
fake_jobs <- read.csv("fake_job_postings.csv", stringsAsFactors = FALSE)


# Filter out the relevant columns where fraudulent is 1
fake_salary <- fake_jobs %>%
  filter(fraudulent == 1) %>%
  filter(trimws(salary_range) != "") %>%
  select(title, fraudulent, salary_range)

