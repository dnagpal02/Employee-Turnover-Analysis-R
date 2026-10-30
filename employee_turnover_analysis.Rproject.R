install.packages(c("tidyverse", "janitor", "skimr", "broom"))
library(tidyverse)
library(janitor)
library(skimr)

# Import data
hr <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv") |>
  clean_names()

# Explore the data
glimpse(hr)
skim(hr)

# Check missing values
colSums(is.na(hr))

# Check duplicate rows
sum(duplicated(hr))

# Overall attrition rate
hr |>
  summarise(
    total_employees = n(),
    employees_left = sum(attrition == "Yes"),
    turnover_rate = mean(attrition == "Yes") * 100
  )

# Attrition rate by department
hr |>
  group_by(department) |>
  summarise(
    employees = n(),
    left = sum(attrition == "Yes"),
    turnover_rate = mean(attrition == "Yes") * 100,
    .groups = "drop"
  ) |>
  ggplot(aes(x = department, y = turnover_rate)) +
  geom_col(fill="pink") +
  labs(
    title = "Employee Turnover Rate by Department",
    x = "Department",
    y = "Turnover Rate (%)"
  ) +
  theme_minimal()

hr |>
  group_by(over_time) |>
  summarise(
    total_employees = n(),
    employees_left = sum(attrition == "Yes"),
    turnover_rate = mean(attrition == "Yes")*100,
    .groups = "drop"
  )
hr |>
  group_by(over_time) |>
  summarise(
    turnover_rate = mean(attrition == "Yes") * 100,
    .groups = "drop"
  ) |>
  ggplot(aes(x = over_time, y = turnover_rate)) +
  geom_col(fill = "steelblue") +
  labs(
    title = "Employee Turnover by Overtime",
    colour = "Blue",
    x = "Works Overtime",
    y = "Turnover Rate (%)"
  ) +
  theme_minimal()

hr |>
  group_by(attrition) |>
  summarise(
    average_income = mean(monthly_income),
    median_income = median(monthly_income),
    .groups = "drop"
  )

ggplot(hr, aes(x = attrition, y = monthly_income)) +
  geom_boxplot() +
  labs(
    title = "Monthly Income by Employee Attrition",
    x = "Attrition",
    y = "Monthly Income"
  ) +
  theme_minimal()

hr |>
  group_by(job_satisfaction) |>
  summarise(
    total_employees = n(),
    employees_left = sum(attrition == "Yes"),
    turnover_rate = mean(attrition == "Yes") * 100,
    .groups = "drop"
  )

hr |>
  group_by(job_satisfaction) |>
  summarise(
    turnover_rate = mean(attrition == "Yes") * 100,
    .groups = "drop"
  ) |>
  ggplot(aes(
    x = factor(job_satisfaction),
    y = turnover_rate
  )) +
  geom_col(fill = "red") +
  labs(
    title = "Employee Turnover by Job Satisfaction",
    x = "Job Satisfaction (1–4)",
    y = "Turnover Rate (%)"
  ) +
  theme_minimal()

hr |>
  group_by(attrition) |>
  summarise(
    average_years = mean(years_at_company),
    median_years = median(years_at_company),
    .groups = "drop"
  )

ggplot(hr, aes(x = attrition, y = years_at_company)) +
  geom_boxplot() +
  labs(
    title = "Years at Company by Employee Attrition",
    x = "Attrition",
    y = "Years at Company"
  ) +
  theme_minimal()