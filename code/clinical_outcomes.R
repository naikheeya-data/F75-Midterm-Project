# Clinical Outcomes Analysis
# Aniya George

# Load Packages
library(tidyverse)
library(gtsummary)

# Load Dataset
f75CO <- read.csv("data/f75_interim.csv")

# Identify Treatment Groups
table(f75CO$arm, useNA = "ifany")

# Read configuration
config <- yaml::read_yaml("config.yml")

# Select treatment group
if (config$treatment_group == "both") {
  f75CO1 <- f75CO
} else if (config$treatment_group == "modified") {
  f75CO1 <- f75CO %>%
    filter(arm == "Modified F75 (intervention)")
} else if (config$treatment_group == "standard") {
  f75CO1 <- f75CO %>%
    filter(arm == "Standard F75")
}

# Examine Time-to-Discharge and Reason for Discharge
table(f75CO$discharged2, useNA = "ifany")

table(f75CO$withdraw2, useNA = "ifany")

# Examine Clinical Stability
summary(f75CO$days_stable)

table(f75CO$days_stable == 999, useNA = "ifany") # [59 people died]

# Creating Summary Table
if (config$treatment_group == "both") {
  
  summary_table <- f75CO1 %>%
    mutate(
      days_stable2 = na_if(as.numeric(as.character(days_stable)), 999)
    ) %>%
    select(arm, days_stable2, discharged2, withdraw2) %>%
    tbl_summary(
      by = arm,
      label = list(
        days_stable2 ~ "Time to Clinical Stabilization (Days)",
        discharged2 ~ "Discharged as Planned",
        withdraw2 ~ "Reason for Not Being Discharged as Planned"
      ),
      statistic = list(
        days_stable2 ~ "{median} ({p25}, {p75})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      missing = "no"
    ) %>%
    add_overall()
  
} else {
  
  summary_table <- f75CO1 %>%
    mutate(
      days_stable2 = na_if(as.numeric(as.character(days_stable)), 999)
    ) %>%
    select(days_stable2, discharged2, withdraw2) %>%
    tbl_summary(
      label = list(
        days_stable2 ~ "Time to Clinical Stabilization (Days)",
        discharged2 ~ "Discharged as Planned",
        withdraw2 ~ "Reason for Not Being Discharged as Planned"
      ),
      statistic = list(
        days_stable2 ~ "{median} ({p25}, {p75})",
        all_categorical() ~ "{n} ({p}%)"
      ),
      missing = "no"
    )
  
}

# Saving Summary Table as RDS
saveRDS(
  summary_table,
  file = here::here("outputs", "summary_table.rds")
)

# Creating Bar Chart
discharge_plot <- f75CO1 %>%
  filter(!is.na(discharged2)) %>%
  count(arm, discharged2) %>%
  group_by(arm) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = arm, y = percentage, fill = discharged2)) +
  geom_col(position = "dodge") +
  labs(
    title = "Discharge Outcomes by Treatment Group",
    x = "Treatment Group",
    y = "Percentage (%)",
    fill = "Discharge Outcome"
  ) +
  theme_minimal()

# Saving Bar Chart as PNG
ggsave(
  here::here("outputs/discharge_plot.png"),
  plot = discharge_plot,
  device = "png"
)
