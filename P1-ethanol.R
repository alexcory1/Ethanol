# Author:  Alex Cory
# Date:    2025-05-05
# Purpose: Ethanol Analysis
#-------------------------------------------------------------------------------

# Load packages
suppressPackageStartupMessages(library("tidyverse")); theme_set(theme_bw())
library("knitr")


data <- read_csv("gas_mileage_data.csv", show_col_types = FALSE)

#Convert ethanol to a factor with labels
data <- data %>%
  filter(!is.na(ethanol), !is.na(mpg)) %>%
  mutate(ethanol = factor(ethanol, levels = c(0,10), labels = c("No", "Yes")))

#Summary statistics
summary_stats <- data %>%
  summarise(n = n(),
            mean_mpg = mean(mpg),
            sd_mpg = sd(mpg))

#Table of ethanol groups
ethanol_counts <- table(data$ethanol)

#MPG summary by group
mpg_by_ethanol <- data %>%
  group_by(ethanol) %>%
  summarise(n = n(),
            mean_mpg = mean(mpg),
            sd_mpg = sd(mpg))

#Plot
ggplot(data, aes(x = ethanol, y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Ethanol in Gasoline (Yes/No)",
       y = "Miles per Gallon",
       title = "Gas Mileage by Ethanol Content")

model <- lm(mpg ~ ethanol, data = data)

#Model summary
model_summary <- summary(model)

#Confidence interval
ci <- confint(model)

#Plot with model fit
ggplot(data, aes(x = ethanol, y = mpg)) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") +
  labs(
    x = "Ethanol in Gasoline",
    y = "Miles per Gallon",
    title = "Model Estimates for 95% Confidence Intervals"
  )
