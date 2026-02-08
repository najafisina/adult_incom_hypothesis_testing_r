
library(dplyr)

data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

########## Occupation-level income statistics ##########
occupation_stats <- data %>%
  group_by(occupation) %>%
  summarise(
    n          = n(),
    high_income = sum(salary == ">50K"),
    prop_high   = mean(salary == ">50K")
  ) %>%
  arrange(desc(prop_high))

occupation_stats

########## Create priority groups (1 = lowest, 3 = highest) ##########
occupation_stats <- occupation_stats %>%
  mutate(
    priority = ntile(prop_high, 3)  # 1 = lowest, 3 = highest
  )

########## Merge priority back to main dataset ##########
data_priority <- data %>%
  left_join(occupation_stats %>% select(occupation, priority),
            by = "occupation")

###### ANOVA: income vs priority (classical) ######
anova_result <- aov(as.numeric(salary == ">50K") ~ factor(priority),
                    data = data_priority)

summary(anova_result)

###### Create numeric high-income indicator ######
data_priority <- data_priority %>%
  mutate(high_income_num = ifelse(salary == ">50K", 1, 0))

###### Bartlett test for homogeneity of variances ######
bartlett.test(high_income_num ~ factor(priority), data = data_priority)

######## Welch one-way test (oneway.test) ########
oneway.test(high_income_num ~ factor(priority), data = data_priority)



############################################
# Plot: High-income rate by priority group #
############################################

library(dplyr)
library(ggplot2)
library(scales)

# 1) Read data + priority created from your analysis
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

occupation_stats <- data %>%
  group_by(occupation) %>%
  summarise(
    n          = n(),
    high_income = sum(salary == ">50K"),
    prop_high   = mean(salary == ">50K")
  ) %>%
  arrange(desc(prop_high))

# Create 3 priority groups (1 = low-income jobs, 3 = high-income jobs)
occupation_stats <- occupation_stats %>%
  mutate(priority = ntile(prop_high, 3))

# Merge priority into main dataset and create numeric income indicator
data_priority <- data %>%
  left_join(occupation_stats %>% select(occupation, priority),
            by = "occupation") %>%
  mutate(high_income_num = ifelse(salary == ">50K", 1, 0))

# 2) Compute percentages per priority group
plot_data <- data_priority %>%
  group_by(priority) %>%
  summarise(high_income_rate = mean(high_income_num))

# 3) Plot (professional style)
p <- ggplot(plot_data,
            aes(x = factor(priority),
                y = high_income_rate,
                fill = factor(priority))) +
  geom_col(width = 0.65, alpha = 0.85) +
  # Colors (same as previous style)
  scale_fill_manual(values = c("1" = "#4C78A8",
                               "2" = "#F39C12",
                               "3" = "#E74C3C")) +
  # Percent labels on bars
  geom_text(
    aes(label = scales::percent(high_income_rate, accuracy = 0.1)),
    vjust = -0.5,
    size  = 4,
    color = "black"
  ) +
  scale_y_continuous(
    labels = percent_format(),
    expand = expansion(mult = c(0, 0.08))
  ) +
  labs(
    title = "High-Income Rate Across Occupation Priority Groups",
    x     = "Priority Level (1 = Low Income, 3 = High Income)",
    y     = "High-Income Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position   = "none",
    plot.title        = element_text(face = "bold", hjust = 0.5),
    # Remove vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # Keep soft horizontal lines
    panel.grid.major.y = element_line(color = "grey85", size = 0.4),
    panel.grid.minor.y = element_blank()
  )

print(p)

# 4) Save high-quality PDF
ggsave("priority_high_income_rate.pdf",
       plot   = p,
       width  = 8,
       height = 5,
       dpi    = 300,
       device = cairo_pdf)
