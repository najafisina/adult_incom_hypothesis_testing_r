########################################
# 1) education_num_plot.pdf
########################################

library(dplyr)
library(ggplot2)
library(scales)

# Read data
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# Create readable income group
data <- data %>%
  mutate(
    IncomeGroup = ifelse(salary == ">50K", "High Income", "Low Income"),
    IncomeGroup = factor(IncomeGroup, levels = c("Low Income", "High Income"))
  )

# Frequency table by education.num and income group
df_plot <- data %>%
  count(education.num, IncomeGroup, name = "count")

# Plot
p <- ggplot(
  df_plot,
  aes(
    x    = factor(education.num),
    y    = count,
    fill = IncomeGroup
  )
) +
  geom_col(
    position = position_dodge(width = 0.9),
    width    = 0.9,
    color    = "black"
  ) +
  scale_fill_manual(
    values = c("Low Income" = "#4C78A8",
               "High Income" = "#E74C3C")
  ) +
  labs(
    x    = "Education Number",
    y    = "Count",
    fill = "Income Group"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 9),
    axis.title         = element_text(face = "bold"),
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 8),
    plot.margin        = margin(t = 10, r = 20, b = 60, l = 60),
    panel.grid.major.x = element_blank(),  # remove vertical grid lines
    panel.grid.minor.x = element_blank()
  )

print(p)

ggsave(
  "education_num_plot.pdf",
  plot   = p,
  width  = 9,
  height = 5.5,
  dpi    = 300
)


########################################
# 2) education_income_side_by_side.pdf
########################################

library(dplyr)
library(ggplot2)
library(scales)

# Read data
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# Income group
data <- data %>%
  mutate(
    IncomeGroup = ifelse(salary == ">50K", "High Income", "Low Income"),
    IncomeGroup = factor(IncomeGroup, levels = c("Low Income", "High Income"))
  )

# Education order
edu_order <- c(
  "Preschool", "1st-4th", "5th-6th", "7th-8th",
  "9th", "10th", "11th", "12th",
  "HS-grad", "Some-college", "Assoc-acdm", "Assoc-voc",
  "Bachelors", "Masters", "Prof-school", "Doctorate"
)
data$education <- factor(data$education, levels = edu_order)

# Table of counts and percentages
df_bar <- data %>%
  count(education, IncomeGroup) %>%
  mutate(pct_total = n / sum(n))

# Plot
p <- ggplot(
  df_bar,
  aes(
    x    = education,
    y    = n,
    fill = IncomeGroup
  )
) +
  geom_col(
    position = position_dodge(width = 0.9),
    width    = 0.9,
    color    = "black"
  ) +
  geom_text(
    aes(label = percent(pct_total, accuracy = 0.1)),
    position = position_dodge(width = 0.9),
    vjust    = -0.4,
    size     = 2.0,
    color    = "black",
    alpha    = 0.85
  ) +
  scale_fill_manual(
    values = c("Low Income" = "#4C78A8",
               "High Income" = "#E74C3C")
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    x    = "Education Level",
    y    = "Count",
    fill = "Income Group"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 8.5),
    axis.title         = element_text(face = "bold"),
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 8),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p)

ggsave(
  "education_income_side_by_side.pdf",
  plot   = p,
  width  = 11,
  height = 5,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 3) mean_hours_by_age_income.pdf
########################################

library(dplyr)
library(ggplot2)

# Read data
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# Income group
data <- data %>%
  mutate(
    income_group = ifelse(salary == ">50K", "Income > 50K", "Income <= 50K"),
    income_group = factor(income_group, levels = c("Income <= 50K", "Income > 50K"))
  )

# Mean weekly hours by age and income group
summary_df <- data %>%
  group_by(age, income_group) %>%
  summarise(mean_hours = mean(hours.per.week, na.rm = TRUE), .groups = "drop")

# Plot
p <- ggplot(
  summary_df,
  aes(
    x     = age,
    y     = mean_hours,
    color = income_group
  )
) +
  geom_line(size = 0.7) +
  scale_color_manual(
    values = c(
      "Income <= 50K" = "#2C3E50",
      "Income > 50K"  = "#E74C3C"
    )
  ) +
  labs(
    x     = "Age",
    y     = "Average Hours Worked per Week",
    color = "Income Group",
    title = "Mean Weekly Working Hours Across Age for Income Groups"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position   = "top",
    legend.title      = element_text(size = 11),
    legend.text       = element_text(size = 10),
    plot.title        = element_text(face = "bold", hjust = 0.5),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank()
  )

print(p)

ggsave(
  "mean_hours_by_age_income.pdf",
  plot   = p,
  width  = 8,
  height = 5,
  device = cairo_pdf
)


########################################
# 4) occupation_customerType_stacked_bar.pdf
########################################

library(dplyr)
library(ggplot2)
library(scales)

# Read data
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# Customer type
data <- data %>%
  mutate(
    CustomerType = ifelse(salary == ">50K", "Potential Customer", "Non-Potential Customer"),
    CustomerType = factor(
      CustomerType,
      levels = c("Non-Potential Customer", "Potential Customer")
    )
  )

# Counts and percentages
df_bar <- data %>%
  count(occupation, CustomerType) %>%
  mutate(pct_total = n / sum(n))

# Order occupations by total count
occ_order <- df_bar %>%
  group_by(occupation) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(occupation)

df_bar$occupation <- factor(df_bar$occupation, levels = occ_order)

# Plot
p <- ggplot(df_bar, aes(x = occupation, y = n, fill = CustomerType)) +
  geom_col() +
  geom_text(
    aes(
      y     = n,
      label = percent(pct_total, accuracy = 0.1)
    ),
    position = position_stack(vjust = 0.5),
    size     = 2.3,
    color    = "black",
    alpha    = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Non-Potential Customer" = "#4C78A8",
      "Potential Customer"     = "#F58518"
    )
  ) +
  labs(
    x    = "Occupation",
    y    = "Count",
    fill = "Customer Type"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x       = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    legend.position   = c(0.78, 0.8),   # legend inside plot
    legend.direction  = "vertical",
    legend.title      = element_text(size = 9),
    legend.text       = element_text(size = 8),
    legend.key.size   = unit(0.4, "lines"),
    legend.spacing.y  = unit(0.15, "lines"),
    legend.box.spacing = unit(0.15, "lines"),
    legend.background = element_rect(fill = "white", color = NA, alpha = 0.8)
  )

print(p)

ggsave(
  "occupation_customerType_stacked_bar.pdf",
  plot   = p,
  width  = 10,
  height = 5,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 5) age_vs_pot_boxplot.pdf
########################################

library(ggplot2)
library(dplyr)

# Read data
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# Potential vs Non-potential
data <- data %>%
  mutate(
    pot = ifelse(salary == ">50K", "Potential", "Non-Potential"),
    pot = factor(pot, levels = c("Non-Potential", "Potential"))
  )

# Summary stats for label box
stats <- data %>%
  group_by(pot) %>%
  summarise(
    N           = n(),
    Q1          = quantile(age, 0.25),
    Med         = median(age),
    Q3          = quantile(age, 0.75),
    IQR         = Q3 - Q1,
    lower_fence = Q1 - 1.5 * IQR,
    upper_fence = Q3 + 1.5 * IQR,
    Min_no_out  = min(age[age >= lower_fence]),
    Max_no_out  = max(age[age <= upper_fence]),
    .groups     = "drop"
  ) %>%
  mutate(
    label = sprintf(
      "Min (no out.): %d\n25%%: %d\n50%%: %d\n75%%: %d\nMax (no out.): %d\nN: %d",
      round(Min_no_out), round(Q1), round(Med),
      round(Q3), round(Max_no_out), N
    ),
    label_x = as.numeric(pot) + 0.28,           # slightly to the right of box center
    label_y = Q3 + (Q3 - Q1) * 0.25             # above the box
  )

# Plot
p <- ggplot(data, aes(x = pot, y = age, fill = pot)) +
  geom_boxplot(
    outlier.shape = 16,
    outlier.size  = 1.6,
    outlier.alpha = 0.8
  ) +
  geom_label(
    data        = stats,
    inherit.aes = FALSE,
    aes(x = label_x, y = label_y, label = label),
    size       = 3,
    label.size = 0.25,
    label.r    = unit(0.15, "lines"),
    fill       = "white",
    alpha      = 0.9
  ) +
  scale_fill_manual(
    values = c(
      "Non-Potential" = "#66C2A5",
      "Potential"     = "#FC8D62"
    )
  ) +
  labs(
    x = "Customer Type",
    y = "Age"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.title.x       = element_text(face = "bold"),
    axis.title.y       = element_text(face = "bold"),
    legend.position    = "none"
  )

print(p)

ggsave(
  "age_vs_pot_boxplot.pdf",
  plot   = p,
  width  = 7,
  height = 4.5,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 6) sex_customerType_stacked_bar.pdf
########################################

library(dplyr)
library(ggplot2)
library(scales)

# Read data
data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# CustomerType and ordered sex
data <- data %>%
  mutate(
    CustomerType = ifelse(salary == ">50K", "Potential Customer", "Non-Potential Customer"),
    CustomerType = factor(
      CustomerType,
      levels = c("Non-Potential Customer", "Potential Customer")
    ),
    sex = factor(sex, levels = c("Male", "Female"))
  )

# Percentage table
df_bar <- data %>%
  count(sex, CustomerType) %>%
  mutate(pct_total = n / sum(n))

# Plot
p <- ggplot(df_bar, aes(x = sex, y = n, fill = CustomerType)) +
  geom_col() +
  geom_text(
    aes(
      label = percent(pct_total, accuracy = 0.1),
      y     = n
    ),
    position = position_stack(vjust = 0.5),
    color    = "black",
    size     = 4,
    alpha    = 0.7
  ) +
  labs(
    x    = "Sex",
    y    = "Count",
    fill = "Customer Type"
  ) +
  scale_fill_manual(values = c("#6EA8FF", "#FF6A6A")) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position    = c(0.78, 0.8),
    legend.direction   = "vertical",
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9),
    legend.key.size    = unit(0.5, "lines"),
    legend.background  = element_rect(fill = "white", color = NA, alpha = 0.8)
  )

print(p)

ggsave(
  "sex_customerType_stacked_bar.pdf",
  plot   = p,
  width  = 7,
  height = 4.5,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 7) histogram_hours_pot.pdf
########################################

library(ggplot2)
library(scales)
library(dplyr)

# Assumes 'data' already read above
data$pot <- ifelse(data$salary == ">50K", "Potential", "Non-Potential")
data$pot <- factor(data$pot, levels = c("Non-Potential", "Potential"))

bins_num <- 20

# Base stacked histogram
p_base <- ggplot(data, aes(x = hours.per.week, fill = pot)) +
  geom_histogram(
    bins     = bins_num,
    position = "stack",
    alpha    = 0.6,
    color    = NA
  )

# Extract bin data
b     <- ggplot_build(p_base)
layer <- b$data[[1]]
N     <- nrow(data)

layer$pot <- levels(data$pot)[layer$group]

top_bins <- subset(layer, pot == "Potential" & count > 0)
top_bins$percent <- top_bins$count / N

# Add percentage labels
p <- p_base +
  geom_text(
    data        = top_bins,
    inherit.aes = FALSE,
    aes(
      x     = x,
      y     = y,
      label = ifelse(
        percent < 0.01,
        "1%",                               # minimum shown as 1%
        percent(percent, accuracy = 1)
      )
    ),
    vjust = -0.3,
    size  = 3,
    color = "grey40"
  ) +
  labs(
    x    = "hours-per-week",
    y    = "Count",
    fill = "Customer Type"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position    = c(0.8, 0.8),
    legend.direction   = "vertical",
    legend.background  = element_rect(fill = "white", alpha = 0.8),
    legend.key.size    = unit(0.5, "lines"),
    legend.title       = element_text(size = 10),
    legend.text        = element_text(size = 9)
  )

print(p)

ggsave(
  "histogram_hours_pot.pdf",
  plot   = p,
  width  = 8,
  height = 6,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 8) high_income_by_education_gender.pdf
########################################

library(dplyr)
library(ggplot2)

df <- read.csv("HW1.csv")

# High income indicator
df$income_high <- ifelse(df$salary == ">50K", 1, 0)

# Proportion of high income by education x sex
edu_gender <- df %>%
  group_by(education, sex) %>%
  summarise(
    prop_high = mean(income_high),
    n         = n(),
    .groups   = "drop"
  )

# Bar plot
p <- ggplot(edu_gender, aes(x = education, y = prop_high, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    x     = "Education Level",
    y     = "Proportion of High Income",
    title = "High Income Rate by Education Level and Gender",
    fill  = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(p)

ggsave(
  "high_income_by_education_gender.pdf",
  plot   = p,
  width  = 10,
  height = 5.5,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 9) education_income_effect.pdf
########################################

library(dplyr)
library(ggplot2)

df <- read.csv("HW1.csv")
df$income_high <- ifelse(df$salary == ">50K", 1, 0)

# Overall high-income proportion by education
edu_overall <- df %>%
  group_by(education) %>%
  summarise(
    prop_high = mean(income_high),
    n         = n(),
    .groups   = "drop"
  )

p <- ggplot(edu_overall, aes(x = education, y = prop_high)) +
  geom_bar(stat = "identity", fill = "#4C78A8") +
  labs(
    x     = "Education Level",
    y     = "Proportion of High Income",
    title = "Effect of Education on Income (Overall)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )

print(p)

ggsave(
  "education_income_effect.pdf",
  plot   = p,
  width  = 10,
  height = 5,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# 10) education_income_gender_lineplot.pdf
########################################

library(dplyr)
library(ggplot2)

# Reuse edu_gender from above block
p <- ggplot(
  edu_gender,
  aes(
    x     = education,
    y     = prop_high,
    color = sex,
    group = sex
  )
) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    x     = "Education Level",
    y     = "High Income Probability",
    title = "Effect of Education on Income for Men and Women",
    color = "Sex"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x        = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(),
    panel.grid.minor.y = element_line()
  )

print(p)

ggsave(
  "education_income_gender_lineplot.pdf",
  plot   = p,
  width  = 10,
  height = 5,
  dpi    = 300,
  device = cairo_pdf
)

