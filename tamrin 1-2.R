###############################
# Packages
###############################
library(dplyr)
library(ggplot2)
library(scales)

###############################
# Read data and build years_of_education
###############################

data <- read.csv("HW1.csv", stringsAsFactors = TRUE)

# Map education levels to years_of_education
edu_mapping <- c(
  "Preschool"    = 1,
  "1st-4th"      = 3,
  "5th-6th"      = 4.5,
  "7th-8th"      = 6,
  "9th"          = 7,
  "10th"         = 8,
  "11th"         = 9,
  "12th"         = 10,
  "HS-grad"      = 10,
  "Some-college" = 12,
  "Assoc-acdm"   = 14,
  "Assoc-voc"    = 14,
  "Bachelors"    = 18,
  "Masters"      = 20,
  "Prof-school"  = 23,
  "Doctorate"    = 28
)

data <- data %>%
  mutate(
    years_of_education = edu_mapping[as.character(education)]
  )




###############################
# Part (a): One-sample t-test
###############################

high_income <- subset(data, salary == ">50K")

t_test_a <- t.test(
  high_income$years_of_education,
  mu = 17,
  alternative = "greater"
)

print(t_test_a)

# Histogram for part (a)
mean_years <- mean(high_income$years_of_education, na.rm = TRUE)

p_a <- ggplot(high_income, aes(x = years_of_education)) +
  geom_histogram(
    binwidth = 1,
    fill  = "#4C78A8",
    color = "white",
    alpha = 0.85
  ) +
  geom_vline(
    xintercept = mean_years,
    color = "#E74C3C",
    size  = 1.2
  ) +
  geom_vline(
    xintercept = 17,
    color    = "#F39C12",
    size     = 1.2,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribution of Years of Education among High-Income Individuals (>50K)",
    x = "Years of Education",
    y = "Frequency"
  ) +
  annotate(
    "text",
    x     = mean_years + 0.3,
    y     = max(table(high_income$years_of_education)) * 0.9,
    label = paste0("Sample Mean = ", round(mean_years, 2)),
    color = "#E74C3C",
    hjust = 0,
    size  = 4
  ) +
  annotate(
    "text",
    x     = 17.3,
    y     = max(table(high_income$years_of_education)) * 0.75,
    label = "Hypothesized Mean = 17",
    color = "#F39C12",
    hjust = 0,
    size  = 4
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey85", size = 0.4),
    panel.grid.minor.y = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_a)

ggsave(
  "years_of_education_high_income_ttest.pdf",
  plot   = p_a,
  width  = 10,
  height = 6,
  dpi    = 300,
  device = cairo_pdf
)


########################################
# Part (b): Two-sample t-test (Welch)
########################################

high_income <- subset(data, salary == ">50K")
low_income  <- subset(data, salary == "<=50K")

t_test_b <- t.test(
  high_income$years_of_education,
  low_income$years_of_education,
  alternative = "greater",
  var.equal   = FALSE
)

print(t_test_b)

# Boxplot for part (b)
p_b <- ggplot(data, aes(x = salary, y = years_of_education, fill = salary)) +
  geom_boxplot(alpha = 0.8) +
  scale_fill_manual(values = c("<=50K" = "#4C78A8", ">50K" = "#E74C3C")) +
  labs(
    title = "Comparison of Education Years Between Income Groups",
    x = "Income Group",
    y = "Years of Education"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_b)

ggsave(
  "boxplot_education_vs_income.pdf",
  plot   = p_b,
  width  = 9,
  height = 6,
  dpi    = 300,
  device = cairo_pdf
)


#######################################
# Part (c): Two-proportion test
#######################################

data <- data %>%
  mutate(edu_group = ifelse(years_of_education > 16, "HighEdu", "LowEdu"))

tab <- table(data$edu_group, data$salary)
print(tab)

prop_test_c <- prop.test(
  x = c(tab["HighEdu", ">50K"], tab["LowEdu", ">50K"]),
  n = c(sum(tab["HighEdu", ]),   sum(tab["LowEdu", ])),
  alternative = "greater",
  correct = FALSE
)

print(prop_test_c)

# Bar plot for part (c)
plot_data_c <- data %>%
  group_by(edu_group) %>%
  summarise(
    high_income_rate = mean(salary == ">50K"),
    .groups = "drop"
  )

p_c <- ggplot(plot_data_c, aes(x = edu_group, y = high_income_rate, fill = edu_group)) +
  geom_col(width = 0.6, alpha = 0.85) +
  scale_fill_manual(values = c("HighEdu" = "#4C78A8", "LowEdu" = "#E74C3C")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  geom_text(
    aes(label = percent(high_income_rate, accuracy = 0.1)),
    vjust = -0.5,
    size  = 4
  ) +
  labs(
    title = "Percentage of High-Income Individuals by Education Group",
    x = "Education Group",
    y = "High-Income Percentage"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title      = element_text(face = "bold", hjust = 0.5)
  )

print(p_c)

ggsave(
  filename = "HighIncome_by_EducationGroup.pdf",
  plot     = p_c,
  width    = 9,
  height   = 6,
  dpi      = 300,
  device   = cairo_pdf
)


#######################################
# Part (d): Correlation (education.num vs years_of_education)
#######################################

ok <- complete.cases(data$education.num, data$years_of_education)

cor_test_d <- cor.test(
  data$education.num[ok],
  data$years_of_education[ok],
  method = "pearson"
)

print(cor_test_d)

p_d <- ggplot(data, aes(x = education.num, y = years_of_education)) +
  geom_point(alpha = 0.3, color = "#2C3E50") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Relationship Between education-num and Years of Education") +
  xlab("Education-num") +
  ylab("Years of Education") +
  theme_minimal()

print(p_d)
