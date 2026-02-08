library(dplyr)
library(ggplot2)
library(scales)

########################################
# Read data and create high-income flag
########################################

data <- read.csv("HW1_updated.csv", stringsAsFactors = TRUE)

data <- data %>%
  mutate(
    income_high = ifelse(salary == ">50K", 1, 0)
  )

########################################
# 1) Overall gender effect on high income
########################################

overall <- data %>%
  group_by(sex) %>%
  summarise(
    high_rate = mean(income_high),
    n         = n(),
    .groups   = "drop"
  )

cat("### Overall high-income rate by sex:\n")
print(overall)

overall_diff <- overall$high_rate[overall$sex == "Male"] -
  overall$high_rate[overall$sex == "Female"]
overall_sign <- sign(overall_diff)

cat("\nOverall (Male - Female) difference:", overall_diff, "\n")
cat("Overall direction (sign):", overall_sign, "\n\n")

########################################
# 2) Loop over occupations (Simpson-like check)
########################################

occupations   <- unique(data$occupation)
paradox_list  <- list()
idx           <- 1

for (occ in occupations) {
  
  df_occ <- subset(data, occupation == occ)
  
  # Skip very small occupation groups
  if (nrow(df_occ) < 30) next
  
  stats_occ <- df_occ %>%
    group_by(sex) %>%
    summarise(
      high_rate = mean(income_high),
      n         = n(),
      .groups   = "drop"
    )
  
  # Need both genders for comparison
  if (nrow(stats_occ) < 2) next
  
  # Difference (Male - Female) in this occupation
  diff_occ <- stats_occ$high_rate[stats_occ$sex == "Male"] -
    stats_occ$high_rate[stats_occ$sex == "Female"]
  sign_occ <- sign(diff_occ)
  
  cat("Occupation:", occ, "\n")
  print(stats_occ)
  cat("Difference (Male - Female):", diff_occ, "\n")
  cat("Direction (sign):", sign_occ, "\n\n")
  
  # Simpson-like paradox: local sign opposite to overall sign
  if (sign_occ == -overall_sign) {
    cat(">>> SIMPSON-LIKE PARADOX detected in occupation:", occ, "<<<\n\n")
    paradox_list[[idx]] <- list(
      occupation = occ,
      stats      = stats_occ,
      diff       = diff_occ
    )
    idx <- idx + 1
  }
}


########################################
# 3) Plot high-income rate by sex in paradox occupations
########################################

# Example paradox occupations (from previous step)
paradox_occs <- c("Priv-house-serv", "Other-service")

df_subset <- data %>%
  filter(occupation %in% paradox_occs)

plot_df_multi <- df_subset %>%
  group_by(occupation, sex) %>%
  summarise(
    high_rate = mean(income_high, na.rm = TRUE),
    n         = n(),
    .groups   = "drop"
  )

p <- ggplot(plot_df_multi, aes(x = sex, y = high_rate, fill = sex)) +
  geom_col(alpha = 0.85) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_fill_manual(values = c("Male" = "#4C78A8", "Female" = "#E74C3C")) +
  geom_text(
    aes(label = percent(high_rate, accuracy = 0.1)),
    vjust = -0.5,
    size  = 3.5
  ) +
  facet_wrap(~ occupation) +
  labs(
    title = "High-Income Rate by Sex in Simpson-Paradox Occupations",
    x     = "Sex",
    y     = "High Income Percentage"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title        = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

print(p)

ggsave(
  filename = "Simpson_paradox_occupations.pdf",
  plot     = p,
  width    = 10,
  height   = 6,
  dpi      = 300,
  device   = cairo_pdf
)
