# ==============================================================================
# APPLIED STATISTICS (052911) - PROJECT H3: EXPLORATORY DATA ANALYSIS (EDA)
# Builds directly on `data` (or loads "hospital_clean.csv")
# ==============================================================================

eda_pkgs <- c("tidyverse", "patchwork", "scales", "forcats", "car", "FactoMineR", "factoextra", "ggrepel")
new_eda  <- eda_pkgs[!(eda_pkgs %in% installed.packages()[, "Package"])]
if (length(new_eda)) install.packages(new_eda)
invisible(lapply(eda_pkgs, library, character.only = TRUE))

dir.create("plots", showWarnings = FALSE)

# FIXED DATA LOADING BLOCK:
# Checks specifically if 'data' is a data.frame in your workspace (ignoring utils::data)
if (!exists("data", envir = .GlobalEnv) || !is.data.frame(data)) {
  data <- read.csv("data/hospital_clean.csv", stringsAsFactors = FALSE)
  sev_levels <- c("Minor", "Moderate", "Major", "Extreme")
  data <- data %>%
    mutate(
      severity_tier  = factor(severity_tier,  levels = sev_levels),
      mortality_risk = factor(mortality_risk, levels = sev_levels),
      age_group      = factor(age_group,      levels = c("Under 50", "50 to 69", "70 or Older")),
      gender         = factor(gender),
      ed_admit       = factor(ed_admit,       levels = c("No", "Yes")),
      admission_type = factor(admission_type, levels = c("Emergency", "Urgent", "Elective")),
      region         = relevel(factor(region),    ref = "New York City"),
      race           = relevel(factor(race),      ref = "White"),
      ethnicity      = relevel(factor(ethnicity), ref = "Not Span/Hispanic"),
      payer          = factor(payer,          levels = c("Medicare", "Medicaid", "Private", "Other")),
      any_procedure  = factor(any_procedure,  levels = c("No", "Yes")),
      provider_id    = factor(provider_id)
    )
}

# ==============================================================================
# 1. SAMPLE ATTRITION & COHORT SUMMARY TABLE (FOR SLIDE 1 / POSTER OVERVIEW)
# ==============================================================================

if (exists("n_raw") && exists("n_after_filters") && exists("n_hosp_before")) {
  attrition_tbl <- tibble(
    Step = c(
      "1. Raw SPARCS Heart Failure (APR-DRG 194) stays",
      "2. After removing redacted IDs, censored 120+ stays & invalid costs",
      "3. Final cohort after filtering hospitals with >= 25 stays"
    ),
    Stays     = c(n_raw, n_after_filters, nrow(data)),
    Hospitals = c(NA, n_hosp_before, n_distinct(data$provider_id))
  )
  print(attrition_tbl)
}

# Summary statistics stratified by APR Severity of Illness Tier
cohort_summary <- data %>%
  group_by(severity_tier) %>%
  summarise(
    n_stays        = n(),
    pct_cohort     = round(100 * n() / nrow(data), 1),
    mean_cost      = round(mean(total_costs), 0),
    median_cost    = round(median(total_costs), 0),
    sd_log_cost    = round(sd(log_cost), 3),    # Notice variance growth across tiers!
    iqr_log_cost   = round(IQR(log_cost), 3),
    mean_los       = round(mean(los_days), 2),
    pct_procedure  = round(100 * mean(any_procedure == "Yes"), 1),
    pct_died       = round(100 * mean(died), 1),
    .groups = "drop"
  )
cat("\n--- Cohort Summary by Clinical Severity Tier ---\n")
print(cohort_summary)

# ==============================================================================
# 2. PLOT 1: RESPONSE DISTRIBUTIONS (RAW VS. LOG-TRANSFORMED)
#    Motivates Gaussian assumption in Linear & Mixed-Effects Models
# ==============================================================================

p1a <- ggplot(data, aes(x = total_costs)) +
  geom_histogram(bins = 60, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  scale_x_continuous(labels = label_dollar(), limits = c(0, quantile(data$total_costs, 0.99))) +
  labs(title = "A. Raw Total Costs (Right-Skewed)", x = "Total Cost (USD, 99th pct)", y = "Count") +
  theme_minimal(base_size = 11)

p1b <- ggplot(data, aes(x = log_cost)) +
  geom_histogram(aes(y = after_stat(density)), bins = 60, fill = "#2b5c8f", color = "white", alpha = 0.85) +
  stat_function(fun = dnorm, args = list(mean = mean(data$log_cost), sd = sd(data$log_cost)),
                color = "#d95f02", linewidth = 1) +
  labs(title = "B. Log(Total Costs) with Normal Fit", x = "log_cost", y = "Density") +
  theme_minimal(base_size = 11)

p1c <- ggplot(data, aes(x = los_days)) +
  geom_histogram(binwidth = 1, fill = "#388e3c", color = "white", alpha = 0.85) +
  scale_x_continuous(limits = c(0, 35)) +
  labs(title = "C. Raw Length of Stay (Days)", x = "LOS (Days, capped at 35)", y = "Count") +
  theme_minimal(base_size = 11)

p1d <- ggplot(data, aes(x = log_los)) +
  geom_histogram(aes(y = after_stat(density)), bins = 35, fill = "#388e3c", color = "white", alpha = 0.85) +
  stat_function(fun = dnorm, args = list(mean = mean(data$log_los), sd = sd(data$log_los)),
                color = "#d95f02", linewidth = 1) +
  labs(title = "D. Log(LOS + 0.5) Distribution", x = "log_los", y = "Density") +
  theme_minimal(base_size = 11)

fig1_response <- (p1a | p1b) / (p1c | p1d) +
  plot_annotation(title = "Response Variable Transformations: Achieving Approximate Gaussianity")
ggsave("plots/01_eda_response_distributions.png", fig1_response, width = 11, height = 7, dpi = 300)

# ==============================================================================
# 3. PLOT 2: VARIANCE HETEROGENEITY (MOTIVATING MODULE 2: varIdent & varPower)
# ==============================================================================

# Formal statistical tests for heteroscedasticity across groups
cat("\n--- Formal Homoscedasticity Tests (Module 2 Motivation) ---\n")
print(bartlett.test(log_cost ~ severity_tier, data = data))
print(car::leveneTest(log_cost ~ severity_tier * admission_type, data = data))

# Panel A: Box + Violin plot of log_cost across Severity Tier & Admission Type
p2a <- ggplot(data, aes(x = severity_tier, y = log_cost, fill = severity_tier)) +
  geom_violin(alpha = 0.35, color = NA, trim = TRUE) +
  geom_boxplot(width = 0.25, outlier.alpha = 0.15, outlier.size = 0.7) +
  facet_wrap(~ admission_type) +
  scale_fill_brewer(palette = "YlOrRd") +
  labs(
    title = "A. Mean & Spread of log_cost by Severity Tier and Admission Type",
    subtitle = "Dispersion widens substantially in 'Extreme' severity and 'Elective' stays",
    x = "APR Severity of Illness Tier",
    y = "log(Total Cost)"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

# Panel B: Explicit plot of Group Standard Deviations (visual proof for varIdent)
sd_grid <- data %>%
  group_by(severity_tier, admission_type, any_procedure) %>%
  summarise(
    sd_log_cost = sd(log_cost),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 20)

p2b <- ggplot(sd_grid, aes(x = severity_tier, y = sd_log_cost,
                           color = admission_type, group = interaction(admission_type, any_procedure),
                           linetype = any_procedure)) +
  geom_point(size = 3) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "B. Standard Deviation of log_cost Across Clinical Strata",
    subtitle = "Directly motivates nlme::varIdent(form = ~ 1 | severity_tier * admission_type)",
    x = "APR Severity of Illness Tier",
    y = "SD(log_cost)",
    color = "Admission Type",
    linetype = "Procedure Performed"
  ) +
  theme_minimal(base_size = 11)

fig2_hetero <- p2a / p2b + plot_layout(heights = c(1.3, 1))
ggsave("plots/02_eda_variance_heterogeneity.png", fig2_hetero, width = 10, height = 8, dpi = 300)

# ==============================================================================
# 4. PLOT 3: PROVIDER HIERARCHY & REGIONAL CLUSTERING (MOTIVATING MODULE 3 & 4)
#    Visualizing Random Intercepts & Random Slopes across Hospitals
# ==============================================================================

# Compute raw ANOVA-based Intraclass Correlation Coefficient (unadjusted ICC)
anova_prov <- aov(log_cost ~ provider_id, data = data)
ms_table   <- summary(anova_prov)[[1]]
n0         <- (nrow(data) - sum(table(data$provider_id)^2) / nrow(data)) / (n_distinct(data$provider_id) - 1)
sigma2_b   <- (ms_table["provider_id", "Mean Sq"] - ms_table["Residuals", "Mean Sq"]) / n0
sigma2_w   <- ms_table["Residuals", "Mean Sq"]
raw_icc    <- sigma2_b / (sigma2_b + sigma2_w)
cat(sprintf("\nUnadjusted Provider ICC for log_cost: %.1f%% of total variance is between hospitals!\n", 100 * raw_icc))

# Panel A: Hospital medians & IQRs across all ~140 hospitals, colored by Region
hosp_rank_df <- data %>%
  group_by(provider_id, provider_name, region, hosp_volume, hosp_cmi) %>%
  summarise(
    mean_log_cost = mean(log_cost),
    se_log_cost   = sd(log_cost) / sqrt(n()),
    q25           = quantile(log_cost, 0.25),
    q75           = quantile(log_cost, 0.75),
    .groups = "drop"
  ) %>%
  arrange(mean_log_cost) %>%
  mutate(rank = row_number())

p3a <- ggplot(hosp_rank_df, aes(x = rank, y = mean_log_cost, color = region)) +
  geom_linerange(aes(ymin = mean_log_cost - 1.96 * se_log_cost,
                     ymax = mean_log_cost + 1.96 * se_log_cost), alpha = 0.6) +
  geom_point(aes(size = hosp_volume), alpha = 0.85) +
  geom_hline(yintercept = mean(data$log_cost), linetype = "dashed", color = "black") +
  labs(
    title = sprintf("A. Caterpillar View of All %d Hospitals Ranked by Mean log_cost (Raw ICC = %.1f%%)",
                    nrow(hosp_rank_df), 100 * raw_icc),
    subtitle = "Strong institutional baseline differences and regional stratification (NYC vs. Upstate)",
    x = "Hospital Rank (Ordered by Mean log_cost)",
    y = "Mean log_cost (+/- 95% CI)",
    color = "Health Service Area (Region)",
    size = "Stay Volume"
  ) +
  theme_minimal(base_size = 11)

# Panel B: Empirical Hospital Severity Slopes (Motivating Random Slopes: ~ 1 + severity_centered | provider_id)
# Select a random sample of 30 large hospitals to show differing slopes clearly
set.seed(42)
sample_hosps <- hosp_rank_df %>% filter(hosp_volume >= 200) %>% slice_sample(n = 30) %>% pull(provider_id)

p3b <- ggplot(data %>% filter(provider_id %in% sample_hosps),
              aes(x = severity_num, y = log_cost, group = provider_id, color = region)) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.6, alpha = 0.7) +
  stat_smooth(aes(group = 1), method = "lm", color = "black", linewidth = 1.4, se = TRUE) +
  scale_x_continuous(breaks = 1:4, labels = sev_levels) +
  labs(
    title = "B. Hospital-Specific Linear Profiles Across Severity (30 High-Volume Hospitals)",
    subtitle = "Differing baseline heights = Random Intercepts; differing steepness = Random Slopes",
    x = "APR Severity of Illness (1 = Minor to 4 = Extreme)",
    y = "Fitted Hospital log_cost Trajectory"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

fig3_hierarchy <- p3a / p3b + plot_layout(heights = c(1.2, 1))
ggsave("plots/03_eda_provider_hierarchy.png", fig3_hierarchy, width = 11, height = 8.5, dpi = 300)

# ==============================================================================
# 5. PLOT 4: NON-LINEAR RELATIONSHIPS (MOTIVATING MODULE 5: NONPARAMETRIC GAMMs)
# ==============================================================================

# Panel A: Non-linear relationship between los_days (and log_los) vs. log_cost by Severity Tier
p4a <- ggplot(data %>% filter(los_days <= 40),
              aes(x = los_days, y = log_cost, color = severity_tier)) +
  geom_point(alpha = 0.05, size = 0.6) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE, linewidth = 1.1) +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title = "A. Non-Linear Cost Escalation Over Length of Stay (Capped at 40 Days)",
    subtitle = "Steep initial ICU/diagnostic surge followed by diminishing marginal daily cost (Motivates GAM splines)",
    x = "Length of Stay (Days)",
    y = "log(Total Cost)",
    color = "Severity Tier"
  ) +
  theme_minimal(base_size = 11)

# Panel B: Hospital-level ecological relationships (CMI & Volume vs. Hospital Mean Cost)
p4b <- ggplot(hosp_rank_df, aes(x = hosp_cmi, y = mean_log_cost)) +
  geom_point(aes(size = hosp_volume, color = region), alpha = 0.8) +
  geom_smooth(method = "loess", color = "black", linetype = "dashed", se = TRUE) +
  labs(
    title = "B. Hospital Case-Mix Index (hosp_cmi) vs. Mean Hospital log_cost",
    subtitle = "Hospitals treating more complex case mixes incur systematically higher costs",
    x = "Hospital Case-Mix Index (Mean APR Severity)",
    y = "Hospital Mean log_cost",
    color = "Region",
    size = "Volume"
  ) +
  theme_minimal(base_size = 11)

fig4_nonparam <- p4a | p4b
ggsave("plots/04_eda_nonlinear_relationships.png", fig4_nonparam, width = 13, height = 5.5, dpi = 300)

# ==============================================================================
# 6. BDA PREREQUISITE: MULTICOLLINEARITY (GVIF) & CASE-MIX MCA BIPLOT
#    Directly satisfies Syllabus Sec. 6 & Project FAQ ("use BDA techniques for EDA")
# ==============================================================================

# 6a. Generalized Variance Inflation Factor (GVIF) check on fixed-effect predictors
prelim_lm <- lm(
  log_cost ~ log_los + severity_tier + mortality_risk + age_group + gender +
    ed_admit + admission_type + payer + any_procedure + region +
    log_hosp_volume + hosp_cmi + hosp_ed_share,
  data = data
)
cat("\n--- Multicollinearity Check: Generalized VIF (GVIF^(1/(2*Df)) < 2 is ideal) ---\n")
print(car::vif(prelim_lm))

# 6b. Multiple Correspondence Analysis (MCA) on categorical patient case-mix
mca_vars <- data %>%
  dplyr::select(severity_tier, mortality_risk, age_group, admission_type, ed_admit, payer, any_procedure)

mca_res <- FactoMineR::MCA(mca_vars, graph = FALSE)

p5_mca <- factoextra::fviz_mca_var(
  mca_res,
  repel = TRUE,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  title = "Multiple Correspondence Analysis (MCA) of Patient Case-Mix Profiles"
) +
  labs(subtitle = "Dim 1 captures Clinical Acuity (Minor -> Extreme); Dim 2 separates Elective/Procedure vs. Emergency/ED") +
  theme_minimal(base_size = 11)

ggsave("plots/05_eda_casemix_mca.png", p5_mca, width = 9, height = 6.5, dpi = 300)

