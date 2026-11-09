# ==============================================================================
# APPLIED STATISTICS - PROJECT: HOSPITAL PROVIDER EFFICIENCY
# ==============================================================================

### 0. SETUP ------------------------------------------------------------------


pkgs <- c("dplyr", "tidyr", "readr", "writexl", "ggplot2", "patchwork", "scales", "forcats")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create("data",  showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

MIN_STAYS <- 25          # minimum stays per hospital (slide 6: "≥ [N] stays")
raw_file  <- "data/raw_sparcs_hf_2021.csv"


### 1. DOWNLOAD ALL HEART-FAILURE STAYS (APR-DRG 194) -------------------------
# Socrata returns at most 50,000 rows per request -> download in pages.
# $order=:id makes the paging stable (no duplicated / skipped rows).

if (!file.exists(raw_file)) {
  base_url <- paste0(
    "https://health.data.ny.gov/resource/tg3i-cinn.csv?",
    "$where=apr_drg_code='194'&$order=:id&$limit=50000&$offset="
  )
  pages  <- list()
  offset <- 0
  repeat {
    cat("Downloading rows from offset", offset, "...\n")
    page <- read.csv(URLencode(paste0(base_url, offset)),
                     colClasses = "character",       # same types on every page
                     stringsAsFactors = FALSE)
    pages[[length(pages) + 1]] <- page
    if (nrow(page) < 50000) break
    offset <- offset + 50000
  }
  raw_sparcs <- bind_rows(pages)
  write.csv(raw_sparcs, raw_file, row.names = FALSE)
} else {
  raw_sparcs <- read.csv(raw_file, colClasses = "character", stringsAsFactors = FALSE)
}

cat("Raw rows:", nrow(raw_sparcs), "\n")      # CHECK: 57839
stopifnot(nrow(raw_sparcs) > 50000)            # stops if the download was truncated


### 2. PREPARATION (as in Data_Preparation.R) ---------------------------------
# Note: with colClasses = "character" missing values arrive as "" -> treat as NA
raw_sparcs[raw_sparcs == ""] <- NA

n_raw <- nrow(raw_sparcs)

View(raw_sparcs)

# Clean and map directly to Slide 3: Subject + Provider + Y + Subject Covariates + Provider Covariates
hosp_df <- raw_sparcs %>%
  # Remove censored "120+" stays and missing facility IDs
  filter(
    !is.na(permanent_facility_id),
    length_of_stay != "120+",
    type_of_admission %in% c("Emergency", "Urgent", "Elective")
  ) %>%
  mutate(
    # Numerical conversions
    los_days      = as.numeric(length_of_stay),
    total_costs   = as.numeric(gsub("[^0-9.]", "", total_costs)),
    total_charges = as.numeric(gsub("[^0-9.]", "", total_charges)),
    severity_num  = as.numeric(apr_severity_of_illness_code),
    
    # Continuous Responses (Y) - Log-transformed for normality
    log_cost      = log(total_costs),
    log_los       = log(los_days + 0.5),
    
    # Grouping Factor (Provider)
    provider_id   = factor(permanent_facility_id),
    provider_name = factor(facility_name),
    
    # Subject-Level Covariates (Patient Case-Mix)
    age_group     = factor(age_group, levels = c("0 to 17", "18 to 29", "30 to 49", "50 to 69", "70 or Older")),
    gender        = factor(gender),
    admission_type= factor(type_of_admission, levels = c("Elective", "Urgent", "Emergency")),
    severity_tier = factor(
      apr_severity_of_illness,
      levels = c("Minor", "Moderate", "Major", "Extreme")
    ),
    mortality_risk = factor(
      apr_risk_of_mortality,
      levels = c("Minor", "Moderate", "Major", "Extreme")
    ),
    ed_admit      = factor(ifelse(emergency_department_indicator == "Y", "Yes", "No"))
  ) %>%
  drop_na(log_cost, log_los, severity_tier, age_group, gender, admission_type)

# Compute Provider-Level Covariates (Hospital Volume, Case-Mix Index, Region)
# and filter to hospitals with at least 25 admissions for stable random-slope estimation
provider_summary <- hosp_df %>%
  group_by(provider_id) %>%
  summarise(
    hosp_volume      = n(),
    hosp_cmi         = mean(severity_num, na.rm = TRUE), # Hospital Case-Mix Index
    hosp_ed_share    = mean(ed_admit == "Yes", na.rm = TRUE),
    region           = first(hospital_service_area),
    .groups = "drop"
  ) %>%
  filter(hosp_volume >= 25)

hosp_df <- hosp_df %>%
  inner_join(provider_summary, by = "provider_id") %>%
  mutate(
    subject_id = row_number(),
    region     = factor(region),
    log_hosp_volume = log(hosp_volume),
    # Center numerical severity around hospital or grand mean for mixed-effects stability
    severity_centered = severity_num - mean(severity_num)
  )

cat("Final Dataset Dimensions (n >> p):", dim(hosp_df), "\n")
cat("Number of Unique Hospital Providers:", n_distinct(hosp_df$provider_id), "\n")

#View(hosp_df)

library(writexl)

write_xlsx(hosp_df, "data/hospital_df.xlsx")

