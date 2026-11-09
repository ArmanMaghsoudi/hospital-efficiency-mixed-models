#########################################################
############ Applied Statistics Project - Dataset: hospital_df


#  Steps:
#    0. Setup
#    1. Load dataset
#    2. Integrity checks
#    3. Remove redundant columns
#    4. Remove constant, empty and unused columns
#    5. Fix variable types
#    6. Final checks
#    7. Save output (cleaned dataset)



### 0. Setup ------------------------------------------------------------------
library(readxl)
#setwd("~/Desktop/AS Project")



### 1. LOAD DATASET and FIRST LOOK  -------------------------------------------
hosp <- read_excel("data/hospital_df.xlsx")

dim(hosp) 
colnames(hosp)
str(hosp)

#Checking missing values per columns 
na_count <- colSums(is.na(hosp))  # number of missing values per column
na_count[na_count > 0]

#' Comment: 
#' Missing values only in ccsr_procedure_* (no procedure recorded),
#' payment_typology_2/3 (no 2nd/3rd payer), zip_code_3_digits, birth_weight.
#' --- None of the variables we use in the models has missing values.

# Working copy (let's create a copy to work on)
data <- hosp



### 2. INTEGRITY CHECKS -------------------------------------------------------
# Each row must be a different admission
sum(duplicated(data$subject_id))         # CHECK: 0


# Each hospital must have only one name and one region.
# If a hospital code had two names, it would not be a valid group.
n_hosp <- length(unique(data$provider_id))
n_hosp                                                       # CHECK: 122
nrow(unique(data[, c("provider_id", "provider_name")]))      # CHECK: 122
nrow(unique(data[, c("provider_id", "region")]))             # CHECK: 122

# No impossible values in the outcomes
min(data$length_of_stay)                 # CHECK: at least 1 day
min(data$total_costs)                    # CHECK: greater than 0


### 3. REMOVE REDUNDANT COLUMNS -----------------------------------------------
# The dataset contains 33 columns from SPARCS, to which another 17 columns have been added. 
# Some of these additions are simply copies with more convenient names.

# Before deleting the originals we check that they are really identical.
all(data$los_days       == data$length_of_stay)                  # CHECK: TRUE
all(data$severity_num   == data$apr_severity_of_illness_code)    # CHECK: TRUE
all(data$severity_tier  == data$apr_severity_of_illness)         # CHECK: TRUE
all(data$mortality_risk == data$apr_risk_of_mortality)           # CHECK: TRUE
all(data$admission_type == data$type_of_admission)               # CHECK: TRUE
all(data$provider_id    == data$permanent_facility_id)           # CHECK: TRUE
all(data$provider_name  == data$facility_name)                   # CHECK: TRUE
all(data$region         == data$hospital_service_area)           # CHECK: TRUE
table(data$ed_admit, data$emergency_department_indicator) #only "No-N" and "Yes-Y" are non-zero

# Check the log variables added: the difference with the formula must be (almost) zero
max(abs(data$log_cost - log(data$total_costs)))            # CHECK: ~0
data$length_of_stay <- as.numeric(data$length_of_stay) #first, change type to length_of_stay
max(abs(data$log_los  - log(data$length_of_stay + 0.5)))   # CHECK: ~0
# NB: log_los uses +0.5, not +1

#Remove redundant columns
redundant_cols <- c("length_of_stay", "apr_severity_of_illness_code",
                    "apr_severity_of_illness", "apr_risk_of_mortality",
                    "type_of_admission", "permanent_facility_id",
                    "facility_name", "hospital_service_area",
                    "emergency_department_indicator")
data <- data[, !(names(data) %in% redundant_cols)]  #REMOVE



### 4. REMOVE CONSTANT, EMPTY AND UNUSED COLUMNS ------------------------------

# 4a. Columns with only one value carry no information
unique(data$discharge_year)        # CHECK: only 2021
unique(data$apr_drg_code)          # CHECK: only 194 (heart failure)
unique(data$apr_mdc_code)          # CHECK: only 5
unique(data$apr_medical_surgical)  # CHECK: only "Medical"
sum(!is.na(data$birth_weight))     # CHECK: 0 (column completely empty)

constant_cols <- c("discharge_year", "apr_drg_code", "apr_drg_description",
                   "apr_mdc_code", "apr_mdc_description",
                   "apr_medical_surgical", "birth_weight")
data <- data[, !(names(data) %in% constant_cols)]

# 4b. Columns we do not use in the analysis
unused_cols <- c(
  "operating_certificate_number",  # licence number, can cover 2-3 hospitals
  "hospital_county",               # 43 levels, region is enough
  "zip_code_3_digits",             # patient residence, 50 levels
  "ccsr_diagnosis_description",    # same information as ccsr_diagnosis_code
  "ccsr_procedure_description",    # same information as ccsr_procedure_code
  "payment_typology_2",            # secondary payer, mostly empty
  "payment_typology_3",            # tertiary payer, mostly empty
  "total_charges"                  # list price; we use total_costs instead
)
data <- data[, !(names(data) %in% unused_cols)] #REMOVE

ncol(data) # CHECK: 26


### 5. FIX VARIABLES TYPES --------------------------------------------------------
# Given that in lm()/lme() the FIRST level of a factor is the reference category,
# for each categoric variable we choose the order of the levels ourselves.

# Severity and mortality risk: from least to most severe
sev_levels <- c("Minor", "Moderate", "Major", "Extreme")
data$severity_tier  <- factor(data$severity_tier,  levels = sev_levels)
data$mortality_risk <- factor(data$mortality_risk, levels = sev_levels)

# Age
table(data$age_group)
# very few young patients (only 3 under 18)
# so we merge the classes below 50 into a single class

data$age_group[data$age_group == "0 to 17"]  <- "Under 50"
data$age_group[data$age_group == "18 to 29"] <- "Under 50"
data$age_group[data$age_group == "30 to 49"] <- "Under 50"
data$age_group <- factor(data$age_group,
                         levels = c("Under 50", "50 to 69", "70 or Older"))
table(data$age_group)  # CHECK: 3 classes, no NA

# Simple categorical variables (first level = reference)
data$gender <- factor(data$gender,   levels = c("F", "M"))
data$ed_admit <- factor(data$ed_admit, levels = c("No", "Yes"))
data$admission_type <- factor(data$admission_type, 
                              levels = c("Emergency", "Urgent", "Elective"))

data$region <- relevel(factor(data$region), ref = "New York City")
data$race <- relevel(factor(data$race), ref = "White")
data$ethnicity <- relevel(factor(data$ethnicity), ref = "Not Span/Hispanic")
# "Unknown" ethnicity stays as a category (it is a real answer, not missing)

# Primary payer (who pays for the hospital stay)
# 9 categories, some very small -> 4 groups
data$payer <- "Other"      # start with "Other", then overwrite
data$payer[data$payment_typology_1 == "Medicare"] <- "Medicare"
data$payer[data$payment_typology_1 == "Medicaid"] <- "Medicaid"
data$payer[data$payment_typology_1 == "Private Health Insurance"]  <- "Private"
data$payer[data$payment_typology_1 == "Blue Cross/Blue Shield"]    <- "Private"
data$payer[data$payment_typology_1 == "Managed Care, Unspecified"] <- "Private"
data$payer <- factor(data$payer,
                     levels = c("Medicare", "Medicaid", "Private", "Other"))

table(data$payment_typology_1, data$payer)   # CHECK: each payer in one group
data$payment_typology_1 <- NULL


# Principal diagnosis: TRUE if it is heart failure (code CIR019)
data$hf_principal <- data$ccsr_diagnosis_code == "CIR019"
data$ccsr_diagnosis_code <- factor(data$ccsr_diagnosis_code)

# Procedure: 127 codes or empty. Empty = no procedure recorded.
# We keep only the information "procedure yes / no".
data$any_procedure <- "Yes"
data$any_procedure[is.na(data$ccsr_procedure_code)] <- "No"
data$any_procedure <- factor(data$any_procedure, levels = c("No", "Yes"))
data$ccsr_procedure_code <- NULL

#' Comments
#' The procedure code has 127 distinct values and is empty in 42% of cases. 
#' A factor with 127 levels is unusable, but the information 
#' “was a procedure performed?” is useful, because a procedure increases costs.

# Discharge disposition is known only at the END of the stay, so it could be an
# outcome, not a regressor. We only use it to create three indicators.
data$died        <- data$patient_disposition == "Expired"
data$left_ama    <- data$patient_disposition == "Left Against Medical Advice" 
data$transferred <- data$patient_disposition == "Short-term Hospital"

# - died: TRUE if the patient died in the hospital
# - left_ama: TRUE if he left against medical advice
# - transferred: TRUE if transferred to another hospital

# Grouping variable
data$provider_id <- factor(data$provider_id)



### 6.FINAL CHECKS ----------------------------------------------------

# 6a. Put the columns in a logical order
data <- data[, c(
  # identifiers
  "subject_id", "provider_id", "provider_name",
  # responses
  "log_cost", "log_los", "total_costs", "los_days",
  # patient variables (level 1)
  "severity_tier", "severity_num", "severity_centered", "mortality_risk",
  "age_group", "gender", "race", "ethnicity", "ed_admit", "admission_type",
  "payer", "ccsr_diagnosis_code", "hf_principal", "any_procedure",
  # hospital variables (level 2)
  "region", "hosp_volume", "log_hosp_volume", "hosp_cmi", "hosp_ed_share",
  # discharge disposition
  "patient_disposition", "died", "left_ama", "transferred")]

dim(data) # CHECK: 30 columns


# 6b. The model variables must have no missing values
model_vars <- c("log_cost", "log_los", "severity_tier", "severity_centered",
                "mortality_risk", "age_group", "gender", "ed_admit", "payer",
                "region", "log_hosp_volume", "hosp_cmi", "hosp_ed_share",
                "provider_id")
colSums(is.na(data[, model_vars]))  # CHECK: all 0

# 6c. Quick summary
summary(data[, c("log_cost", "log_los", "severity_tier", "age_group", "payer")])
summary(as.numeric(table(data$provider_id)))   # patients per hospital

# Standard deviation of the responses by severity: it grows with severity,
# which is why we will use varIdent(form = ~ 1 | severity_tier) later
tapply(data$log_cost, data$severity_tier, sd)
tapply(data$log_los,  data$severity_tier, sd)



# 7. SAVE CLEANED DATASET -----------------------------------------------------
write.csv(data, "data/hospital_clean.csv", row.names = FALSE)





