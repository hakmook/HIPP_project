#### Figure 6 C & D, models for cell composition ~ race/ancestry + 7 other covariates ####
#### Author: Ke Xu, Hakmook Kang ####

library(dplyr)
library(sjPlot)

################ Import Data ################
# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

# Define the independent variables
independent_vars_full <- "race2 + Donor_HbA1c_s + Age_years_s + Gender + center + BMI_s + PreShipmentCultureTime + IsletTransitTime"

# List of dependent variables
dependent_vars <- c(
  "AlphaCellPct", "BetaCellPct", "DeltaCellPct"
)


# N = 299

## Beta cell composition ~ race + 7 other covariates (n=299)

 
formula_full <- as.formula(paste('BetaCellPct', "~", independent_vars_full))
model_full <- lm(formula_full, data = hipp)


tab_model(model_full)
 

# Alpha cell composition ~ race + 7 other covariates (n=299)

 
formula_full <- as.formula(paste('AlphaCellPct', "~", independent_vars_full))
model_full <- lm(formula_full, data = hipp)


tab_model(model_full)
 


# Delta cell composition ~ race + 7 other covariates (n=299)

 
formula_full <- as.formula(paste('DeltaCellPct', "~", independent_vars_full))
model_full <- lm(formula_full, data = hipp)


tab_model(model_full)
 

 
# Define outcome variables
dependent_vars <- c("AlphaCellPct", "BetaCellPct", "DeltaCellPct")
results_299 <- data.frame()

# Loop over outcomes
for (outcome in dependent_vars) {
  formula <- as.formula(paste0(outcome, " ~ race2 + Donor_HbA1c_s + Age_years_s + Gender + center + BMI_s + PreShipmentCultureTime + IsletTransitTime"))
  model <- lm(formula, data = hipp)
  coefs <- summary(model)$coefficients
  asian_row <- coefs[grep("race2Asian", rownames(coefs)), ]
  results_299 <- rbind(results_299, data.frame(
    Cell = gsub("CellPct", "", outcome),
    Estimate = asian_row[1],
    P_value = asian_row[4]
  ))
}

# Adjust p-values
results_299$Adj_P_value <- p.adjust(results_299$P_value, method = "BH")

 


# N = 268

 

# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
hipp = merge(gen_dat, hipp, by="DONOR_RRID")

# Define the independent variables
independent_vars_full <- "SuperPopulationClass + Donor_HbA1c_s + Age_years_s + Gender + center + BMI_s + PreShipmentCultureTime + IsletTransitTime"

# List of dependent variables
dependent_vars <- c(
  "AlphaCellPct", "BetaCellPct", "DeltaCellPct"
)


hipp$SuperPopulationClass <- relevel(factor(hipp$SuperPopulationClass), ref = "EUR")
 

# Beta cell composition ~ race (genetic) + 7 other covariates (n=268)

 
formula_full <- as.formula(paste('BetaCellPct', "~", independent_vars_full))
model_full <- lm(formula_full, data = hipp)


tab_model(model_full)
 

# Alpha cell composition ~ race (genetic) + 7 other covariates (n=268)

 
formula_full <- as.formula(paste('AlphaCellPct', "~", independent_vars_full))
model_full <- lm(formula_full, data = hipp)


tab_model(model_full)
 

# Delta cell composition ~ race + 7 other covariates (n=268)

 
formula_full <- as.formula(paste('DeltaCellPct', "~", independent_vars_full))
model_full <- lm(formula_full, data = hipp)


tab_model(model_full)
 

 
# Define outcome variables
dependent_vars <- c("AlphaCellPct", "BetaCellPct", "DeltaCellPct")
results_268 <- data.frame()

# Loop over outcomes
for (outcome in dependent_vars) {
  formula <- as.formula(paste0(outcome, " ~ SuperPopulationClass + Donor_HbA1c_s + Age_years_s + Gender + center + BMI_s + PreShipmentCultureTime + IsletTransitTime"))
  model <- lm(formula, data = hipp)
  coefs <- summary(model)$coefficients
  eas_row <- coefs[grep("SuperPopulationClassEAS", rownames(coefs)), ]
  results_268 <- rbind(results_268, data.frame(
    Cell = gsub("CellPct", "", outcome),
    Estimate = eas_row[1],
    P_value = eas_row[4]
  ))
}

# Adjust p-values
results_268$Adj_P_value <- p.adjust(results_268$P_value, method = "BH")

 


# Summary Table

## Table for N=299 (Asian)
 
# Display table: Self-reported Race (n = 299): Effect of Asian vs Reference on Cell Composition
results_299

## Table for N=268 (EAS)
 
# Display table: Genetic Ancestry (n = 268): Effect of EAS vs EUR on Cell Composition
results_268
 