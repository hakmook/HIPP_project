#### Figure 4 ####
#### Author: Ke Xu, Hakmook Kang####

# Load package 
library(dplyr)
library(tidyverse)
library(Hmisc)
library(rms)
library(kableExtra)
library(knitr)
library(dplyr)
library(xlsx)
library(VGAM)
library(sjPlot)

# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat <- gen_dat

# merge genetic dataset
hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_sub = merge(gen_dat, hipp, by="DONOR_RRID")
dat_sub <- dat_sub[which(!is.na(dat_sub$Donor_HbA1c_s)),]




# Define the independent variables
independent_vars_full <- "race2 + Donor_HbA1c_s + Age_years_s + Gender + center + BMI_s + PreShipmentCultureTime + IsletTransitTime"
independent_vars_reduced <- "Donor_HbA1c_s + Age_years_s + Gender + center + BMI_s + PreShipmentCultureTime + IsletTransitTime"


### Insulin secretion traits
# List of dependent variables
dependent_vars <- c(
  "INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
  "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
  "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
  "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI"
)

# Initialize a list to store ANOVA results
anova_results <- list()

# Loop through dependent variables and fit models
for (var in dependent_vars) {
  # Fit the full and reduced models
  formula_full <- as.formula(paste(var, "~", independent_vars_full))
  formula_reduced <- as.formula(paste(var, "~", independent_vars_reduced))
  
  model_full <- lm(formula_full, data = hipp)
  model_reduced <- lm(formula_reduced, data = hipp)
  
  # Perform ANOVA
  anova_results[[var]] <- anova(model_reduced, model_full)
}

# Display the ANOVA results for all models
anova_results_ins <- anova_results
anova_results_ins


### Glucagon secretion traits
# List of dependent variables
dependent_vars <- c(
  "GCG_basal_pg_IEQ",
  "GCG_G_16_7_AUC_pg_IEQ",
  "GCG_G_16_7_II",
  "GCG_G_16_7_IBMX_100_AUC_pg_IEQ",
  "GCG_G_16_7_IBMX_100_SI",
  "GCG_G_1_7_Epi_1_AUC_pg_IEQ",
  "GCG_G_1_7_Epi_1_SI",
  "GCG_KCl_20_AUC_pg_IEQ",
  "GCG_KCl_20_SI"
)

# Initialize a list to store ANOVA results
anova_results <- list()

# Loop through dependent variables and fit models
for (var in dependent_vars) {
  # Fit the full and reduced models
  formula_full <- as.formula(paste(var, "~", independent_vars_full))
  formula_reduced <- as.formula(paste(var, "~", independent_vars_reduced))
  
  model_full <- lm(formula_full, data = hipp)
  model_reduced <- lm(formula_reduced, data = hipp)
  
  # Perform ANOVA
  anova_results[[var]] <- anova(model_reduced, model_full)
}

# Display the ANOVA results for all models
anova_results_glu <- anova_results
anova_results_glu

