#### Figure 6B ####
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
library(readxl)

################ Import Data ################
# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta_noscale.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
hipp = merge(gen_dat, hipp, by="DONOR_RRID")

#####################################################################

# Define the independent variables
independent_vars_full <- "Gender + PC1 + PC2 + PC3 + PC4 + PC5 + Donor_HbA1c + Age_years + center + BMI + Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"
independent_vars_reduced <- "PC1 + PC2 + PC3 + PC4 + PC5 + Donor_HbA1c + Age_years + center + BMI + Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"


# List of dependent variables
dependent_vars <- c(
  "Alpha_cell_pct", "Beta_cell_pct", "Delta_cell_pct"
)


#####################################################################

# Initialize a list to store ANOVA results
anova_results <- list()

# Initialize a data frame to store the results
results <- data.frame(
  names = character(), Coefficient = numeric(), P.value = numeric(),
  Adj.P.value = numeric(), stringsAsFactors = FALSE
)

# Initialize separate lists for global p-values and coefficient p-values
global_p_values <- c()
coeff_p_values <- c()

# Loop through dependent variables
for (var in dependent_vars) {
#  var = 'BetaCellPct'
  # Fit the full model
  formula_full <- as.formula(paste(var, "~", independent_vars_full))
  model_full <- lm(formula_full, data = hipp)
  
  # Extract coefficients and p-values for Gender
  gender_coeff <- summary(model_full)$coefficients[grep("Gender", rownames(summary(model_full)$coefficients)), ]
  coeffs <- gender_coeff["Estimate"]
  p_vals <- round(gender_coeff["Pr(>|t|)"], 4)
  
  # Perform ANOVA
  formula_reduced <- as.formula(paste(var, "~", independent_vars_reduced))
  model_reduced <- lm(formula_reduced, data = hipp)
  global_p <- anova(model_reduced, model_full)[2, "Pr(>F)"]
  
  # Collect the global p-value and coefficient p-values
  global_p_values <- c(global_p_values, global_p)
  coeff_p_values <- c(coeff_p_values, p_vals)
  
  # Add results for each race comparison
  for (i in 1:length(coeffs)) {
    results <- rbind(results, data.frame(
      names = paste(var, "Gender-Fem:Male", rownames(gender_coeff)[i], sep = " "),
      Coefficient = coeffs[i],
      P.value = p_vals[i],
      Adj.P.value = NA  # Placeholder for later
    ))
  }
  
  # Temporarily store global p-value (adjustment done later)
  results <- rbind(results, data.frame(
    names = paste(var, "Global p-value"),
    Coefficient = NA,
    P.value = global_p,
    Adj.P.value = NA  # Placeholder
  ))
}

# Adjust the global p-values together using FDR method
global_p_values_adj <- p.adjust(global_p_values, method = "fdr")

# Adjust the coefficient p-values together using FDR method
coeff_p_values_adj <- p.adjust(coeff_p_values, method = "fdr")

# Update adjusted p-values in the results data frame
# Update adjusted p-values for coefficients
coeff_index <- which(!grepl("Global p-value", results$names))
results$Adj.P.value[coeff_index] <- coeff_p_values_adj

# Update adjusted p-values for global p-values
global_index <- grep("Global p-value", results$names)
results$Adj.P.value[global_index] <- global_p_values_adj

results

