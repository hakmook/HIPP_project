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
hipp<-read_excel("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/20240411_HIPP_data_freeze_202211071_new_morphology_data_updated.xlsx", sheet="Master_table")

################ Clean data ################

################ Replace spaces, parentheses, dashes, and other special characters with underscores ################
colnames(hipp) <- gsub("[^[:alnum:]]+", "_", colnames(hipp))
colnames(hipp) <- gsub("_+", "_", colnames(hipp))
colnames(hipp) <- gsub("^_|_$", "", colnames(hipp))
colnames(hipp) <- gsub("content", "percent", colnames(hipp))

################ Filter cohort ################
hipp = hipp[which(hipp$n299cohort == "Y"),]

################ define non-diabetes and pre-diabetes ################
# [ ] group
hipp$group<-NA
hipp$group[hipp$Donor_HbA1c<5.7]<-1
hipp$group[hipp$Donor_HbA1c>=5.7]<-2
hipp$group<-factor(hipp$group, levels = 1:2, labels = c("Normal","Pre-diabetes"))

################ clean varaibles ################
# [ ] race2
# redefine race to combine American Indian with Native Hawaiian/Pacific Islander
hipp$race2<-NA
hipp$race2[hipp$Race=="White"]<-1
hipp$race2[hipp$Race=="Black or African American"]<-2
hipp$race2[hipp$Race=="Hispanic/Latino"]<-3
hipp$race2[hipp$Race=="Asian"]<-4
# hipp$race2[hipp$Race=="American Indian or Alaska Native" | 
#              hipp$Race=="Native Hawaiian or Other Pacific Islander"]<-5
# hipp$race2[hipp$Race=="Not Available"]<-6
hipp$race2<-factor(hipp$race2, levels = 1:4, 
                   labels = c("White","Black","Hispanic","Asian"))
label(hipp$race2) = "Race"

hipp<-subset(hipp, subset = !is.na(race2))  # dim(hipp)[1]  321
hipp$Race<-factor(hipp$Race)
hipp$race2<-factor(hipp$race2)

# [ ] center
hipp$center<-NA
hipp$center[hipp$Center=="Scharp-Lacy"]<-1
hipp$center[hipp$Center=="SC-ICRC"]<-2
hipp$center[hipp$Center=="Miami"]<-3
hipp$center[hipp$Center=="Wisconsin"]<-4
hipp$center[hipp$Center=="Pennsylvania"]<-5
hipp$center<-factor(hipp$center, levels = 1:5, labels = c("Scharp-Lacy","SC-ICRC","Miami","Wisconsin","Pennsylvania"))
hipp$center<-factor(hipp$center)
hipp$Center<-factor(hipp$Center)

hipp$BetaCellPct<-probitlink(hipp$Beta_Cells/100)
hipp$AlphaCellPct<-probitlink(hipp$Alpha_Cells/100)
hipp$DeltaCellPct<-probitlink(hipp$Delta_Cells/100)


#####################################################################

# Define the independent variables
independent_vars_full <- "Gender + race2 + Donor_HbA1c + Age_years + center + BMI + Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"
independent_vars_reduced <- "race2 + Donor_HbA1c + Age_years + center + BMI + Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"


# List of dependent variables
dependent_vars <- c(
  "AlphaCellPct", "BetaCellPct", "DeltaCellPct"
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

