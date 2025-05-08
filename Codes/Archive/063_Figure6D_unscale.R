#### Figure 6D, Secretion traits, hormone contents, and cell compositions with predicted ancestry  ####
#### Author: Ke Xu, Hakmook Kang ####

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


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta_noscale.RData")
hipp <- hipp




################ merge 2 dataset ################
# new data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")

#####################################################################
INS_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
              "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
              "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
              "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

GCG_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
              "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
              "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
              "GCG_KCl_20_SI")

cell_vars <- c("BetaCellPct", "AlphaCellPct", "DeltaCellPct")
content_vars <- c("Islet_Insulin_Content_ng_IEQ", "Islet_Glucagon_Content_pg_IEQ")

dat_all <- dat_all %>%
  mutate(across(all_of(c(INS_vars, GCG_vars)), as.numeric))
#####################################################################

############################# Insulin secretion traits #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(INS_vars))
temp_summary <- NULL

for (var in INS_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- INS_vars
Summary_Table

############################# Glucagon secretion traits #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(GCG_vars))
temp_summary <- NULL

for (var in GCG_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- GCG_vars
Summary_Table

############################# Cell compositions #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(cell_vars))
temp_summary <- NULL

for (var in cell_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- cell_vars
Summary_Table


############################# Hormone contents #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(content_vars))
temp_summary <- NULL

for (var in content_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- content_vars
Summary_Table

