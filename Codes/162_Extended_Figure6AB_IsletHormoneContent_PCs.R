#### Extended Data Figure 6, Insulin and Glucagon Secretion Traits with Islet Hormone Content (scaled) ####
#### Author: Ke Xu, Hakmook Kang ####

rm(list = ls())

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")

###################################################################
# 3 main explanatory variables
vars_int <- c("Islet_Insulin_Content_ng_IEQ_s", "Islet_Glucagon_Content_pg_IEQ_s")

INS_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
              "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
              "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
              "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

GCG_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
              "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
              "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
              "GCG_KCl_20_SI")

co_vars <- c("Donor_HbA1c_s", "Gender", "PC1", "PC2", "PC3", "PC4", "PC5", "Age_years_s", "center", "BMI_s", "PreShipmentCultureTime", "IsletTransitTime")
###################################################################
###################################################################
vars_int = vars_int
outcome_int_ins = INS_vars
outcome_int_glu = GCG_vars

hipp <- dat_all[, c(vars_int, INS_vars, GCG_vars, co_vars)]

############################################
### Insulin secretion and Islet_Insulin_Content_ng_IEQ_s
########################################
Summary_Table = vector(mode='list', length=2)

insulin_tb = NULL
for (m in 1:length(outcome_int_ins)){
  model = as.formula(paste0(outcome_int_ins[m], "~", 
                            "Islet_Insulin_Content_ng_IEQ_s", " + Gender + Donor_HbA1c_s + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_tb = rbind(insulin_tb, temp1) 
}

insulin_tb = as.data.frame(insulin_tb)
insulin_tb$adj_pvalue = formatC(p.adjust(insulin_tb[,4], method='fdr'),
                                format = "e", digits = 3)
Summary_Table = insulin_tb
rownames(Summary_Table) = outcome_int_ins

Summary_Table

############################################
### Glucagon secretion and Islet_Insulin_Content_ng_IEQ_s
########################################
Summary_Table = vector(mode='list', length=2)

insulin_tb = NULL
for (m in 1:length(outcome_int_glu)){
  model = as.formula(paste0(outcome_int_glu[m], "~", 
                            "Islet_Insulin_Content_ng_IEQ_s", " + Gender + Donor_HbA1c_s + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_tb = rbind(insulin_tb, temp1) 
}

insulin_tb = as.data.frame(insulin_tb)
insulin_tb$adj_pvalue = formatC(p.adjust(insulin_tb[,4], method='fdr'),
                                format = "e", digits = 3)
Summary_Table = insulin_tb
rownames(Summary_Table) = outcome_int_glu

Summary_Table

############################################
### Insulin secretion and Islet_Glucagon_Content_pg_IEQ_s
########################################
Summary_Table = vector(mode='list', length=2)

insulin_tb = NULL
for (m in 1:length(outcome_int_ins)){
  model = as.formula(paste0(outcome_int_ins[m], "~", 
                            "Islet_Glucagon_Content_pg_IEQ_s", " + Gender + Donor_HbA1c_s + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_tb = rbind(insulin_tb, temp1) 
}

insulin_tb = as.data.frame(insulin_tb)
insulin_tb$adj_pvalue = formatC(p.adjust(insulin_tb[,4], method='fdr'),
                                format = "e", digits = 3)
Summary_Table = insulin_tb
rownames(Summary_Table) = outcome_int_ins

Summary_Table

############################################
### Glucagon secretion and Islet_Glucagon_Content_pg_IEQ_s
########################################
Summary_Table = vector(mode='list', length=2)

insulin_tb = NULL
for (m in 1:length(outcome_int_glu)){
  model = as.formula(paste0(outcome_int_glu[m], "~", 
                            "Islet_Glucagon_Content_pg_IEQ_s", " + Gender + Donor_HbA1c_s + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_tb = rbind(insulin_tb, temp1) 
}

insulin_tb = as.data.frame(insulin_tb)
insulin_tb$adj_pvalue = formatC(p.adjust(insulin_tb[,4], method='fdr'),
                                format = "e", digits = 3)
Summary_Table = insulin_tb
rownames(Summary_Table) = outcome_int_glu

Summary_Table
