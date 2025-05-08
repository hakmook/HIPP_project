#### Extended Data Figure 4C&D, Secretion traits vs age/BMI_s (scaled) ####
#### Author: Ke Xu, Hakmook Kang ####

rm(list = ls())

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat <- gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")


###################
###################
# 3 main explanatory variables
vars_int = c("Age_years_s", "BMI_s")

# insulin secretion variables, 11 variables
insulin_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
                  "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
                  "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
                  "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

# glucagon secretion variables, 9 variables
glucagon_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
                   "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
                   "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
                   "GCG_KCl_20_SI")

#################################################

#################################################
############ Age
# insulin secretion 
Summary_Table = vector(mode='list', length=2)
model_out = vector(mode='list', length=length(insulin_vars))

temp_summary = NULL
for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], "~", vars_int[1], " + Donor_HbA1c_s + Gender + race2 + BMI_s + center +
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                  format = "e", digits = 3)
Summary_Table[[1]] = temp_summary

rownames(Summary_Table[[1]]) = insulin_vars


# glucagon variables
model_out = vector(mode='list', length=length(glucagon_vars))

temp_summary = NULL
for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~", vars_int[1], " + Donor_HbA1c_s + Gender + race2 + BMI_s + center + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                  format = "e", digits = 3)
Summary_Table[[2]] = temp_summary

rownames(Summary_Table[[2]]) = glucagon_vars

names(Summary_Table) = c("Age:Insulin", "Age:Glucagon")



#################################################
############ BMI_s
# insulin secretion 
Summary_Table_al = vector(mode='list', length=2)
model_out = vector(mode='list', length=length(insulin_vars))

temp_summary = NULL
for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], "~", vars_int[2], " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                  format = "e", digits = 3)
Summary_Table_al[[1]] = temp_summary

rownames(Summary_Table_al[[1]]) = insulin_vars


# glucagon variables
model_out = vector(mode='list', length=length(glucagon_vars))

temp_summary = NULL
for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~", vars_int[2], " + Donor_HbA1c_s + Gender + race2 +  Age_years_s + center + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                  format = "e", digits = 3)
Summary_Table_al[[2]] = temp_summary

rownames(Summary_Table_al[[2]]) = glucagon_vars

names(Summary_Table_al) = c("BMI_s:Insulin", "BMI_s:Glucagon")




# Summary Table

### Insulin secretion trait and Age
Summary_Tab = Summary_Table[[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Insulin secretion trait and BMI_s
Summary_Tab = Summary_Table_al[[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Glucagon secretion trait and Age
Summary_Tab = Summary_Table[[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Glucagon secretion trait and BMI_s
Summary_Tab = Summary_Table_al[[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


