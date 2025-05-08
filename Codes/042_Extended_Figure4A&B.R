#### Extended Data Figure 4A&B, Insulin and Glucagon Secretion Traits with preshipment, transit time, and islet purity (scaled) ####
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
vars_int <- c("PreShipmentCultureTime","IsletTransitTime","IsletPurity") 

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

############ pre-shipment culture time
# insulin secretion 
Summary_Table_PCT = vector(mode='list', length=2)
insulin_model_out = vector(mode='list', length=length(insulin_vars))
insulin_preshipmtct <- NULL

for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], "~", vars_int[1], " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,] # coefficient for pre-shipment culture time
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_preshipmtct = rbind(insulin_preshipmtct, temp1) 
}

insulin_preshipmtct = as.data.frame(insulin_preshipmtct)
insulin_preshipmtct$adj_pvalue = formatC(p.adjust(insulin_preshipmtct[,4], method='fdr'),
                                         format = "e", digits = 3)
Summary_Table_PCT[[1]] = insulin_preshipmtct
rownames(Summary_Table_PCT[[1]]) = insulin_vars


# glucagon variables
glucagon_model_out = vector(mode='list', length=length(glucagon_vars))
glucagon_preshipmtct <- NULL

for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~", vars_int[1], 
                            " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  glucagon_preshipmtct = rbind(glucagon_preshipmtct, temp1) 
}

glucagon_preshipmtct = as.data.frame(glucagon_preshipmtct)
glucagon_preshipmtct$adj_pvalue = formatC(p.adjust(glucagon_preshipmtct[,4], 
                                                   method='fdr'),
                                          format = "e", digits = 3)
Summary_Table_PCT[[2]] = glucagon_preshipmtct
rownames(Summary_Table_PCT[[2]]) = glucagon_vars
names(Summary_Table_PCT) = c("Preshipment Culture Time: Insulin", 
                             "Preshipment Culture Time: Glucagon")



#################################################
############ Islet Transit Time

# insulin secretion 
Summary_Table_ITT = vector(mode='list', length=2)
#insulin_model_out = vector(mode='list', length=length(insulin_vars))

insulin_ITT = NULL
for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], "~", vars_int[2], " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_ITT = rbind(insulin_ITT, temp1) 
}

insulin_ITT = as.data.frame(insulin_ITT)
insulin_ITT$adj_pvalue = formatC(p.adjust(insulin_ITT[,4], method='fdr'),
                                 format = "e", digits = 3)
Summary_Table_ITT[[1]] = insulin_ITT
rownames(Summary_Table_ITT[[1]]) = insulin_vars


# glucagon variables
#glucagon_model_out = vector(mode='list', length=length(glucagon_vars))

glucagon_ITT = NULL
for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~", vars_int[2], " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  glucagon_ITT = rbind(glucagon_ITT, temp1) 
}

glucagon_ITT = as.data.frame(glucagon_ITT)
glucagon_ITT$adj_pvalue = formatC(p.adjust(glucagon_ITT[,4], method='fdr'),
                                  format = "e", digits = 3)
Summary_Table_ITT[[2]] = glucagon_ITT
rownames(Summary_Table_ITT[[2]]) = glucagon_vars
names(Summary_Table_ITT) = c("Islet Transit Time: Insulin", "Islet Transit Time: Glucagon")


#################################################
############ Islet Purity
# insulin secretion 
Summary_Table_IP = vector(mode='list', length=2)
#insulin_model_out = vector(mode='list', length=length(insulin_vars))

insulin_IP = NULL
for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], "~", vars_int[3], " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_IP = rbind(insulin_IP, temp1) 
}

insulin_IP = as.data.frame(insulin_IP)
insulin_IP$adj_pvalue = formatC(p.adjust(insulin_IP[,4], method='fdr'),
                                format = "e", digits = 3)
Summary_Table_IP[[1]] = insulin_IP
rownames(Summary_Table_IP[[1]]) = insulin_vars


# glucagon variables
#glucagon_model_out = vector(mode='list', length=length(glucagon_vars))
glucagon_IP = NULL
for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~", vars_int[3], " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  glucagon_IP = rbind(glucagon_IP, temp1) 
}

glucagon_IP = as.data.frame(glucagon_IP)
glucagon_IP$adj_pvalue = formatC(p.adjust(glucagon_IP[,4], method='fdr'),
                                 format = "e", digits = 3)
Summary_Table_IP[[2]] = glucagon_IP
rownames(Summary_Table_IP[[2]]) = glucagon_vars
names(Summary_Table_IP) = c("Islet Purity: Insulin", "Islet Purity: Glucagon")



# Assessing correlation between insulin and glucagon secretion traits, and gender.
#################################################
############ Gender
# insulin secretion 
Summary_Table_Gender = vector(mode='list', length=2)
#insulin_model_out = vector(mode='list', length=length(insulin_vars))

insulin_Gender = NULL
for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], "~", "Gender", " + Donor_HbA1c_s + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_Gender = rbind(insulin_Gender, temp1) 
}

insulin_Gender = as.data.frame(insulin_Gender)
insulin_Gender$adj_pvalue = formatC(p.adjust(insulin_Gender[,4], method='fdr'),
                                    format = "e", digits = 3)
Summary_Table_Gender[[1]] = insulin_Gender
rownames(Summary_Table_Gender[[1]]) = insulin_vars


# glucagon variables
#glucagon_model_out = vector(mode='list', length=length(glucagon_vars))

glucagon_Gender = NULL
for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~", "Gender", " + Donor_HbA1c_s + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  glucagon_Gender = rbind(glucagon_Gender, temp1) 
}

glucagon_Gender = as.data.frame(glucagon_Gender)
glucagon_Gender$adj_pvalue = formatC(p.adjust(glucagon_Gender[,4], method='fdr'),
                                     format = "e", digits = 3)
Summary_Table_Gender[[2]] = glucagon_Gender
rownames(Summary_Table_Gender[[2]]) = glucagon_vars

names(Summary_Table_Gender) = c("Gender: Insulin", "Gender: Glucagon")




# Summary Table

## Preshipment culture time
### Insulin secretion trait and preshipment culture time
Summary_Tab_PCT = Summary_Table_PCT[[1]][,-c(2,3)]
colnames(Summary_Tab_PCT) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_PCT


### Glucagon secretion trait and preshipment culture time
Summary_Tab_PCT = Summary_Table_PCT[[2]][,-c(2,3)]
colnames(Summary_Tab_PCT) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_PCT


## Islet Transit Time
### Insulin secretion trait and islet transit time
Summary_Tab_ITT = Summary_Table_ITT[[1]][,-c(2,3)]
colnames(Summary_Tab_ITT) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_ITT


### Glucagon secretion trait and islet transit time
Summary_Tab_ITT = Summary_Table_ITT[[2]][,-c(2,3)]
colnames(Summary_Tab_ITT) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_ITT


## Islet Purity
### Insulin secretion trait and islet purity
Summary_Tab_IP = Summary_Table_IP[[1]][,-c(2,3)]
colnames(Summary_Tab_IP) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_IP


### Glucagon secretion trait and islet purity
Summary_Tab_IP = Summary_Table_IP[[2]][,-c(2,3)]
colnames(Summary_Tab_IP) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_IP


## Gender
### Insulin secretion and gender
Summary_Tab_Gender = Summary_Table_Gender[[1]][,-c(2,3)]
colnames(Summary_Tab_Gender) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_Gender


### Glucagon secretion and gender
Summary_Tab_Gender = Summary_Table_Gender[[2]][,-c(2,3)]
colnames(Summary_Tab_Gender) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab_Gender


