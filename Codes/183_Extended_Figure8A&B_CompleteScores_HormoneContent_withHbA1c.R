#### Extended Data Figure 8, Complete scores - Hormone content, Hormone content with GRS T1D and T2D UPDATED without HbA1c (scaled) ####
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


dat_all$HLA.DR.DQ = scale(dat_all$HLA.DR.DQ)
dat_all$HLA.Class.1 = scale(dat_all$HLA.Class.1)
dat_all$HLA.Class.2 = scale(dat_all$HLA.Class.2)
dat_all$Non.HLA = scale(dat_all$Non.HLA)
dat_all$T1D.GRS = scale(dat_all$T1D.GRS)
dat_all$BETA_CELL = scale(dat_all$BETA_CELL)
dat_all$PROINSULIN = scale(dat_all$PROINSULIN)
dat_all$OBESITY = scale(dat_all$OBESITY)
dat_all$LIPODYSTROPHY = scale(dat_all$LIPODYSTROPHY)
dat_all$LIVER_LIPID = scale(dat_all$LIVER_LIPID)
dat_all$T2D.GRS = scale(dat_all$T2D.GRS)
dat_all$PC1 = scale(dat_all$PC1)
dat_all$PC2 = scale(dat_all$PC2)
dat_all$PC3 = scale(dat_all$PC3)
dat_all$PC4 = scale(dat_all$PC4)
dat_all$PC5 = scale(dat_all$PC5)


content_vars = c("Islet_Insulin_Content_ng_IEQ_s", "Islet_Glucagon_Content_pg_IEQ_s")



#################################################
#################################################
############ GRS T1D & T2D
# GRS T1D and hormone contents
Summary_Table = vector(mode='list', length=2)
model_out = vector(mode='list', length=length(content_vars))

temp_summary = NULL
for (m in 1:length(content_vars)){
  model = as.formula(paste0(content_vars[m], "~ T1D.GRS + Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime + PC1 + PC2 + PC3 + PC4 + PC5"))
  fit = lm(model, data = dat_all)
  model_out[[m]] = fit 
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

rownames(Summary_Table[[1]]) = content_vars

names(model_out) <- content_vars


# GRS T2D and hormone contents
model_out_g = vector(mode='list', length=length(content_vars))

temp_summary = NULL
for (m in 1:length(content_vars)){
  model = as.formula(paste0(content_vars[m], "~ T2D.GRS + Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime + PC1 + PC2 + PC3 + PC4 + PC5"))
  fit = lm(model, data = dat_all)
  model_out_g[[m]] = fit
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

rownames(Summary_Table[[2]]) = content_vars

names(Summary_Table) = c("GRS T1D:cell comp", "GRS T2D:cell comp")

names(model_out_g) = content_vars



########################
# Summary Table

## GRS T1D and hormone contents
Summary_Tab = Summary_Table[[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab

## GRS T2D and hormone contents
Summary_Tab = Summary_Table[[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab

