#### Extended Data Figure 7 A&C islet processing traits####
#### Author: Ke Xu, Hakmook Kang ####

rm(list = ls())

# Load package 
library(dplyr)
library(kableExtra)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat <- gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")

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

cell_comp <- c("Beta_cell_pct","Alpha_cell_pct","Delta_cell_pct")

content_vars = c("Islet_Insulin_Content_ng_IEQ_s", "Islet_Glucagon_Content_pg_IEQ_s")

##### cell composition ~ BMI + other covariates #####

cellcomp.preship <- data.frame(cell_comp=cell_comp,
                               preship_coef=rep(NA,3), 
                               preship_pval=rep(NA,3), preship_adjpval=rep(NA,3))
cellcomp.transit <- data.frame(cell_comp=cell_comp,
                               transit_coef=rep(NA,3), 
                               transit_pval=rep(NA,3), transit_adjpval=rep(NA,3))

for (k in 1:3) {
  # model with center
  mod1 = as.formula(paste0(cell_comp[k],"~  PreShipmentCultureTime +  BMI_s + center + Donor_HbA1c_s + Gender + race2 + Age_years_s"))
  mod2 = as.formula(paste0(cell_comp[k],"~  IsletTransitTime +  BMI_s + center + Donor_HbA1c_s + Gender + race2+ Age_years_s"))
  
  fit1 <- lm(mod1, data=hipp)
  temp <- summary(fit1)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  
  cellcomp.preship$preship_coef[k] <- temp1[2,1]
  cellcomp.preship$preship_pval[k] <- temp1[2,4]
  
  fit2 <- lm(mod2, data=hipp)
  temp2 <- summary(fit2)
  temp1.1 <- round(temp2$coefficients,4)
  temp1.1 <- formatC(temp1.1, format="e", digits=3)
  cellcomp.transit$transit_coef[k] <- temp1.1[2,1]
  cellcomp.transit$transit_pval[k] <- temp1.1[2,4]
}

cellcomp.preship$preship_adjpval <- formatC(p.adjust(cellcomp.preship$preship_pval,method='fdr'),
                                            format="e",digits=3)
cellcomp.transit$transit_adjpval <- formatC(p.adjust(cellcomp.transit$transit_pval,method='fdr'),
                                            format="e",digits=3)

## Results & Summary

### Cell composition ~ Preshipment culture time
colnames(cellcomp.preship) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.preship


### Cell composition ~ transit time
colnames(cellcomp.transit) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.transit



# Hormone content ~ race+preshipment culture time+transit time + other covariates
##### hormone content ~ BMI + other covariates #####

INSGCGcont.preship <- data.frame(hormone_content=content_vars,
                                 preship_coef=rep(NA,2), 
                                 preship_pval=rep(NA,2), preship_adjpval=rep(NA,2))
INSGCGcont.transit <- data.frame(hormone_content=content_vars,
                                 transit_coef=rep(NA,2), transit_pval=rep(NA,2),
                                 transit_adjpval=rep(NA,2))


for (k in 1:2) {
  # model with center
  mod1 = as.formula(paste0(content_vars[k],"~ PreShipmentCultureTime +  race2 + center + Donor_HbA1c_s + Gender + Age_years_s + BMI_s"))
  
  mod2 = as.formula(paste0(content_vars[k],"~ IsletTransitTime  +  race2 + center + Donor_HbA1c_s + Gender + Age_years_s + BMI_s"))
  
  
  fit1 <- lm(mod1, data=hipp)
  temp <- summary(fit1)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  
  INSGCGcont.preship$preship_coef[k] <- temp1[2,1]
  INSGCGcont.preship$preship_pval[k] <- temp1[2,4]
  
  fit2 <- lm(mod2, data=hipp)
  temp2 <- summary(fit2)
  temp1.1 <- round(temp2$coefficients,4)
  temp1.1 <- formatC(temp1.1, format="e", digits=3)
  
  INSGCGcont.transit$transit_coef[k] <- temp1.1[2,1]
  INSGCGcont.transit$transit_pval[k] <- temp1.1[2,4]
  
  
}

INSGCGcont.preship$preship_adjpval <- formatC(p.adjust(INSGCGcont.preship$preship_pval,method='fdr'),
                                              format="e",digits=3)
INSGCGcont.transit$transit_adjpval <- formatC(p.adjust(INSGCGcont.transit$transit_pval,method='fdr'),
                                              format="e",digits=3)


## Results & Summary

### Hormone content ~ Preshipment culture time
colnames(INSGCGcont.preship) <- c("Hormone Content","Coefficient","P-value","Adjusted P-value")
INSGCGcont.preship




### Hormone content ~ Transit time
colnames(INSGCGcont.transit) <- c("Hormone Content","Coefficient","P-value","Adjusted P-value")
INSGCGcont.transit


