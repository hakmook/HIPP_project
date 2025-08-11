#### Extended Data Figure 7 A&C####
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

content_vars = c("Islet_Insulin_Content_ng_IEQ_s", "Islet_Glucagon_Content_pg_IEQ_s")

cell_comp <- c("Beta_cell_pct","Alpha_cell_pct","Delta_cell_pct")




###############################################################################################
##### cell composition ~ BMI_s + other covariates #####
# Cell composition ~ age + pre-shipment culture time + transit time + other covariates
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


##### hormone content ~ BMI_s + other covariates #####
# Hormone content ~ race+preshipment culture time+transit time + other covariates
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



###############################################################################################
##### cell composition ~ BMI_s + other covariates #####
cellcomp.BMI_s <- data.frame(cell_comp=cell_comp,
                           BMI_s_coef=rep(NA,3), BMI_s_pval=rep(NA,3), BMI_s_adjpval=rep(NA,3))
cellcomp.HbA1c <- data.frame(cell_comp=cell_comp,
                             HbA1c_coef=rep(NA,3), 
                             HbA1c_pval=rep(NA,3), HbA1c_adjpval=rep(NA,3))
cellcomp.center <- data.frame(cell_comp=cell_comp,
                              center_pval=rep(NA,3),center_adjpval=rep(NA,3))
cellcomp.Age <- data.frame(cell_comp=cell_comp,
                           Age_coef=rep(NA,3), Age_pval=rep(NA,3), Age_adjpval=rep(NA,3))

for (k in 1:3) {
  # model with center
  mod = as.formula(paste0(cell_comp[k],"~ BMI_s + center + Donor_HbA1c_s + Gender + race2 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit <- lm(mod, data=hipp)
  temp <- summary(fit)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  cellcomp.BMI_s$BMI_s_coef[k] <- temp1[2,1]
  cellcomp.BMI_s$BMI_s_pval[k] <- temp1[2,4]
  
  Age.idx <- which(rownames(temp1)=="Age_years_s")
  cellcomp.Age$Age_coef[k] <- temp1[Age.idx,1]
  cellcomp.Age$Age_pval[k] <- temp1[Age.idx,4]
  
  A1c.idx <- which(rownames(temp1)=="Donor_HbA1c_s")
  cellcomp.HbA1c$HbA1c_coef[k] <- temp1[A1c.idx,1]
  cellcomp.HbA1c$HbA1c_pval[k] <- temp1[A1c.idx,4]
  
  # model without center
  mod.noCenter <- as.formula(paste0(cell_comp[k],"~ BMI_s + Donor_HbA1c_s + Gender + race2 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter, fit)
  cellcomp.center$center_pval[k] <- temp.center$`Pr(>F)`[2]
}

cellcomp.BMI_s$BMI_s_adjpval <- formatC(p.adjust(cellcomp.BMI_s$BMI_s_pval,method='fdr'),
                                    format="e",digits=3)
cellcomp.HbA1c$HbA1c_adjpval <- formatC(p.adjust(cellcomp.HbA1c$HbA1c_pval,method='fdr'),
                                        format="e",digits=3)
cellcomp.center$center_adjpval <- formatC(p.adjust(cellcomp.center$center_pval,method='fdr'),
                                          format="e",digits=3)
cellcomp.Age$Age_adjpval <- formatC(p.adjust(cellcomp.Age$Age_pval,method='fdr'),
                                    format="e",digits=3)

### Cell composition ~ BMI_s
colnames(cellcomp.BMI_s) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.BMI_s

### Cell composition ~ Center
colnames(cellcomp.center) <- c("Cell composition","P-value","Adjusted P-value")
cellcomp.center

### Cell composition ~ HbA1c
colnames(cellcomp.HbA1c) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.HbA1c

### Cell composition ~ Age
colnames(cellcomp.Age) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.Age


# Hormone content ~ BMI_s/center/hemoglobinA1C + other covariates
##### hormone content ~ BMI_s + other covariates #####
INSGCGcont.BMI_s <- data.frame(hormone_content=content_vars,
                             BMI_s_coef=rep(NA,2), BMI_s_pval=rep(NA,2), BMI_s_adjpval=rep(NA,2))
INSGCGcont.HbA1c <- data.frame(hormone_content=content_vars,
                               HbA1c_coef=rep(NA,2), 
                               HbA1c_pval=rep(NA,2), HbA1c_adjpval=rep(NA,2))
INSGCGcont.center <- data.frame(hormone_content=content_vars,
                                center_pval=rep(NA,2),center_adjpval=rep(NA,2))
INSGCGcont.age <- data.frame(hormone_content=content_vars,
                             age_coef=rep(NA,2), age_pval=rep(NA,2), age_adjpval=rep(NA,2))
INSGCGcont.sex <- data.frame(hormone_content=content_vars,
                             sex_coef=rep(NA,2), sex_pval=rep(NA,2), sex_adjpval=rep(NA,2))

for (k in 1:2) {
  # model with center
  mod = as.formula(paste0(content_vars[k],"~ BMI_s + center + Donor_HbA1c_s + Gender + race2 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit <- lm(mod, data=hipp)
  temp <- summary(fit)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  INSGCGcont.BMI_s$BMI_s_coef[k] <- temp1[2,1]
  INSGCGcont.BMI_s$BMI_s_pval[k] <- temp1[2,4]
  A1c.idx <- which(rownames(temp1)=="Donor_HbA1c_s")
  INSGCGcont.HbA1c$HbA1c_coef[k] <- temp1[A1c.idx,1]
  INSGCGcont.HbA1c$HbA1c_pval[k] <- temp1[A1c.idx,4]
  age.idx <- which(rownames(temp1)=="Age_years_s")
  INSGCGcont.age$age_coef[k] <- temp1[age.idx,1]
  INSGCGcont.age$age_pval[k] <- temp1[age.idx,4]
  sex.idx <- which(rownames(temp1)=="GenderMale")
  INSGCGcont.sex$sex_coef[k] <- temp1[sex.idx,1]
  INSGCGcont.sex$sex_pval[k] <- temp1[sex.idx,4]
  
  # model without center
  mod.noCenter <- as.formula(paste0(content_vars[k],"~ BMI_s + Donor_HbA1c_s + Gender + race2 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter, fit)
  INSGCGcont.center$center_pval[k] <- temp.center$`Pr(>F)`[2]
}

INSGCGcont.BMI_s$BMI_s_adjpval <- formatC(p.adjust(INSGCGcont.BMI_s$BMI_s_pval,method='fdr'),
                                      format="e",digits=3)
INSGCGcont.HbA1c$HbA1c_adjpval <- formatC(p.adjust(INSGCGcont.HbA1c$HbA1c_pval,method='fdr'),
                                          format="e",digits=3)
INSGCGcont.center$center_adjpval <- formatC(p.adjust(INSGCGcont.center$center_pval,method='fdr'),
                                            format="e",digits=3)
INSGCGcont.age$age_adjpval <- formatC(p.adjust(INSGCGcont.age$age_pval,method='fdr'),
                                      format="e",digits=3)
INSGCGcont.sex$sex_adjpval <- formatC(p.adjust(INSGCGcont.sex$sex_pval,method='fdr'),
                                      format="e",digits=3)
## Results & Summary
### Hormone content ~ BMI_s
colnames(INSGCGcont.BMI_s) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.BMI_s

### Hormone content ~ Center
colnames(INSGCGcont.center) <- c("Cell composition","P-value","Adjusted P-value")
INSGCGcont.center

### Hormone content ~ HbA1c
colnames(INSGCGcont.HbA1c) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.HbA1c

### Hormone content ~ Age
colnames(INSGCGcont.age) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.age

### Hormone content ~ Sex
colnames(INSGCGcont.sex) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.sex