#### Figure 6G&K, Cell composition vs. age, pre-shipment culture time, transit time; Hormone content vs. race, pre-shipment culture time, transit time ####
#### Author: Ke Xu, Hakmook Kang ####

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

##### cell composition ~ BMI + other covariates #####
cell_comp <- c("Beta_cell_pct","Alpha_cell_pct","Delta_cell_pct")
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

colnames(cellcomp.preship) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.preship

colnames(cellcomp.transit) <- c("Cell composition","P-value","Adjusted P-value")
cellcomp.transit



############ Hormone content ~ race+preshipment culture time+transit time + other covariates ########
INSGCG.content <- c("Islet_Insulin_Content_ng_IEQ_s",
                    "Islet_Glucagon_Content_pg_IEQ_s")


##### hormone content ~ BMI + other covariates #####

INSGCGcont.preship <- data.frame(hormone_content=INSGCG.content,
                                 preship_coef=rep(NA,2), 
                                 preship_pval=rep(NA,2), preship_adjpval=rep(NA,2))
INSGCGcont.transit <- data.frame(hormone_content=INSGCG.content,
                                 transit_coef=rep(NA,2), transit_pval=rep(NA,2),
                                 transit_adjpval=rep(NA,2))


for (k in 1:2) {
  # model with center
  mod1 = as.formula(paste0(INSGCG.content[k],"~ PreShipmentCultureTime +  race2 + center + Donor_HbA1c_s + Gender + Age_years_s + BMI_s"))
  
  mod2 = as.formula(paste0(INSGCG.content[k],"~ IsletTransitTime +  race2 + center + Donor_HbA1c_s + Gender + Age_years_s + BMI_s"))
  
  
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

colnames(INSGCGcont.preship) <- c("Hormone Content","Coefficient","P-value","Adjusted P-value")
INSGCGcont.preship

colnames(INSGCGcont.transit) <- c("Hormone Content","Coefficient","P-value","Adjusted P-value")
INSGCGcont.transit