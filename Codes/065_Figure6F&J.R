#### Figure 6F&J, Cell composition vs. age, pre-shipment culture time, transit time; Hormone content vs. race, pre-shipment culture time, transit time ####
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



############ Hormone content ~ BMI/center/hemoglobinA1C + other covariates ########
INSGCG.content <- c("Islet_Insulin_Content_ng_IEQ_s",
                    "Islet_Glucagon_Content_pg_IEQ_s")


##### hormone content ~ BMI + other covariates #####
INSGCGcont.BMI <- data.frame(hormone_content=INSGCG.content,
                             BMI_coef=rep(NA,2), BMI_pval=rep(NA,2), BMI_adjpval=rep(NA,2))
INSGCGcont.HbA1c <- data.frame(hormone_content=INSGCG.content,
                               HbA1c_coef=rep(NA,2), 
                               HbA1c_pval=rep(NA,2), HbA1c_adjpval=rep(NA,2))
INSGCGcont.center <- data.frame(hormone_content=INSGCG.content,
                                center_pval=rep(NA,2),center_adjpval=rep(NA,2))
INSGCGcont.age <- data.frame(hormone_content=INSGCG.content,
                             age_coef=rep(NA,2), age_pval=rep(NA,2), age_adjpval=rep(NA,2))
INSGCGcont.sex <- data.frame(hormone_content=INSGCG.content,
                             sex_coef=rep(NA,2), sex_pval=rep(NA,2), sex_adjpval=rep(NA,2))

for (k in 1:2) {
  # model with center
  mod = as.formula(paste0(INSGCG.content[k],"~ BMI_s + center + Donor_HbA1c + Gender + race2 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit <- lm(mod, data=hipp)
  temp <- summary(fit)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  INSGCGcont.BMI$BMI_coef[k] <- temp1[2,1]
  INSGCGcont.BMI$BMI_pval[k] <- temp1[2,4]
  A1c.idx <- which(rownames(temp1)=="Donor_HbA1c")
  INSGCGcont.HbA1c$HbA1c_coef[k] <- temp1[A1c.idx,1]
  INSGCGcont.HbA1c$HbA1c_pval[k] <- temp1[A1c.idx,4]
  age.idx <- which(rownames(temp1)=="Age_years_s")
  INSGCGcont.age$age_coef[k] <- temp1[age.idx,1]
  INSGCGcont.age$age_pval[k] <- temp1[age.idx,4]
  sex.idx <- which(rownames(temp1)=="GenderMale")
  INSGCGcont.sex$sex_coef[k] <- temp1[sex.idx,1]
  INSGCGcont.sex$sex_pval[k] <- temp1[sex.idx,4]
  
  # model without center
  mod.noCenter <- as.formula(paste0(INSGCG.content[k],"~ BMI_s + Donor_HbA1c + Gender + race2 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter, fit)
  INSGCGcont.center$center_pval[k] <- temp.center$`Pr(>F)`[2]
}

INSGCGcont.BMI$BMI_adjpval <- formatC(p.adjust(INSGCGcont.BMI$BMI_pval,method='fdr'),
                                      format="e",digits=3)
INSGCGcont.HbA1c$HbA1c_adjpval <- formatC(p.adjust(INSGCGcont.HbA1c$HbA1c_pval,method='fdr'),
                                          format="e",digits=3)
INSGCGcont.center$center_adjpval <- formatC(p.adjust(INSGCGcont.center$center_pval,method='fdr'),
                                            format="e",digits=3)
INSGCGcont.age$age_adjpval <- formatC(p.adjust(INSGCGcont.age$age_pval,method='fdr'),
                                      format="e",digits=3)
INSGCGcont.sex$sex_adjpval <- formatC(p.adjust(INSGCGcont.sex$sex_pval,method='fdr'),
                                      format="e",digits=3)



colnames(INSGCGcont.BMI) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.BMI

colnames(INSGCGcont.center) <- c("Cell composition","P-value","Adjusted P-value")
INSGCGcont.center

colnames(INSGCGcont.HbA1c) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.HbA1c

colnames(INSGCGcont.age) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.age

colnames(INSGCGcont.sex) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
INSGCGcont.sex