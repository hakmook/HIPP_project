#### Figure 6F&J, Cell composition/hormone content vs. BMI, center and hemoglobin A1C (scaled) ####
#### Author: Ke Xu, Hakmook Kang ####

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

# new data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
hipp = merge(gen_dat, hipp, by="DONOR_RRID")



############ Cell composition ~ BMI/center/hemoglobinA1C + other covariates ########
cell_comp <- c("Beta_cell_pct","Alpha_cell_pct","Delta_cell_pct")


cellcomp.BMI <- data.frame(cell_comp=cell_comp,
                           BMI_coef=rep(NA,3), BMI_pval=rep(NA,3), BMI_adjpval=rep(NA,3))
cellcomp.HbA1c <- data.frame(cell_comp=cell_comp,
                             HbA1c_coef=rep(NA,3), 
                             HbA1c_pval=rep(NA,3), HbA1c_adjpval=rep(NA,3))
cellcomp.center <- data.frame(cell_comp=cell_comp,
                              center_pval=rep(NA,3),center_adjpval=rep(NA,3))
cellcomp.Age <- data.frame(cell_comp=cell_comp,
                           Age_coef=rep(NA,3), Age_pval=rep(NA,3), Age_adjpval=rep(NA,3))

for (k in 1:3) {
  # model with center
  mod = as.formula(paste0(cell_comp[k],"~ BMI_s + center + Donor_HbA1c_s + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit <- lm(mod, data=hipp)
  temp <- summary(fit)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  cellcomp.BMI$BMI_coef[k] <- temp1[2,1]
  cellcomp.BMI$BMI_pval[k] <- temp1[2,4]
  
  Age.idx <- which(rownames(temp1)=="Age_years_s")
  cellcomp.Age$Age_coef[k] <- temp1[Age.idx,1]
  cellcomp.Age$Age_pval[k] <- temp1[Age.idx,4]
  
  A1c.idx <- which(rownames(temp1)=="Donor_HbA1c_s")
  cellcomp.HbA1c$HbA1c_coef[k] <- temp1[A1c.idx,1]
  cellcomp.HbA1c$HbA1c_pval[k] <- temp1[A1c.idx,4]
  
  # model without center
  mod.noCenter <- as.formula(paste0(cell_comp[k],"~ BMI_s + Donor_HbA1c_s + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter, fit)
  cellcomp.center$center_pval[k] <- temp.center$`Pr(>F)`[2]
}

cellcomp.BMI$BMI_adjpval <- formatC(p.adjust(cellcomp.BMI$BMI_pval,method='fdr'),
                                    format="e",digits=3)
cellcomp.HbA1c$HbA1c_adjpval <- formatC(p.adjust(cellcomp.HbA1c$HbA1c_pval,method='fdr'),
                                        format="e",digits=3)
cellcomp.center$center_adjpval <- formatC(p.adjust(cellcomp.center$center_pval,method='fdr'),
                                          format="e",digits=3)
cellcomp.Age$Age_adjpval <- formatC(p.adjust(cellcomp.Age$Age_pval,method='fdr'),
                                    format="e",digits=3)

colnames(cellcomp.BMI) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.BMI

colnames(cellcomp.center) <- c("Cell composition","P-value","Adjusted P-value")
cellcomp.center

colnames(cellcomp.HbA1c) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.HbA1c

colnames(cellcomp.Age) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.Age



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
  mod = as.formula(paste0(INSGCG.content[k],"~ BMI_s + center + Donor_HbA1c_s + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
  fit <- lm(mod, data=hipp)
  temp <- summary(fit)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  INSGCGcont.BMI$BMI_coef[k] <- temp1[2,1]
  INSGCGcont.BMI$BMI_pval[k] <- temp1[2,4]
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
  mod.noCenter <- as.formula(paste0(INSGCG.content[k],"~ BMI_s + Donor_HbA1c_s + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + PreShipmentCultureTime + IsletTransitTime"))
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