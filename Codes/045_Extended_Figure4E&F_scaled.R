#### Extended Data Figure 4E&F, Secretion traits vs age/BMI_s/center, vs hormone content + cell composition, vs hormone content + cell composition + hormone content * cell composition (scaled)####
#### Author: Ke Xu, Danni Shi, Hakmook Kang ####

# Load package 
library(dplyr)
library(readxl)


################ Import Data ################
# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp


## 11 insulin secretion traits
insulin_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
                  "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
                  "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
                  "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

## 9 glucagon secretion traits
glucagon_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
                   "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
                   "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
                   "GCG_KCl_20_SI")

# Secretion vs age/BMI_s/centers, controlling for race, sex, HbA1c, Pre-shipment culture time, and Islet transit time.
##### Insulin secretion  #####
INSsec.nullmod.sumtab <-  vector(mode='list', length=3) # summary table

tmp.INSsec.age.sum <- NULL
tmp.INSsec.BMI_s.sum <- NULL
tmp.INSsec.center.sum <- NULL
for (m in 1:length(insulin_vars)) {
  model = as.formula(paste0(insulin_vars[m],"~ + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  # age
  temp.age = temp$coefficients[which(rownames(temp$coefficients)=="Age_years_s"),]
  temp.age[1:3] = round(temp.age[1:3], 4)
  temp.age[4] = formatC(temp.age[4], format = "e", digits = 3)
  tmp.INSsec.age.sum = rbind(tmp.INSsec.age.sum, temp.age) 
  # BMI_s
  temp.BMI_s = temp$coefficients[which(rownames(temp$coefficients)=="BMI_s"),]
  temp.BMI_s[1:3] = round(temp.BMI_s[1:3], 4)
  temp.BMI_s[4] = formatC(temp.BMI_s[4], format = "e", digits = 3)
  tmp.INSsec.BMI_s.sum = rbind(tmp.INSsec.BMI_s.sum, temp.BMI_s) 
  
  # center
  mod.noCenter <- as.formula(paste0(insulin_vars[m],"~ + Donor_HbA1c_s + Gender + race2 + Age_years_s + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter,fit)
  tmp.INSsec.center.sum <- rbind(tmp.INSsec.center.sum,
                                 temp.center$`Pr(>F)`[2])
}
tmp.INSsec.age.sum <- as.data.frame(tmp.INSsec.age.sum)
tmp.INSsec.age.sum$adj_pvalue = formatC(p.adjust(tmp.INSsec.age.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.INSsec.BMI_s.sum <- as.data.frame(tmp.INSsec.BMI_s.sum)
tmp.INSsec.BMI_s.sum$adj_pvalue = formatC(p.adjust(tmp.INSsec.BMI_s.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.INSsec.center.sum <- as.data.frame(tmp.INSsec.center.sum)
tmp.INSsec.center.sum$adj_pvalue = formatC(p.adjust(tmp.INSsec.center.sum[,1],
                                                    method='fdr'),
                                           format="e", digits=3)
colnames(tmp.INSsec.center.sum)[1] <- "p-value"

INSsec.nullmod.sumtab[[1]] = tmp.INSsec.age.sum
INSsec.nullmod.sumtab[[2]] = tmp.INSsec.BMI_s.sum
INSsec.nullmod.sumtab[[3]] = tmp.INSsec.center.sum
rownames(INSsec.nullmod.sumtab[[1]]) <- rownames(INSsec.nullmod.sumtab[[2]]) <- 
  rownames(INSsec.nullmod.sumtab[[3]]) <- insulin_vars
names(INSsec.nullmod.sumtab) <- c("Age","BMI_s","Center")



##### Glucagon secretion #####
GCGsec.nullmod.sumtab <-  vector(mode='list', length=3) # summary table

tmp.GCGsec.age.sum <- NULL
tmp.GCGsec.BMI_s.sum <- NULL
tmp.GCGsec.center.sum <- NULL
for (m in 1:length(glucagon_vars)) {
  model = as.formula(paste0(glucagon_vars[m],"~ + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  # age
  temp.age = temp$coefficients[which(rownames(temp$coefficients)=="Age_years_s"),]
  temp.age[1:3] = round(temp.age[1:3], 4)
  temp.age[4] = formatC(temp.age[4], format = "e", digits = 3)
  tmp.GCGsec.age.sum = rbind(tmp.GCGsec.age.sum, temp.age) 
  # BMI_s
  temp.BMI_s = temp$coefficients[which(rownames(temp$coefficients)=="BMI_s"),]
  temp.BMI_s[1:3] = round(temp.BMI_s[1:3], 4)
  temp.BMI_s[4] = formatC(temp.BMI_s[4], format = "e", digits = 3)
  tmp.GCGsec.BMI_s.sum = rbind(tmp.GCGsec.BMI_s.sum, temp.BMI_s) 
  
  # center
  mod.noCenter <- as.formula(paste0(glucagon_vars[m],"~ + Donor_HbA1c_s + Gender + race2 + Age_years_s + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter,fit)
  tmp.GCGsec.center.sum <- rbind(tmp.GCGsec.center.sum,
                                 temp.center$`Pr(>F)`[2])
}
tmp.GCGsec.age.sum <- as.data.frame(tmp.GCGsec.age.sum)
tmp.GCGsec.age.sum$adj_pvalue = formatC(p.adjust(tmp.GCGsec.age.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.GCGsec.BMI_s.sum <- as.data.frame(tmp.GCGsec.BMI_s.sum)
tmp.GCGsec.BMI_s.sum$adj_pvalue = formatC(p.adjust(tmp.GCGsec.BMI_s.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.GCGsec.center.sum <- as.data.frame(tmp.GCGsec.center.sum)
tmp.GCGsec.center.sum$adj_pvalue = formatC(p.adjust(tmp.GCGsec.center.sum[,1],
                                                    method='fdr'),
                                           format="e", digits=3)
colnames(tmp.GCGsec.center.sum)[1] <- "p-value"

GCGsec.nullmod.sumtab[[1]] = tmp.GCGsec.age.sum
GCGsec.nullmod.sumtab[[2]] = tmp.GCGsec.BMI_s.sum
GCGsec.nullmod.sumtab[[3]] = tmp.GCGsec.center.sum
rownames(GCGsec.nullmod.sumtab[[1]]) <- rownames(GCGsec.nullmod.sumtab[[2]]) <- 
  rownames(GCGsec.nullmod.sumtab[[3]]) <- glucagon_vars
names(GCGsec.nullmod.sumtab) <- c("Age","BMI_s","Center")




## Results & Summary Table
### Insulin secretion
#### Insulin secretion with age
INSsec.age.sumtb <- INSsec.nullmod.sumtab[[1]][,-c(2,3)]
colnames(INSsec.age.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
INSsec.age.sumtb

#### Insulin secretion with BMI_s
INSsec.BMI_s.sumtb <- INSsec.nullmod.sumtab[[2]][,-c(2,3)]
colnames(INSsec.BMI_s.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
INSsec.BMI_s.sumtb

#### Insulin secretion with center
# Since `center` is a factor/categorical variable with multiple levels, here we use F-test to test whether the `center` variable is significant, and we only include the p-value and the adjusted p-value from F-test.
colnames(INSsec.nullmod.sumtab[[3]]) <- c("P-value","Adjusted P-value")
INSsec.nullmod.sumtab[[3]]



### Glucagon secretion

#### Glucagon secretion with age
GCGsec.age.sumtb <- GCGsec.nullmod.sumtab[[1]][,-c(2,3)]
colnames(GCGsec.age.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
GCGsec.age.sumtb

#### Glucagon secretion with BMI_s
GCGsec.BMI_s.sumtb <- GCGsec.nullmod.sumtab[[2]][,-c(2,3)]
colnames(GCGsec.BMI_s.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
GCGsec.BMI_s.sumtb

#### Glucagon secretion with center
colnames(GCGsec.nullmod.sumtab[[3]]) <- c("P-value","Adjusted P-value")
GCGsec.nullmod.sumtab[[3]]





