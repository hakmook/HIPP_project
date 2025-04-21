#### Extended Data Figure 4E&F, Secretion traits vs age/BMI/center, vs hormone content + cell composition, vs hormone content + cell composition + hormone content * cell composition ####
#### Author: Ke Xu, Danni Shi, Hakmook Kang ####

# Load package 
library(dplyr)
library(readxl)


################ Import Data ################
# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta_noscale.RData")
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

# Secretion vs age/BMI/centers, controlling for race, sex, HbA1c, Pre-shipment culture time, and Islet transit time.
##### Insulin secretion  #####
INSsec.nullmod.sumtab <-  vector(mode='list', length=3) # summary table

tmp.INSsec.age.sum <- NULL
tmp.INSsec.BMI.sum <- NULL
tmp.INSsec.center.sum <- NULL
for (m in 1:length(insulin_vars)) {
  model = as.formula(paste0(insulin_vars[m],"~ + Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  # age
  temp.age = temp$coefficients[which(rownames(temp$coefficients)=="Age_years"),]
  temp.age[1:3] = round(temp.age[1:3], 4)
  temp.age[4] = formatC(temp.age[4], format = "e", digits = 3)
  tmp.INSsec.age.sum = rbind(tmp.INSsec.age.sum, temp.age) 
  # BMI
  temp.BMI = temp$coefficients[which(rownames(temp$coefficients)=="BMI"),]
  temp.BMI[1:3] = round(temp.BMI[1:3], 4)
  temp.BMI[4] = formatC(temp.BMI[4], format = "e", digits = 3)
  tmp.INSsec.BMI.sum = rbind(tmp.INSsec.BMI.sum, temp.BMI) 
  
  # center
  mod.noCenter <- as.formula(paste0(insulin_vars[m],"~ + Donor_HbA1c + Gender + race2 + Age_years + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter,fit)
  tmp.INSsec.center.sum <- rbind(tmp.INSsec.center.sum,
                                 temp.center$`Pr(>F)`[2])
}
tmp.INSsec.age.sum <- as.data.frame(tmp.INSsec.age.sum)
tmp.INSsec.age.sum$adj_pvalue = formatC(p.adjust(tmp.INSsec.age.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.INSsec.BMI.sum <- as.data.frame(tmp.INSsec.BMI.sum)
tmp.INSsec.BMI.sum$adj_pvalue = formatC(p.adjust(tmp.INSsec.BMI.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.INSsec.center.sum <- as.data.frame(tmp.INSsec.center.sum)
tmp.INSsec.center.sum$adj_pvalue = formatC(p.adjust(tmp.INSsec.center.sum[,1],
                                                    method='fdr'),
                                           format="e", digits=3)
colnames(tmp.INSsec.center.sum)[1] <- "p-value"

INSsec.nullmod.sumtab[[1]] = tmp.INSsec.age.sum
INSsec.nullmod.sumtab[[2]] = tmp.INSsec.BMI.sum
INSsec.nullmod.sumtab[[3]] = tmp.INSsec.center.sum
rownames(INSsec.nullmod.sumtab[[1]]) <- rownames(INSsec.nullmod.sumtab[[2]]) <- 
  rownames(INSsec.nullmod.sumtab[[3]]) <- insulin_vars
names(INSsec.nullmod.sumtab) <- c("Age","BMI","Center")



##### Glucagon secretion #####
GCGsec.nullmod.sumtab <-  vector(mode='list', length=3) # summary table

tmp.GCGsec.age.sum <- NULL
tmp.GCGsec.BMI.sum <- NULL
tmp.GCGsec.center.sum <- NULL
for (m in 1:length(glucagon_vars)) {
  model = as.formula(paste0(glucagon_vars[m],"~ + Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  # age
  temp.age = temp$coefficients[which(rownames(temp$coefficients)=="Age_years"),]
  temp.age[1:3] = round(temp.age[1:3], 4)
  temp.age[4] = formatC(temp.age[4], format = "e", digits = 3)
  tmp.GCGsec.age.sum = rbind(tmp.GCGsec.age.sum, temp.age) 
  # BMI
  temp.BMI = temp$coefficients[which(rownames(temp$coefficients)=="BMI"),]
  temp.BMI[1:3] = round(temp.BMI[1:3], 4)
  temp.BMI[4] = formatC(temp.BMI[4], format = "e", digits = 3)
  tmp.GCGsec.BMI.sum = rbind(tmp.GCGsec.BMI.sum, temp.BMI) 
  
  # center
  mod.noCenter <- as.formula(paste0(glucagon_vars[m],"~ + Donor_HbA1c + Gender + race2 + Age_years + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
  fit.noCenter <- lm(mod.noCenter, data=hipp)
  temp.center <- anova(fit.noCenter,fit)
  tmp.GCGsec.center.sum <- rbind(tmp.GCGsec.center.sum,
                                 temp.center$`Pr(>F)`[2])
}
tmp.GCGsec.age.sum <- as.data.frame(tmp.GCGsec.age.sum)
tmp.GCGsec.age.sum$adj_pvalue = formatC(p.adjust(tmp.GCGsec.age.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.GCGsec.BMI.sum <- as.data.frame(tmp.GCGsec.BMI.sum)
tmp.GCGsec.BMI.sum$adj_pvalue = formatC(p.adjust(tmp.GCGsec.BMI.sum[,4],
                                                 method='fdr'),
                                        format="e", digits=3)
tmp.GCGsec.center.sum <- as.data.frame(tmp.GCGsec.center.sum)
tmp.GCGsec.center.sum$adj_pvalue = formatC(p.adjust(tmp.GCGsec.center.sum[,1],
                                                    method='fdr'),
                                           format="e", digits=3)
colnames(tmp.GCGsec.center.sum)[1] <- "p-value"

GCGsec.nullmod.sumtab[[1]] = tmp.GCGsec.age.sum
GCGsec.nullmod.sumtab[[2]] = tmp.GCGsec.BMI.sum
GCGsec.nullmod.sumtab[[3]] = tmp.GCGsec.center.sum
rownames(GCGsec.nullmod.sumtab[[1]]) <- rownames(GCGsec.nullmod.sumtab[[2]]) <- 
  rownames(GCGsec.nullmod.sumtab[[3]]) <- glucagon_vars
names(GCGsec.nullmod.sumtab) <- c("Age","BMI","Center")




## Results & Summary Table
### Insulin secretion
#### Insulin secretion with age
INSsec.age.sumtb <- INSsec.nullmod.sumtab[[1]][,-c(2,3)]
colnames(INSsec.age.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
INSsec.age.sumtb

#### Insulin secretion with BMI
INSsec.BMI.sumtb <- INSsec.nullmod.sumtab[[2]][,-c(2,3)]
colnames(INSsec.BMI.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
INSsec.BMI.sumtb

#### Insulin secretion with center
# Since `center` is a factor/categorical variable with multiple levels, here we use F-test to test whether the `center` variable is significant, and we only include the p-value and the adjusted p-value from F-test.
colnames(INSsec.nullmod.sumtab[[3]]) <- c("P-value","Adjusted P-value")
INSsec.nullmod.sumtab[[3]]



### Glucagon secretion

#### Glucagon secretion with age
GCGsec.age.sumtb <- GCGsec.nullmod.sumtab[[1]][,-c(2,3)]
colnames(GCGsec.age.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
GCGsec.age.sumtb

#### Glucagon secretion with BMI
GCGsec.BMI.sumtb <- GCGsec.nullmod.sumtab[[2]][,-c(2,3)]
colnames(GCGsec.BMI.sumtb) <- c("Coefficient","P-value","Adjusted P-value")
GCGsec.BMI.sumtb

#### Glucagon secretion with center
colnames(GCGsec.nullmod.sumtab[[3]]) <- c("P-value","Adjusted P-value")
GCGsec.nullmod.sumtab[[3]]





#####################################################################################################################################################################
#####################################################################################################################################################################
# Secretion vs hormone content + cell composition + 8 covariates
INSGCG.content <- c("Islet_Insulin_Content_ng_IEQ",
                    "Islet_Glucagon_Content_pg_IEQ")
cell.comp <- c("Beta_Cells","Alpha_Cells","Delta_Cells")

##### Insulin secretion vs hormone content + cells  #####
## insulin secretion ~ insulin content + cell composition + 8 covariates
INSsec.inscont.cell <- data.frame(secretion=rep(insulin_vars,each=3),
                                  hormone_content=rep(INSGCG.content[1],3*length(insulin_vars)),
                                  cell_comp=rep(cell.comp,length(insulin_vars)),
                                  hormone_cont_coef=rep(NA,3*length(insulin_vars)),
                                  hormone_cont_pval=rep(NA,3*length(insulin_vars)),
                                  hormone_cont_adjpval=rep(NA,3*length(insulin_vars)),
                                  cell_comp_coef=rep(NA,3*length(insulin_vars)),
                                  cell_comp_pval=rep(NA,3*length(insulin_vars)),
                                  cell_comp_adjpval=rep(NA,3*length(insulin_vars)))


for (m in 1:length(insulin_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(insulin_vars[m],"~", INSGCG.content[1], "+", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[2:3,c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    INSsec.inscont.cell[3*(m-1)+k,1] <- insulin_vars[m]
    INSsec.inscont.cell[3*(m-1)+k,3] <- cell.comp[k]
    INSsec.inscont.cell[3*(m-1)+k,c(4,5,7,8)] <- c(temp1[1,1],temp1[1,2],
                                                   temp1[2,1],temp1[2,2])
    
    
  }
}
INSsec.inscont.cell$hormone_cont_adjpval <- formatC(p.adjust(INSsec.inscont.cell$hormone_cont_pval,
                                                             method='fdr'),
                                                    format="e",digits=3)
INSsec.inscont.cell$cell_comp_adjpval <- formatC(p.adjust(INSsec.inscont.cell$cell_comp_pval,
                                                          method='fdr'),
                                                 format="e",digits=3)


## insulin secretion ~ glucagon content + cell composition + 8 covariates
INSsec.gcgcont.cell <- data.frame(secretion=rep(insulin_vars,each=3),
                                  hormone_content=rep(INSGCG.content[2],3*length(insulin_vars)),
                                  cell_comp=rep(cell.comp,length(insulin_vars)),
                                  hormone_cont_coef=rep(NA,3*length(insulin_vars)),
                                  hormone_cont_pval=rep(NA,3*length(insulin_vars)),
                                  hormone_cont_adjpval=rep(NA,3*length(insulin_vars)),
                                  cell_comp_coef=rep(NA,3*length(insulin_vars)),
                                  cell_comp_pval=rep(NA,3*length(insulin_vars)),
                                  cell_comp_adjpval=rep(NA,3*length(insulin_vars)))
for (m in 1:length(insulin_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(insulin_vars[m],"~", INSGCG.content[2], "+", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[2:3,c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    INSsec.gcgcont.cell[3*(m-1)+k,1] <- insulin_vars[m]
    INSsec.gcgcont.cell[3*(m-1)+k,3] <- cell.comp[k]
    INSsec.gcgcont.cell[3*(m-1)+k,c(4,5,7,8)] <- c(temp1[1,1],temp1[1,2],
                                                   temp1[2,1],temp1[2,2])
  }
}
INSsec.gcgcont.cell$hormone_cont_adjpval <- formatC(p.adjust(INSsec.gcgcont.cell$hormone_cont_pval,
                                                             method='fdr'),
                                                    format="e",digits=3)
INSsec.gcgcont.cell$cell_comp_adjpval <- formatC(p.adjust(INSsec.gcgcont.cell$cell_comp_pval,
                                                          method='fdr'),
                                                 format="e",digits=3)



##### Glucagon secretion vs hormone content + cells #####
## glucagon secretion ~ insulin content + cell composition + 8 covariates
GCGsec.inscont.cell <- data.frame(secretion=rep(glucagon_vars,each=3),
                                  hormone_content=rep(INSGCG.content[1],3*length(glucagon_vars)),
                                  cell_comp=rep(cell.comp,length(glucagon_vars)),
                                  hormone_cont_coef=rep(NA,3*length(glucagon_vars)),
                                  hormone_cont_pval=rep(NA,3*length(glucagon_vars)),
                                  hormone_cont_adjpval=rep(NA,3*length(glucagon_vars)),
                                  cell_comp_coef=rep(NA,3*length(glucagon_vars)),
                                  cell_comp_pval=rep(NA,3*length(glucagon_vars)),
                                  cell_comp_adjpval=rep(NA,3*length(glucagon_vars)))
for (m in 1:length(glucagon_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(glucagon_vars[m],"~", INSGCG.content[1], "+", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[2:3,c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    GCGsec.inscont.cell[3*(m-1)+k,1] <- glucagon_vars[m]
    GCGsec.inscont.cell[3*(m-1)+k,3] <- cell.comp[k]
    GCGsec.inscont.cell[3*(m-1)+k,c(4,5,7,8)] <- c(temp1[1,1],temp1[1,2],
                                                   temp1[2,1],temp1[2,2])
  }
}
GCGsec.inscont.cell$hormone_cont_adjpval <- formatC(p.adjust(GCGsec.inscont.cell$hormone_cont_pval,
                                                             method='fdr'),
                                                    format="e",digits=3)
GCGsec.inscont.cell$cell_comp_adjpval <- formatC(p.adjust(GCGsec.inscont.cell$cell_comp_pval,
                                                          method='fdr'),
                                                 format="e",digits=3)



## glucagon secretion ~ glucagon content + cell composition + 8 covariates
GCGsec.gcgcont.cell <- data.frame(secretion=rep(glucagon_vars,each=3),
                                  hormone_content=rep(INSGCG.content[2],3*length(glucagon_vars)),
                                  cell_comp=rep(cell.comp,length(glucagon_vars)),
                                  hormone_cont_coef=rep(NA,3*length(glucagon_vars)),
                                  hormone_cont_pval=rep(NA,3*length(glucagon_vars)),
                                  hormone_cont_adjpval=rep(NA,3*length(glucagon_vars)),
                                  cell_comp_coef=rep(NA,3*length(glucagon_vars)),
                                  cell_comp_pval=rep(NA,3*length(glucagon_vars)),
                                  cell_comp_adjpval=rep(NA,3*length(glucagon_vars)))
for (m in 1:length(glucagon_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(glucagon_vars[m],"~", INSGCG.content[2], "+", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[2:3,c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    GCGsec.gcgcont.cell[3*(m-1)+k,1] <- glucagon_vars[m]
    GCGsec.gcgcont.cell[3*(m-1)+k,3] <- cell.comp[k]
    GCGsec.gcgcont.cell[3*(m-1)+k,c(4,5,7,8)] <- c(temp1[1,1],temp1[1,2],
                                                   temp1[2,1],temp1[2,2])
  }
}
GCGsec.gcgcont.cell$hormone_cont_adjpval <- formatC(p.adjust(GCGsec.gcgcont.cell$hormone_cont_pval,
                                                             method='fdr'),
                                                    format="e",digits=3)
GCGsec.gcgcont.cell$cell_comp_adjpval <- formatC(p.adjust(GCGsec.gcgcont.cell$cell_comp_pval,
                                                          method='fdr'),
                                                 format="e",digits=3)


## Results & Summary Table
### Insulin secretion 
#### Insulin secretion ~ insulin hormone content + cell composition + 8 covariates
colnames(INSsec.inscont.cell) <- c("Insulin secretion","Hormone content","Cell composition",
                                   "Hormone content coefficient","Hormone content p-value",
                                   "Hormone content adjuste p-value",
                                   "Cell composition coefficient","Cell composition p-value",
                                   "Cell composition adjuste p-value")
INSsec.inscont.cell


#### Insulin secretion ~ glucagon hormone content + cell composition + 8 covariates
colnames(INSsec.gcgcont.cell) <- c("Insulin secretion","Hormone content","Cell composition",
                                   "Hormone content coefficient","Hormone content p-value",
                                   "Hormone content adjuste p-value",
                                   "Cell composition coefficient","Cell composition p-value",
                                   "Cell composition adjuste p-value")
INSsec.gcgcont.cell


### Glucagon secretion
#### Glucagon secretion ~ insulin hormone content + cell composition + 8 covariates
colnames(GCGsec.inscont.cell) <- c("Glucagon secretion","Hormone content","Cell composition",
                                   "Hormone content coefficient","Hormone content p-value",
                                   "Hormone content adjuste p-value",
                                   "Cell composition coefficient","Cell composition p-value",
                                   "Cell composition adjuste p-value")
GCGsec.inscont.cell

#### Glucagon secretion ~ glucagon hormone content + cell composition + 8 covariates
colnames(GCGsec.gcgcont.cell) <- c("Glucagon secretion","Hormone content","Cell composition",
                                   "Hormone content coefficient","Hormone content p-value",
                                   "Hormone content adjuste p-value",
                                   "Cell composition coefficient","Cell composition p-value",
                                   "Cell composition adjuste p-value")
GCGsec.gcgcont.cell




#####################################################################################################################################################################
#####################################################################################################################################################################
# Secretion vs hormone content + cell composition + hormone content * cell composition + 8 covariates

##### insulin secretion ~ hormone content + cell + interaction #####
## insulin secretion ~ insulin content + cell + interaction + interaction + 8 covariates
INSsec.inscontcell.int <- data.frame(secretion=rep(insulin_vars,each=3),
                                     hormone_content=rep(INSGCG.content[1],3*length(insulin_vars)),
                                     cell_comp=rep(cell.comp,length(insulin_vars)),
                                     hormone_cont_coef=rep(NA,3*length(insulin_vars)),
                                     hormone_cont_pval=rep(NA,3*length(insulin_vars)),
                                     hormone_cont_adjpval=rep(NA,3*length(insulin_vars)),
                                     cell_comp_coef=rep(NA,3*length(insulin_vars)),
                                     cell_comp_pval=rep(NA,3*length(insulin_vars)),
                                     cell_comp_adjpval=rep(NA,3*length(insulin_vars)),
                                     interaction_coef=rep(NA,3*length(insulin_vars)),
                                     interaction_pval=rep(NA,3*length(insulin_vars)),
                                     interaction_adjpval=rep(NA,3*length(insulin_vars)))

for (m in 1:length(insulin_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(insulin_vars[m],"~", INSGCG.content[1], "*", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[c(2,3,nrow(temp$coefficients)),c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    INSsec.inscontcell.int$secretion[3*(m-1)+k] <- insulin_vars[m]
    INSsec.inscontcell.int$cell_comp[3*(m-1)+k] <- cell.comp[k]
    INSsec.inscontcell.int[3*(m-1)+k,c(4,5,7,8,10,11)] <- 
      c(temp1[1,1],temp1[1,2],temp1[2,1],temp1[2,2],temp1[3,1],temp1[3,2])
  }
}
INSsec.inscontcell.int$hormone_cont_adjpval <- 
  formatC(p.adjust(INSsec.inscontcell.int$hormone_cont_pval,method='fdr'),format="e",digits=3)
INSsec.inscontcell.int$cell_comp_adjpval <- 
  formatC(p.adjust(INSsec.inscontcell.int$cell_comp_pval,method='fdr'),format="e",digits=3)
INSsec.inscontcell.int$interaction_adjpval <- 
  formatC(p.adjust(INSsec.inscontcell.int$interaction_pval,method='fdr'),format="e",digits=3)


## insulin secretion ~ glucagon content + cell + interaction + interaction + 8 covariates
INSsec.gcgcontcell.int <- data.frame(secretion=rep(insulin_vars,each=3),
                                     hormone_content=rep(INSGCG.content[2],3*length(insulin_vars)),
                                     cell_comp=rep(cell.comp,length(insulin_vars)),
                                     hormone_cont_coef=rep(NA,3*length(insulin_vars)),
                                     hormone_cont_pval=rep(NA,3*length(insulin_vars)),
                                     hormone_cont_adjpval=rep(NA,3*length(insulin_vars)),
                                     cell_comp_coef=rep(NA,3*length(insulin_vars)),
                                     cell_comp_pval=rep(NA,3*length(insulin_vars)),
                                     cell_comp_adjpval=rep(NA,3*length(insulin_vars)),
                                     interaction_coef=rep(NA,3*length(insulin_vars)),
                                     interaction_pval=rep(NA,3*length(insulin_vars)),
                                     interaction_adjpval=rep(NA,3*length(insulin_vars)))

for (m in 1:length(insulin_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(insulin_vars[m],"~", INSGCG.content[2], "*", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[c(2,3,nrow(temp$coefficients)),c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    INSsec.gcgcontcell.int$secretion[3*(m-1)+k] <- insulin_vars[m]
    INSsec.gcgcontcell.int$cell_comp[3*(m-1)+k] <- cell.comp[k]
    INSsec.gcgcontcell.int[3*(m-1)+k,c(4,5,7,8,10,11)] <- 
      c(temp1[1,1],temp1[1,2],temp1[2,1],temp1[2,2],temp1[3,1],temp1[3,2])
  }
}
INSsec.gcgcontcell.int$hormone_cont_adjpval <- 
  formatC(p.adjust(INSsec.gcgcontcell.int$hormone_cont_pval,method='fdr'),format="e",digits=3)
INSsec.gcgcontcell.int$cell_comp_adjpval <- 
  formatC(p.adjust(INSsec.gcgcontcell.int$cell_comp_pval,method='fdr'),format="e",digits=3)
INSsec.gcgcontcell.int$interaction_adjpval <- 
  formatC(p.adjust(INSsec.gcgcontcell.int$interaction_pval,method='fdr'),format="e",digits=3)



##### glucagon secretion ~ hormone content + cell + interaction #####
## glucagon secretion ~ insulin content + cell + interaction + interaction + 8 covariates
GCGsec.inscontcell.int <- data.frame(secretion=rep(glucagon_vars,each=3),
                                     hormone_content=rep(INSGCG.content[1],3*length(glucagon_vars)),
                                     cell_comp=rep(cell.comp,length(glucagon_vars)),
                                     hormone_cont_coef=rep(NA,3*length(glucagon_vars)),
                                     hormone_cont_pval=rep(NA,3*length(glucagon_vars)),
                                     hormone_cont_adjpval=rep(NA,3*length(glucagon_vars)),
                                     cell_comp_coef=rep(NA,3*length(glucagon_vars)),
                                     cell_comp_pval=rep(NA,3*length(glucagon_vars)),
                                     cell_comp_adjpval=rep(NA,3*length(glucagon_vars)),
                                     interaction_coef=rep(NA,3*length(glucagon_vars)),
                                     interaction_pval=rep(NA,3*length(glucagon_vars)),
                                     interaction_adjpval=rep(NA,3*length(glucagon_vars)))

for (m in 1:length(glucagon_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(glucagon_vars[m],"~", INSGCG.content[1], "*", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[c(2,3,nrow(temp$coefficients)),c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    GCGsec.inscontcell.int$secretion[3*(m-1)+k] <- glucagon_vars[m]
    GCGsec.inscontcell.int$cell_comp[3*(m-1)+k] <- cell.comp[k]
    GCGsec.inscontcell.int[3*(m-1)+k,c(4,5,7,8,10,11)] <- 
      c(temp1[1,1],temp1[1,2],temp1[2,1],temp1[2,2],temp1[3,1],temp1[3,2])
  }
}
GCGsec.inscontcell.int$hormone_cont_adjpval <- 
  formatC(p.adjust(GCGsec.inscontcell.int$hormone_cont_pval,method='fdr'),format="e",digits=3)
GCGsec.inscontcell.int$cell_comp_adjpval <- 
  formatC(p.adjust(GCGsec.inscontcell.int$cell_comp_pval,method='fdr'),format="e",digits=3)
GCGsec.inscontcell.int$interaction_adjpval <- 
  formatC(p.adjust(GCGsec.inscontcell.int$interaction_pval,method='fdr'),format="e",digits=3)


## glucagon secretion ~ glucagon content + cell + interaction + interaction + 8 covariates
GCGsec.gcgcontcell.int <- data.frame(secretion=rep(glucagon_vars,each=3),
                                     hormone_content=rep(INSGCG.content[2],3*length(glucagon_vars)),
                                     cell_comp=rep(cell.comp,length(glucagon_vars)),
                                     hormone_cont_coef=rep(NA,3*length(glucagon_vars)),
                                     hormone_cont_pval=rep(NA,3*length(glucagon_vars)),
                                     hormone_cont_adjpval=rep(NA,3*length(glucagon_vars)),
                                     cell_comp_coef=rep(NA,3*length(glucagon_vars)),
                                     cell_comp_pval=rep(NA,3*length(glucagon_vars)),
                                     cell_comp_adjpval=rep(NA,3*length(glucagon_vars)),
                                     interaction_coef=rep(NA,3*length(glucagon_vars)),
                                     interaction_pval=rep(NA,3*length(glucagon_vars)),
                                     interaction_adjpval=rep(NA,3*length(glucagon_vars)))

for (m in 1:length(glucagon_vars)) {
  for (k in 1:3) {
    mod = as.formula(paste0(glucagon_vars[m],"~", INSGCG.content[2], "*", cell.comp[k],
                            "+ Donor_HbA1c + Gender + race2 + Age_years + center + BMI + 
               Pre_shipment_Culture_Time_hours + Islet_Transit_Time_hours"))
    fit <- lm(mod, data=hipp)
    temp <- summary(fit)
    temp1 <- temp$coefficients[c(2,3,nrow(temp$coefficients)),c(1,4)] # coefficient & p-value
    temp1 <- round(temp1, 4)
    temp1 <- formatC(temp1, format="e", digits=3)
    GCGsec.gcgcontcell.int$secretion[3*(m-1)+k] <- glucagon_vars[m]
    GCGsec.gcgcontcell.int$cell_comp[3*(m-1)+k] <- cell.comp[k]
    GCGsec.gcgcontcell.int[3*(m-1)+k,c(4,5,7,8,10,11)] <- 
      c(temp1[1,1],temp1[1,2],temp1[2,1],temp1[2,2],temp1[3,1],temp1[3,2])
  }
}
GCGsec.gcgcontcell.int$hormone_cont_adjpval <- 
  formatC(p.adjust(GCGsec.gcgcontcell.int$hormone_cont_pval,method='fdr'),format="e",digits=3)
GCGsec.gcgcontcell.int$cell_comp_adjpval <- 
  formatC(p.adjust(GCGsec.gcgcontcell.int$cell_comp_pval,method='fdr'),format="e",digits=3)
GCGsec.gcgcontcell.int$interaction_adjpval <- 
  formatC(p.adjust(GCGsec.gcgcontcell.int$interaction_pval,method='fdr'),format="e",digits=3)



## Results & Summary
### Insulin secretion
#### Insulin secretion ~ insulin hormone content + cell composition + insulin hormone content*cell composition 8 covariates
colnames(INSsec.inscontcell.int) <- c("Insulin secretion","Hormone content","Cell composition",
                                      "Hormone content coefficient","Hormone content p-value",
                                      "Hormone content adjuste p-value",
                                      "Cell composition coefficient","Cell composition p-value",
                                      "Cell composition adjuste p-value",
                                      "Interaction coefficient","Interaction p-value",
                                      "Interaction adjuste p-value")
INSsec.inscontcell.int