#### Extended Figure 7A, Partitioned scores - Secretion traits ####
#### Author: Ke Xu, Hakmook Kang ####

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

###################################################################
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
############ GRS T1D
# insulin secretion 

Part_GRS_vars = c("HLA.DR.DQ", "HLA.Class.1", "HLA.Class.2", "Non.HLA")

Summary_Table = vector(mode='list', length=2)
model_out = vector(mode='list', length=length(insulin_vars))


for (m1 in 1:length(Part_GRS_vars)){
  temp_summary = NULL
  for (m in 1:length(insulin_vars)){
    model = as.formula(paste0(insulin_vars[m], "~", Part_GRS_vars[m1]," +  Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime + PC1 + PC2 + PC3 + PC4 + PC5"))
    fit = lm(model, data = dat_all)
    model_out[[m]][[m1]] = fit 
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[1:3] = round(temp1[1:3], 4)
    temp1[4] = formatC(temp1[4], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
  }
  temp_summary = as.data.frame(temp_summary)
  temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                    format = "e", digits = 3)
  Summary_Table[[1]][[m1]] = temp_summary
  
  rownames(Summary_Table[[1]][[m1]]) = insulin_vars
}

names(Summary_Table[[1]]) = Part_GRS_vars


# glucagon variables
model_out_g = vector(mode='list', length=length(glucagon_vars))

for (m1 in 1:length(Part_GRS_vars)){
  temp_summary = NULL
  for (m in 1:length(glucagon_vars)){
    
    model = as.formula(paste0(glucagon_vars[m], "~", Part_GRS_vars[m1], " +  Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime + PC1 + PC2 + PC3 + PC4 + PC5"))
    fit = lm(model, data = dat_all)
    model_out_g[[m]][[m1]] = fit
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[1:3] = round(temp1[1:3], 4)
    temp1[4] = formatC(temp1[4], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
  }
  
  temp_summary = as.data.frame(temp_summary)
  temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                    format = "e", digits = 3)
  Summary_Table[[2]][[m1]] = temp_summary
  
  rownames(Summary_Table[[2]][[m1]]) = glucagon_vars
  
  names(Summary_Table) = c("Insulin", "Glucagon")
}

names(Summary_Table[[2]]) = Part_GRS_vars



######################################################
############ GRS T2D
# insulin secretion 

Part_GRS_vars2 = c("BETA_CELL", "PROINSULIN", "OBESITY" , "LIPODYSTROPHY", "LIVER_LIPID" )

Summary_Table2 = vector(mode='list', length=2)
model_out2 = vector(mode='list', length=length(insulin_vars))


for (m1 in 1:length(Part_GRS_vars2)){
  temp_summary = NULL
  for (m in 1:length(insulin_vars)){
    model = as.formula(paste0(insulin_vars[m], "~", Part_GRS_vars2[m1]," +  Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime + PC1 + PC2 + PC3 + PC4 + PC5"))
    fit = lm(model, data = dat_all)
    model_out2[[m]][[m1]] = fit 
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[1:3] = round(temp1[1:3], 4)
    temp1[4] = formatC(temp1[4], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
  }
  temp_summary = as.data.frame(temp_summary)
  temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                    format = "e", digits = 3)
  Summary_Table2[[1]][[m1]] = temp_summary
  
  rownames(Summary_Table2[[1]][[m1]]) = insulin_vars
}

names(Summary_Table2[[1]]) = Part_GRS_vars2


# glucagon variables
model_out_g2 = vector(mode='list', length=length(glucagon_vars))

for (m1 in 1:length(Part_GRS_vars2)){
  temp_summary = NULL
  for (m in 1:length(glucagon_vars)){
    
    model = as.formula(paste0(glucagon_vars[m], "~", Part_GRS_vars2[m1], " +  Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime + PC1 + PC2 + PC3 + PC4 + PC5"))
    fit = lm(model, data = dat_all)
    model_out_g2[[m]][[m1]] = fit
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[1:3] = round(temp1[1:3], 4)
    temp1[4] = formatC(temp1[4], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
  }
  
  temp_summary = as.data.frame(temp_summary)
  temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                    format = "e", digits = 3)
  Summary_Table2[[2]][[m1]] = temp_summary
  
  rownames(Summary_Table2[[2]][[m1]]) = glucagon_vars
  
  names(Summary_Table2) = c("Insulin", "Glucagon")
}

names(Summary_Table2[[2]]) = Part_GRS_vars2




# Summary Table

## Partitioned GRS T1
### HLA DR/DQ and Insulin secretion trait 
Summary_Tab = Summary_Table[[1]][[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### HLA Class 1 and Insulin secretion trait 
Summary_Tab = Summary_Table[[1]][[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### HLA Class 2 and Insulin secretion trait 
Summary_Tab = Summary_Table[[1]][[3]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Non HLA and Insulin secretion trait 
Summary_Tab = Summary_Table[[1]][[4]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### HLA DR/DQ and Glucagon secretion trait 
Summary_Tab = Summary_Table[[2]][[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### HLA Class 1 and Glucagon secretion trait 
Summary_Tab = Summary_Table[[2]][[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### HLA Class 2 and Glucagon secretion trait 
Summary_Tab = Summary_Table[[2]][[3]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Non HLA and Glucagon secretion trait 
Summary_Tab = Summary_Table[[2]][[4]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab





## Partitioned GRS T2
### Beta cell and Insulin secretion trait 
Summary_Tab = Summary_Table2[[1]][[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Proinsulin and Insulin secretion trait 
Summary_Tab = Summary_Table2[[1]][[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Obesity and Insulin secretion trait 
Summary_Tab = Summary_Table2[[1]][[3]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Lipodystrophy and Insulin secretion trait 
Summary_Tab = Summary_Table2[[1]][[4]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Liver lipid and Insulin secretion trait 
Summary_Tab = Summary_Table2[[1]][[5]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab



### Beta cell and Glucagon secretion trait 
Summary_Tab = Summary_Table2[[2]][[1]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Proinsulin and Glucagon secretion trait 
Summary_Tab = Summary_Table2[[2]][[2]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Obesity and Glucagon secretion trait 
Summary_Tab = Summary_Table2[[2]][[3]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Lipodystrophy and Glucagon secretion trait 
Summary_Tab = Summary_Table2[[2]][[4]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab


### Liver lipid and Glucagon secretion trait 
Summary_Tab = Summary_Table2[[2]][[5]][,-c(2,3)]
colnames(Summary_Tab) = c("Coeff", "P-val", "Adj P-val")
Summary_Tab



