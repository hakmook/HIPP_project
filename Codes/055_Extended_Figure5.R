#### Extended Data Figure 5####
#### Author: Ke Xu, Hakmook Kang ####

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


###################################################################
###################################################################
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

cell_vars = c("ProbitBeta", "ProbitAlpha", "ProbitDelta")
content_vars = c("Islet_Insulin_Content_ng_IEQ_s", "Islet_Glucagon_Content_pg_IEQ_s")

#################################################
############ insulin secretion 
Summary_Table = vector(mode='list', length=4)
model_out = vector(mode='list', length=length(insulin_vars))

temp_summary = NULL
for (m in 1:length(insulin_vars)){
  model = as.formula(paste0(insulin_vars[m], " ~ SuperPopulationClass + Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  
  model0 = as.formula(paste0(insulin_vars[m], " ~ Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = dat_all)
  fit0 = lm(model0, data = dat_all)
  model_out[[m]] = fit 
  temp = anova(fit, fit0)
  temp1 = temp$`Pr(>F)`[2]
  temp1 = formatC(temp1, format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,1], method='fdr'),
                                  format = "e", digits = 3)
names(temp_summary)[1] = "p-value"
Summary_Table[[1]] = temp_summary

rownames(Summary_Table[[1]]) = insulin_vars

names(model_out) <- insulin_vars


# glucagon variables
model_out_g = vector(mode='list', length=length(glucagon_vars))

temp_summary = NULL
for (m in 1:length(glucagon_vars)){
  model = as.formula(paste0(glucagon_vars[m], "~ SuperPopulationClass + Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  model0 = as.formula(paste0(glucagon_vars[m], " ~ Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  
  fit = lm(model, data = dat_all)
  fit0 = lm(model0, data = dat_all)
  model_out_g[[m]] = fit 
  
  temp = anova(fit, fit0)
  temp1 = temp$`Pr(>F)`[2]
  temp1 = formatC(temp1, format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,1], method='fdr'),
                                  format = "e", digits = 3)
names(temp_summary)[1] = "p-value"

Summary_Table[[2]] = temp_summary

rownames(Summary_Table[[2]]) = glucagon_vars
names(model_out_g) = glucagon_vars

#################################################
############ Cell composition

model_out_cell = vector(mode='list', length=length(cell_vars))
temp_summary = NULL
for (m in 1:length(cell_vars)){
  model = as.formula(paste0(cell_vars[m], " ~ SuperPopulationClass + Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  
  model0 = as.formula(paste0(cell_vars[m], " ~ Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = dat_all)
  fit0 = lm(model0, data = dat_all)
  model_out_cell[[m]] = fit 
  temp = anova(fit, fit0)
  temp1 = temp$`Pr(>F)`[2]
  temp1 = formatC(temp1, format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,1], method='fdr'),
                                  format = "e", digits = 3)
names(temp_summary)[1] = "p-value"
Summary_Table[[3]] = temp_summary

rownames(Summary_Table[[3]]) = cell_vars

names(model_out_cell) <- cell_vars


#################################################
############ Hormone contents

model_out_con = vector(mode='list', length=length(content_vars))
temp_summary = NULL
for (m in 1:length(content_vars)){
  model = as.formula(paste0(content_vars[m], " ~ SuperPopulationClass + Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  
  model0 = as.formula(paste0(content_vars[m], " ~ Donor_HbA1c_s + Gender +  Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = dat_all)
  fit0 = lm(model0, data = dat_all)
  model_out_con[[m]] = fit 
  temp = anova(fit, fit0)
  temp1 = temp$`Pr(>F)`[2]
  temp1 = formatC(temp1, format = "e", digits = 3)
  temp_summary = rbind(temp_summary, temp1) 
}

temp_summary = as.data.frame(temp_summary)
temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,1], method='fdr'),
                                  format = "e", digits = 3)
names(temp_summary)[1] = "p-value"
Summary_Table[[4]] = temp_summary

rownames(Summary_Table[[4]]) = content_vars

names(model_out_con) <- content_vars



# Summary Table
## Insulin secretion traits 
Summary_Tab = Summary_Table[[1]]
colnames(Summary_Tab) = c("P-val", "Adj P-val")
Summary_Tab


## Glucagon secretion traits 
Summary_Tab = Summary_Table[[2]]
colnames(Summary_Tab) = c("P-val", "Adj P-val")
Summary_Tab


## Cell compositions
Summary_Tab = Summary_Table[[3]]
colnames(Summary_Tab) = c("P-val", "Adj P-val")
Summary_Tab


## Hormone contents
Summary_Tab = Summary_Table[[4]]
colnames(Summary_Tab) = c( "P-val", "Adj P-val")
Summary_Tab




############################################################
# INS_basal_ng_IEQ: genetic ancestry

# Define the function for p-values calculations
comput = function(race1, race2, dat1){
  temp1 = dat1$INS_basal_ng_IEQ[dat1[[race1]] == 1]
  temp2 = dat1$INS_basal_ng_IEQ[dat1[[race2]] == 1]
  
  test1 = wilcox.test(temp1, temp2)
  
  pval = c(race1, race2, round(test1$p.value, 4))
  return(pval)
}

out1 = comput("EUR", "AFR", dat_all)
out2 = comput("EUR", "EAS", dat_all)
out3 = comput("EUR", "AMR", dat_all)
out4 = comput("AFR", "EAS", dat_all)
out5 = comput("AFR", "AMR", dat_all)
out6 = comput("EAS", "AMR", dat_all)

outnew = data.frame(rbind(out1, out2, out3, out4, out5, out6))
colnames(outnew) = c("race1", "race2", "P_Value")

outnew$Adjust_P_Value = p.adjust(as.numeric(outnew$P_Value), method="fdr")
rownames(outnew) = NULL

# Convert P_Value and Adjust_P_Value columns to numeric
data <- outnew %>%
  mutate(
    P_Value = as.numeric(P_Value),
    Adjust_P_Value = as.numeric(Adjust_P_Value)
  )

# Format p-values and adjusted p-values as scientific notation
data$pvalueCharacter <- as.character(formatC(data$P_Value, digits = 4, format = 'f'))
data$adjpvalueCharacter <- as.character(formatC(data$Adjust_P_Value, digits = 4, format = 'f'))

# Apply bold formatting for p-values less than 0.05
data[data$P_Value < 0.05, 'pvalueCharacter'] <- cell_spec(data[data$P_Value < 0.05, 'pvalueCharacter'], "html", bold = TRUE)
data[data$Adjust_P_Value < 0.05, 'adjpvalueCharacter'] <- cell_spec(data[data$Adjust_P_Value < 0.05, 'adjpvalueCharacter'], "html", bold = TRUE)

# Drop the original P_Value and Adjust_P_Value columns
data <- data[, -c(3, 4)]

# Rename the columns
colnames(data) <- c("Race1", "Race2", "P-val", "Adj P-val")

# Return the styled table
kbl(data, escape = FALSE, digits = 4) %>%
  kable_styling()



