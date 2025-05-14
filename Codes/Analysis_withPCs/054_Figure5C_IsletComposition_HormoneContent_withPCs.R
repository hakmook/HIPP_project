#### Figure 5, Islet composition ~ hormone content  ####
#### Author: Ke Xu, Hakmook Kang ####

rm(list = ls())

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")

###################################################################
# 3 main explanatory variables
vars_int <- c("Beta_cell", "Alpha_cell", "Delta_cell")

out_vars <- c("Islet_Insulin_Content_ng_IEQ_s", "Islet_Glucagon_Content_pg_IEQ_s")

co_vars <- c("Donor_HbA1c_s", "Gender", "PC1", "PC2", "PC3", "PC4", "PC5", "Age_years_s", "center", "BMI_s", "PreShipmentCultureTime","IsletTransitTime")
###################################################################
###################################################################
vars_int = vars_int
out_vars = out_vars

hipp <- dat_all[, c(vars_int, out_vars, co_vars)]

############################################
### Insulin Secretion
########################################
summary_data = vector(mode='list', length=length(vars_int))
model_out = vector(mode='list', length=length(vars_int))
names(model_out) = vars_int
for (m in 1:length(vars_int)) {
  temp_summary = NULL
  model_out[[m]] = list()  # Initialize model_out[[m]] as an empty list
  
  for (k in 1:length(out_vars)) {
    model = as.formula(paste0(out_vars[k], "~", vars_int[m], 
                              " + Donor_HbA1c_s + Gender + PC1 + PC2 + PC3 + PC4 + PC5 + Age_years_s + center + BMI_s +PreShipmentCultureTime + IsletTransitTime"))
    fit = lm(model, data = hipp)
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[2:3] = round(temp1[2:3], 4)
    temp1[c(1,4)] = formatC(temp1[c(1,4)], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
    
    model_out[[m]][[k]] = summary(fit)
  }
  names(model_out[[m]]) = out_vars  # Assign names correctly
  
  temp_summary = as.data.frame(temp_summary)
  summary_data[[vars_int[m]]] = temp_summary
}

# Create two summary tables for temp1 and temp1.1
summary_temp1 <- do.call(rbind, lapply(summary_data, function(x) x[1,]))
summary_temp1.1 <- do.call(rbind, lapply(summary_data, function(x) x[2,]))

summary_temp1$adjust_pvalues <- p.adjust(summary_temp1$`Pr(>|t|)`, 'fdr')
summary_temp1.1$adjust_pvalues <- p.adjust(summary_temp1.1$`Pr(>|t|)`, 'fdr')


# Results for Insulin Content
summary_temp1


# Results for Glucagon Content
summary_temp1.1