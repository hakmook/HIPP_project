#### Figure 3 ####
#### Author: Ke Xu, Hakmook Kang ####

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp


###################################################################
# 3 main explanatory variables
vars_int <- c("PreShipmentCultureTime","IsletTransitTime","IsletPurity") 

INS_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
              "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
              "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
              "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

GCG_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
              "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
              "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
              "GCG_KCl_20_SI")

co_vars <- c("Donor_HbA1c_s", "Gender", "race2", "Age_years_s", "center", "BMI_s")
###################################################################
###################################################################
vars_int = vars_int
outcome_int_ins = INS_vars
outcome_int_glu = GCG_vars

hipp <- hipp[, c(vars_int, INS_vars, GCG_vars, co_vars)]

############################################
### Insulin Secretion
########################################
Summary_Table_i = vector(mode='list', length=length(vars_int))
model_out = vector(mode='list', length=length(vars_int))
names(model_out) = vars_int
for (m in 1:length(vars_int)){
  temp_summary = NULL
  for (k in 1:length(outcome_int_ins)){
    model = as.formula(paste0(outcome_int_ins[k], "~", vars_int[m], 
            " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
    fit = lm(model, data = hipp)
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[2:3] = round(temp1[2:3], 4)
    temp1[c(1,4)] = formatC(temp1[c(1,4)], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
    
    model_out[[m]][[k]] = summary(fit)
  }
  names(model_out[[m]]) = vars_int
  
  temp_summary = as.data.frame(temp_summary)
  temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                    format = "e", digits = 3)
  Summary_Table_i[[vars_int[m]]] = temp_summary
}


############################################
### Glucagon Secretion
########################################

Summary_Table_g = vector(mode='list', length=length(vars_int))
model_out_g = vector(mode='list', length=length(vars_int))
names(model_out_g) = vars_int
for (m in 1:length(vars_int)){
  temp_summary = NULL
  for (k in 1:length(outcome_int_glu)){
    model = as.formula(paste0(outcome_int_glu[k], "~", vars_int[m], 
    " + Donor_HbA1c_s + Gender + race2 + Age_years_s + center + BMI_s"))
    fit = lm(model, data = hipp)
    temp = summary(fit)
    temp1 = temp$coefficients[2,]
    temp1[2:3] = round(temp1[2:3], 4)
    temp1[c(1,4)] = formatC(temp1[c(1,4)], format = "e", digits = 3)
    temp_summary = rbind(temp_summary, temp1) 
    
    model_out_g[[m]][[k]] = summary(fit)
  }
  names(model_out_g[[m]]) = vars_int
  
  temp_summary = as.data.frame(temp_summary)
  temp_summary$adj_pvalue = formatC(p.adjust(temp_summary[,4], method='fdr'),
                                    format = "e", digits = 3)
  Summary_Table_g[[vars_int[m]]] = temp_summary
}



############################################
### Insulin secretion and gender
########################################
Summary_Table_Gender = vector(mode='list', length=2)

insulin_Gender = NULL
for (m in 1:length(outcome_int_ins)){
  model = as.formula(paste0(outcome_int_ins[m], "~", 
  "Gender", " + Donor_HbA1c_s + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  insulin_Gender = rbind(insulin_Gender, temp1) 
}

insulin_Gender = as.data.frame(insulin_Gender)
insulin_Gender$adj_pvalue = formatC(p.adjust(insulin_Gender[,4], method='fdr'),
                                    format = "e", digits = 3)
Summary_Table_Insulin_Gender = insulin_Gender
rownames(Summary_Table_Insulin_Gender) = outcome_int_ins


############################################
### Glucagon secretion and gender
########################################
Summary_Table_Gender = vector(mode='list', length=2)

glu_Gender = NULL
for (m in 1:length(outcome_int_glu)){
  model = as.formula(paste0(outcome_int_glu[m], "~", 
                            "Gender", " + Donor_HbA1c_s + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model, data = hipp)
  temp = summary(fit)
  temp1 = temp$coefficients[2,]
  temp1[1:3] = round(temp1[1:3], 4)
  temp1[4] = formatC(temp1[4], format = "e", digits = 3)
  glu_Gender = rbind(glu_Gender, temp1) 
}

glu_Gender = as.data.frame(glu_Gender)
glu_Gender$adj_pvalue = formatC(p.adjust(glu_Gender[,4], method='fdr'),
                                    format = "e", digits = 3)
Summary_Table_Glu_Gender = glu_Gender
rownames(Summary_Table_Glu_Gender) = outcome_int_glu



############ Summary Table ############
######### Preshipment culture time #########
###### Insulin secretion trait and preshipment culture time ######
Summary_Table <- Summary_Table_i[["PreShipmentCultureTime"]]
rownames(Summary_Table) = outcome_int_ins
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table

###### Glucagon secretion trait and preshipment culture time ######
Summary_Table <- Summary_Table_g[["PreShipmentCultureTime"]]
rownames(Summary_Table) = outcome_int_glu
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table

######### IsletTransitTime #########
###### Insulin secretion trait and islet transit time ######
Summary_Table <- Summary_Table_i[["IsletTransitTime"]]
rownames(Summary_Table) = outcome_int_ins
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table

###### Glucagon secretion trait and islet transit time ######
Summary_Table <- Summary_Table_g[["IsletTransitTime"]]
rownames(Summary_Table) = outcome_int_glu
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table


######### IsletPurity #########
###### Insulin secretion trait and islet purity ######
Summary_Table <- Summary_Table_i[["IsletPurity"]]
rownames(Summary_Table) = outcome_int_ins
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table

###### Glucagon secretion trait and islet purity ######
Summary_Table <- Summary_Table_g[["IsletPurity"]]
rownames(Summary_Table) = outcome_int_glu
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table



######### Gender #########
###### Insulin secretion and gender ######
Summary_Table <- Summary_Table_Insulin_Gender
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table



###### Glucagon secretion and gender ######
Summary_Table <- Summary_Table_Glu_Gender
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coeff", "P-val", "Adj P-val")
Summary_Table