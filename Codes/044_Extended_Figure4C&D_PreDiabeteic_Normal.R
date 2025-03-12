#### Extended Data Figure 4C&D, Pre-Diabeteic vs. Normal in Islet Insulin and Glucagon Secretory Traits (scaled) ####
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


###################
###################
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

vars_int = c(insulin_vars, glucagon_vars)

cell_comp <- c("Beta_cell_pct","Alpha_cell_pct","Delta_cell_pct")

## hormone content
INSGCG.content <- c("Islet_Insulin_Content_ng_IEQ_s",
                    "Islet_Glucagon_Content_pg_IEQ_s")
#################################################

#################################################
Summary_Table = NULL
model_out = vector(mode='list', length=32)

for (k in 1:length(vars_int)){
  model1 = as.formula(paste0(vars_int[k],"~ group +  Gender + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  fit = lm(model1, data = hipp)
  temp = summary(fit)
  temp1 = round(temp$coefficients[2,],3)
  Summary_Table = rbind(Summary_Table, temp1) 
  
  model_out[[k]] = summary(fit)
}

rownames(Summary_Table) = vars_int
names(model_out) = vars_int


#### cell composition

cellcomp.group <- data.frame(cell_comp=cell_comp,
                             group_coef=rep(NA,3), 
                             group_pval=rep(NA,3), group_adjpval=rep(NA,3))

for (k in 1:3) {
  # model with center
  mod1 = as.formula(paste0(cell_comp[k],"~  group + Gender + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  
  fit1 <- lm(mod1, data=hipp)
  temp <- summary(fit1)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  
  cellcomp.group$group_coef[k] <- temp1[2,1]
  cellcomp.group$group_pval[k] <- temp1[2,4]
}

cellcomp.group$group_adjpval <- formatC(p.adjust(cellcomp.group$group_pval,method='fdr'),
                                        format="e",digits=3)

### hormone content

INSGCGcont.group <- data.frame(hormone_content=INSGCG.content,
                               group_coef=rep(NA,2), group_pval=rep(NA,2),
                               group_adjpval=rep(NA,2))


for (k in 1:2) {
  # model with center
  mod1 = as.formula(paste0(INSGCG.content[k],"~ group +  Gender + race2 + Age_years_s + center + BMI_s + 
               PreShipmentCultureTime + IsletTransitTime"))
  
  fit1 <- lm(mod1, data=hipp)
  temp <- summary(fit1)
  temp1 <- round(temp$coefficients,4)
  temp1 <- formatC(temp1, format="e", digits=3)
  
  INSGCGcont.group$group_coef[k] <- temp1[2,1]
  INSGCGcont.group$group_pval[k] <- temp1[2,4]
  
}

INSGCGcont.group$group_adjpval <- formatC(p.adjust(INSGCGcont.group$group_pval,method='fdr'),
                                          format="e",digits=3)


# Summary Table
# Major explanatory variable is 'Pre-diabetes' vs. 'Normal'

## Secretion traits ~ pre-diabetes vs. normal
Summary_Table = Summary_Table[,-c(2,3)]
colnames(Summary_Table) = c("Coefficient", "P-value")
Summary_Table

## Cell composition ~ pre-diabetes vs. normal
colnames(cellcomp.group) <- c("Cell composition","Coefficient","P-value","Adjusted P-value")
cellcomp.group

## Hormone content ~ pre-diabetes vs. normal
colnames(INSGCGcont.group) <- c("Hormone content","Coefficient","P-value","Adjusted P-value")
INSGCGcont.group
