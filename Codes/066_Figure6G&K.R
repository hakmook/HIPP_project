#### Figure 6G&K, Cell composition vs. age, pre-shipment culture time, transit time; Hormone content vs. race, pre-shipment culture time, transit time ####
#### Author: Ke Xu, Hakmook Kang ####

rm(list = ls())

# Load package 
library(dplyr)

### V1. Scaled ###
# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp


############ Hormone content ~ race+preshipment culture time+transit time + other covariates ########
INSGCG.content <- c("Islet_Insulin_Content_ng_IEQ_s",
                    "Islet_Glucagon_Content_pg_IEQ_s")


##### hormone content ~ BMI + other covariates #####

INSGCGcont.race <- data.frame(hormone_content=INSGCG.content,
                                 race_pval=rep(NA,2), race_adjpval=rep(NA,2))


for (k in 1:2) {
  # test
  # k = 1
  # Model for Hormone content ~ Race
  # Full model
  mod1 = as.formula(paste0(INSGCG.content[k], "~ race2  + center + Donor_HbA1c_s + Gender + Age_years_s + BMI_s"))
  fit1 <- lm(mod1, data=hipp)
  
  # Reduced model (without race2)
  mod0 = as.formula(paste0(INSGCG.content[k], "~ center + Donor_HbA1c_s + Gender + Age_years_s + BMI_s"))
  fit0 <- lm(mod0, data=hipp)
  
  f_test <- anova(fit0, fit1)
  race_p <- f_test$`Pr(>F)`[2]
  race_p <- round(race_p,4)
  race_p <- formatC(race_p, format="e", digits=3)
  
  INSGCGcont.race$race_pval[k] <- race_p
  
  
}

INSGCGcont.race$race_adjpval <- formatC(p.adjust(INSGCGcont.race$race_pval,method='fdr'),
                                              format="e",digits=3)



# Hormone content ~ Race
colnames(INSGCGcont.race) <- c("Hormone Content","P-value","Adjusted P-value")
INSGCGcont.race




