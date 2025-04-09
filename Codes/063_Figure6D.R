#### Figure 6D, Secretion traits, hormone contents, and cell compositions with predicted ancestry  ####
#### Author: Ke Xu, Hakmook Kang ####

# Load package 
library(dplyr)
library(tidyverse)
library(Hmisc)
library(rms)
library(kableExtra)
library(knitr)
library(dplyr)
library(xlsx)
library(VGAM)
library(sjPlot)
library(readxl)


# Load data
hipp<-read_excel("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/20240411_HIPP_data_freeze_202211071_new_morphology_data_updated.xlsx", sheet="Master_table")



################ Clean data ################

################ Replace spaces, parentheses, dashes, and other special characters with underscores ################
colnames(hipp) <- gsub("[^[:alnum:]]+", "_", colnames(hipp))
colnames(hipp) <- gsub("_+", "_", colnames(hipp))
colnames(hipp) <- gsub("^_|_$", "", colnames(hipp))
colnames(hipp) <- gsub("content", "percent", colnames(hipp))

################ Filter cohort ################
hipp = hipp[which(hipp$n299cohort == "Y"),]

################ clean varaibles ################
# [ ] race2
# redefine race to combine American Indian with Native Hawaiian/Pacific Islander
hipp$race2<-NA
hipp$race2[hipp$Race=="White"]<-1
hipp$race2[hipp$Race=="Black or African American"]<-2
hipp$race2[hipp$Race=="Hispanic/Latino"]<-3
hipp$race2[hipp$Race=="Asian"]<-4
# hipp$race2[hipp$Race=="American Indian or Alaska Native" | 
#              hipp$Race=="Native Hawaiian or Other Pacific Islander"]<-5
# hipp$race2[hipp$Race=="Not Available"]<-6
hipp$race2<-factor(hipp$race2, levels = 1:4, 
                   labels = c("White","Black","Hispanic","Asian"))
label(hipp$race2) = "Race"

hipp<-subset(hipp, subset = !is.na(race2))  # dim(hipp)[1]  321
hipp$Race<-factor(hipp$Race)
hipp$race2<-factor(hipp$race2)

# [ ] center
hipp$center<-NA
hipp$center[hipp$Center=="Scharp-Lacy"]<-1
hipp$center[hipp$Center=="SC-ICRC"]<-2
hipp$center[hipp$Center=="Miami"]<-3
hipp$center[hipp$Center=="Wisconsin"]<-4
hipp$center[hipp$Center=="Pennsylvania"]<-5
hipp$center<-factor(hipp$center, levels = 1:5, labels = c("Scharp-Lacy","SC-ICRC","Miami","Wisconsin","Pennsylvania"))
hipp$center<-factor(hipp$center)
hipp$Center<-factor(hipp$Center)

hipp$BetaCellPct<-probitlink(hipp$Beta_Cells/100)
hipp$AlphaCellPct<-probitlink(hipp$Alpha_Cells/100)
hipp$DeltaCellPct<-probitlink(hipp$Delta_Cells/100)

# [ ] PreShipmentCultureTime and IsletTransitTime
hipp$PreShipmentCultureTime<-hipp$Pre_shipment_Culture_Time_hours
hipp$IsletTransitTime<-hipp$Islet_Transit_Time_hours


################ merge 2 dataset ################
# new data
gen_dat = read.csv('/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/IIDP_for_Yasminye_71924_HK.csv')

# old data
gen_dat_old = read.csv('/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/IIDP_102523_FOR_YASMINYE.csv') 

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")

#####################################################################
INS_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
              "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
              "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
              "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

GCG_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
              "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
              "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
              "GCG_KCl_20_SI")

cell_vars <- c("BetaCellPct", "AlphaCellPct", "DeltaCellPct")
content_vars <- c("Islet_Insulin_Content_ng_IEQ", "Islet_Glucagon_Content_pg_IEQ")

dat_all <- dat_all %>%
  mutate(across(all_of(c(INS_vars, GCG_vars)), as.numeric))
#####################################################################

############################# Insulin secretion traits #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(INS_vars))
temp_summary <- NULL

for (var in INS_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- INS_vars
Summary_Table

############################# Glucagon secretion traits #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(GCG_vars))
temp_summary <- NULL

for (var in GCG_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- GCG_vars
Summary_Table

############################# Cell compositions #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(cell_vars))
temp_summary <- NULL

for (var in cell_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- cell_vars
Summary_Table


############################# Hormone contents #######################################
Summary_Table <- vector("list", length = 4)
model_out <- vector("list", length = length(content_vars))
temp_summary <- NULL

for (var in content_vars) {
  model <- as.formula(paste(var, "~ SuperPopulationClass + Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  model0 <- as.formula(paste(var, "~ Donor_HbA1c + Gender + Age_years + center + BMI + PreShipmentCultureTime + IsletTransitTime"))
  
  fit <- lm(model, data = dat_all)
  fit0 <- lm(model0, data = dat_all)
  
  model_out[[var]] <- fit 
  
  temp <- anova(fit, fit0)
  p_value <- formatC(temp$`Pr(>F)`[2], format = "e", digits = 3)
  temp_summary <- rbind(temp_summary, p_value) 
}

Summary_Table[[1]] <- as.data.frame(temp_summary)
Summary_Table[[1]]$adj_pvalue <- formatC(p.adjust(temp_summary[, 1], method = 'fdr'), format = "e", digits = 3)
names(Summary_Table[[1]])[1] <- "p-value"
rownames(Summary_Table[[1]]) <- content_vars
Summary_Table

