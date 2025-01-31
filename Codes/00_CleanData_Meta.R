#### Data Cleaning ####
#### Author: Ke Xu, Hakmook Kang ####

#### Packages Loading ####
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


################ Import Data ################
hipp<-read_excel("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/20240411_HIPP_data_freeze_202211071_new_morphology_data_updated.xlsx", sheet="Master_table")

################ Replace spaces, parentheses, dashes, and other special characters with underscores ################
colnames(hipp) <- gsub("[^[:alnum:]]+", "_", colnames(hipp))
colnames(hipp) <- gsub("_+", "_", colnames(hipp))
colnames(hipp) <- gsub("^_|_$", "", colnames(hipp))
colnames(hipp) <- gsub("content", "percent", colnames(hipp))

################ Filter cohort ################
hipp = hipp[which(hipp$n299cohort == "Y"),]

################ define non-diabetes and pre-diabetes ################
# [ ] group
hipp$group<-NA
hipp$group[hipp$Donor_HbA1c<5.7]<-1
hipp$group[hipp$Donor_HbA1c>=5.7]<-2
hipp$group<-factor(hipp$group, levels = 1:2, labels = c("Normal","Pre-diabetes"))

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

################ Scale variables ################
# [ ] ColdIschemiaDuration
# [ ] PreShipmentCultureTime
# [ ] IsletTransitTime
# [ ] IsletArea
# [ ] IsletPerimeter
# [ ] Age_years
# [ ] BMI
# [ ] Donor_HbA1c
# [ ] Islet_Insulin_Content_ng_IEQ
# [ ] Islet_Glucagon_Content_pg_IEQ
hipp$ColdIschemiaDuration<-scale(as.numeric(hipp$Cold_Ischemia_Duration_hours))
hipp$PreShipmentCultureTime<-scale(hipp$Pre_shipment_Culture_Time_hours)
hipp$IsletTransitTime<-scale(hipp$Islet_Transit_Time_hours)
hipp$IsletArea<-scale(as.numeric(hipp$Islet_area))
hipp$IsletPerimeter<-scale(as.numeric(hipp$Islet_perimeter))
hipp$IsletPurity= scale(as.numeric(hipp$Islet_Purity))
hipp$Age_years_s = scale(hipp$Age_years)
hipp$BMI_s = scale(hipp$BMI)
hipp$Donor_HbA1c_s = scale(hipp$Donor_HbA1c)
hipp$Islet_Insulin_Content_ng_IEQ_s = scale(hipp$Islet_Insulin_Content_ng_IEQ)
hipp$Islet_Glucagon_Content_pg_IEQ_s = scale(hipp$Islet_Glucagon_Content_pg_IEQ)

############ Compute the probit transformation and scale Beta cells, alpha cells, and delta cells ################
# [ ] Beta_Cells
# [ ] Alpha_Cells
# [ ] Delta_Cells
hipp$ProbitBeta<-scale(probitlink(hipp$Beta_Cells/100))
hipp$ProbitAlpha<-scale(probitlink(hipp$Alpha_Cells/100))
hipp$ProbitDelta<-scale(probitlink(hipp$Delta_Cells/100))

hipp$Beta_cell = scale(hipp$Beta_Cells)
hipp$Alpha_cell = scale(hipp$Alpha_Cells)
hipp$Delta_cell = scale(hipp$Delta_Cells)

hipp$Beta_cell_pct <- scale(logit(as.numeric(hipp$Beta_Cells)/100))
hipp$Alpha_cell_pct <- scale(logit(as.numeric(hipp$Alpha_Cells)/100))
hipp$Delta_cell_pct <- scale(logit(as.numeric(hipp$Delta_Cells)/100))

### Question ### 
### Do we still need the percentage or not? We have calculated the probit transformation as above
# Calculate percentage
# [ ] Alpha.Cells.percnt
# [ ] Delta.Cells.percnt
# [ ] Beta.Cells.percnt
# hipp$Alpha.Cells.percnt<-hipp$Alpha.Cells.percnt/100
# hipp$Delta.Cells.percnt<-hipp$Delta.Cells.percnt/100
# hipp$Beta.Cells.percnt<-hipp$Beta.Cells.percnt/100
# hipp$DispersedIsletCellViability.percnt<-scale(hipp$Dispersed_Islet_Cell_Viability/100)

# label(hipp$ProbitBeta) = "Beta cell pct"
# label(hipp$ProbitAlpha) = "Alpha cell pct"
# label(hipp$ProbitDelta) = "Delta cell pct"


#### Scale insulin and glucagon trait variables #### 
# We have a total of 32 insulin and glucagon trait variables
INS_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
              "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
              "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
              "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

GCG_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
              "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
              "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
              "GCG_KCl_20_SI")

# Scale INS variables
hipp[INS_vars] <- lapply(hipp[INS_vars], function(x) scale(as.numeric(x)))

# Scale GCG variables
hipp[GCG_vars] <- lapply(hipp[GCG_vars], function(x) scale(as.numeric(x)))

#### scale morphological variables #### 
morpho_vars <- c("Islet_diameter_preperi", "Islet_area_preperi", "Islet_perimeter_preperi",
                 "Islet_diameter_postperi", "Islet_area_postperi", "Islet_perimeter_postperi",
                 "Islet_diameter", "Islet_area", "Islet_perimeter")

hipp[morpho_vars] <- lapply(hipp[morpho_vars], function(x) scale(as.numeric(x)))


#### Covert 12 'percent' variables to probit(percent/100) = qnorm(percent/100) #### 
percent_vars = c( "INS_basal_percent", 
                  "INS_1st_AUC_percent", "INS_2nd_AUC_percent", "INS_G_16_7_AUC_percent", 
                  "INS_G_16_7_IBMX_100_AUC_percent", "INS_G_1_7_Epi_1_AUC_percent", 
                  "INS_KCl_20_AUC_percent",
                  "GCG_G_16_7_AUC_percent", 
                  "GCG_G_16_7_IBMX_100_AUC_percent", "GCG_G_1_7_Epi_1_AUC_percent", 
                  "GCG_KCl_20_AUC_percent")
# Apply transformations: convert to numeric, probit transform, and scale
hipp[percent_vars] <- lapply(hipp[percent_vars], function(x) scale(probitlink(as.numeric(x))))



# Save data 
save(list=c("hipp"),file="/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
