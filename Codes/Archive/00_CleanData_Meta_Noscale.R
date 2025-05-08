#### Data Cleaning ####
#### Author: Ke Xu, Hakmook Kang ####

#### Packages Loading ####
library(readxl)


################ Import Data ################
hipp<-read_excel("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/20240411_HIPP_data_freeze_202211071_new_morphology_data_updated.xlsx", sheet="Master_table")

################ Clean data ################

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

# [ ] Beta Alpha Delta Cell
hipp$BetaCellPct<-probitlink(hipp$Beta_Cells/100)
hipp$AlphaCellPct<-probitlink(hipp$Alpha_Cells/100)
hipp$DeltaCellPct<-probitlink(hipp$Delta_Cells/100)

### Use scale(logit(as.numeric(hipp$Beta.Cells….)/100)) when % beta cell composition is the outcome variable.
hipp$Beta_cell_pct <- scale(logit(as.numeric(hipp$Beta_Cells)/100))
hipp$Alpha_cell_pct <- scale(logit(as.numeric(hipp$Alpha_Cells)/100))
hipp$Delta_cell_pct <- scale(logit(as.numeric(hipp$Delta_Cells)/100))

# [ ] PreShipmentCultureTime and IsletTransitTime
hipp$PreShipmentCultureTime<-hipp$Pre_shipment_Culture_Time_hours
hipp$IsletTransitTime<-hipp$Islet_Transit_Time_hours

# Save data 
save(list=c("hipp"),file="/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta_noscale.RData")


# delete samples with traits have missing values
INS_vars <- c("INS_basal_ng_IEQ", "INS_1st_AUC_ng_IEQ", "INS_2nd_AUC_ng_IEQ", 
              "INS_G_16_7_AUC_ng_IEQ", "INS_G_16_7_SI", "INS_G_16_7_IBMX_100_AUC_ng_IEQ", 
              "INS_G_16_7_IBMX_100_SI", "INS_G_1_7_Epi_1_AUC_ng_IEQ", "INS_G_1_7_Epi_1_II_updated", 
              "INS_KCl_20_AUC_ng_IEQ", "INS_KCl_20_SI")

GCG_vars <- c("GCG_basal_pg_IEQ", "GCG_G_16_7_AUC_pg_IEQ", "GCG_G_16_7_II", 
              "GCG_G_16_7_IBMX_100_AUC_pg_IEQ", "GCG_G_16_7_IBMX_100_SI", 
              "GCG_G_1_7_Epi_1_AUC_pg_IEQ", "GCG_G_1_7_Epi_1_SI", "GCG_KCl_20_AUC_pg_IEQ", 
              "GCG_KCl_20_SI")

vars_to_check <- c(INS_vars, GCG_vars)

# Remove rows with any missing values in the specified columns
hipp <- hipp[complete.cases(hipp[, vars_to_check]), ]

# Save data 
save(list=c("hipp"),file="/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta_noscale_298.RData")
