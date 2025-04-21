#### Figure 6C&D, Cell composition vs. Race, Pairwise comparisons ####
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

hipp$White = factor(ifelse(hipp$race2 == "White", 1, 0))
hipp$Black = factor(ifelse(hipp$race2 == "Black", 1, 0))
hipp$Hispanic = factor(ifelse(hipp$race2 == "Hispanic", 1, 0))
hipp$Asian = factor(ifelse(hipp$race2 == "Asian", 1, 0))

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

# [ ] PreShipmentCultureTime and IsletTransitTime
hipp$PreShipmentCultureTime<-hipp$Pre_shipment_Culture_Time_hours
hipp$IsletTransitTime<-hipp$Islet_Transit_Time_hours


################ merge 2 dataset ################
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat = gen_dat

hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_all = merge(gen_dat, hipp, by="DONOR_RRID")

dat_all$AFR = factor(ifelse(dat_all$SuperPopulationClass == "AFR", 1, 0))
dat_all$AMR = factor(ifelse(dat_all$SuperPopulationClass == "AMR", 1, 0))
dat_all$EAS = factor(ifelse(dat_all$SuperPopulationClass == "EAS", 1, 0))
dat_all$EUR = factor(ifelse(dat_all$SuperPopulationClass == "EUR", 1, 0))


################ scale Beta, Alpha, and Delta cell ################
hipp$Beta_cell = scale(hipp$Beta_Cells)
hipp$Alpha_cell = scale(hipp$Alpha_Cells)
hipp$Delta_cell = scale(hipp$Delta_Cells)

dat_all$Beta_cell = scale(dat_all$Beta_Cells)
dat_all$Alpha_cell = scale(dat_all$Alpha_Cells)
dat_all$Delta_cell = scale(dat_all$Delta_Cells)


################################################################################
####################### Cell composition: self-reported ########################
comput = function(race1, race2, dat1){
  temp1 = dat1$Beta_cell[dat1[[race1]] == 1]
  temp2 = dat1$Beta_cell[dat1[[race2]] == 1]
  
  temp1.1 = dat1$Alpha_cell[dat1[[race1]] == 1]
  temp2.1 = dat1$Alpha_cell[dat1[[race2]] == 1]
  
  test1 = wilcox.test(temp1, temp2)
  test2 = wilcox.test(temp1.1, temp2.1)
  
  pval = c(race1, race2, round(test1$p.value, 4), round(test2$p.value, 4))
  return(pval)
}

out1 = comput("White", "Black", hipp)
out2 = comput("White", "Asian", hipp)
out3 = comput("White", "Hispanic", hipp)
out4 = comput("Black", "Asian", hipp)
out5 = comput("Black", "Hispanic", hipp)
out6 = comput("Asian", "Hispanic",hipp)

outnew = data.frame(rbind(out1, out2, out3, out4, out5, out6))
colnames(outnew) = c("race1", "race2", "pval_Beta", "pval_Alpha")

outnew$adj_pval_Beta = p.adjust(as.numeric(outnew$pval_Beta), method="fdr")
outnew$adj_pval_Alpha = p.adjust(as.numeric(outnew$pval_Alpha), method="fdr")
rownames(outnew) = NULL
outnew

####################### Cell composition: genetic ancestry ########################
out1 = comput("EUR", "AFR", dat_all)
out2 = comput("EUR", "EAS", dat_all)
out3 = comput("EUR", "AMR", dat_all)
out4 = comput("AFR", "EAS", dat_all)
out5 = comput("AFR", "AMR", dat_all)
out6 = comput("EAS", "AMR", dat_all)

outnew = data.frame(rbind(out1, out2, out3, out4, out5, out6))
colnames(outnew) = c("race1", "race2", "pval_Beta", "pval_Alpha")

outnew$adj_pval_Beta = p.adjust(as.numeric(outnew$pval_Beta), method="fdr")
outnew$adj_pval_Alpha = p.adjust(as.numeric(outnew$pval_Alpha), method="fdr")
rownames(outnew) = NULL
outnew