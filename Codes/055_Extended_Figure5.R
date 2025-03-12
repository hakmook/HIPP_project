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



