#### Data Cleaning ####
#### Author: Ke Xu, Hakmook Kang ####

## Clean Genetic data
gen_dat = read.csv('/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA0/IIDP_for_Yasminye_71924_HK.csv')
### genetic ancestry

gen_dat$AFR = factor(ifelse(gen_dat$SuperPopulationClass == "AFR", 1, 0))
gen_dat$AMR = factor(ifelse(gen_dat$SuperPopulationClass == "AMR", 1, 0))
gen_dat$EAS = factor(ifelse(gen_dat$SuperPopulationClass == "EAS", 1, 0))
gen_dat$EUR = factor(ifelse(gen_dat$SuperPopulationClass == "EUR", 1, 0))

# Save data 
save(list=c("gen_dat"),file="/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")

