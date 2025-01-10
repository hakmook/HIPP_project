#### Figure 1B & 1C ####
#### Author: Ke Xu, Hakmook Kang ####

# Load package 
library(dplyr)


# Load data
load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_meta.RData")
hipp <- hipp

dat.meta <- data[, c("Gender", "race2", "Age_years", 'BMI', "Donor_HbA1c")]

load("/Users/kexu/Library/CloudStorage/OneDrive-VUMC/Research/Active/20241022_HIPP/DATA1/data_process_gen_dat.RData")
gen_dat <- gen_dat


######## For N=299 ########
dat.tab <- dat.meta

# Calculate overall gender distribution percentage
gender_distribution <- dat.tab %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100)

# Calculate race distribution percentage by gender (ensuring sum by gender is 100%)
race_gender_distribution <- dat.tab %>%
  group_by(Gender, race2) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender) %>%  # Ensuring percentages are calculated within each gender group
  mutate(Percentage_by_gender = Count / sum(Count) * 100)

# View results
print("Gender Distribution:")
print(gender_distribution)

print("Race Distribution by Gender:")
print(race_gender_distribution)

# Calculate summary statistics for each race by gender
summary_stats <- dat.tab %>%
  group_by(Gender, race2) %>%
  summarise(
    Age_mean = mean(Age_years, na.rm = TRUE),
    Age_sd = sd(Age_years, na.rm = TRUE),
    Age_range = paste0(range(Age_years, na.rm = TRUE), collapse = " - "),
    
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    BMI_range = paste0(range(BMI, na.rm = TRUE), collapse = " - "),
    
    HbA1c_mean = mean(Donor_HbA1c, na.rm = TRUE),
    HbA1c_sd = sd(Donor_HbA1c, na.rm = TRUE),
    HbA1c_range = paste0(range(Donor_HbA1c, na.rm = TRUE), collapse = " - ")
  )

# View the results
print(summary_stats)



# Calculate summary statistics by gender
summary_stats <- dat.tab %>%
  group_by(Gender) %>%
  summarise(
    Age_mean = mean(Age_years, na.rm = TRUE),
    Age_sd = sd(Age_years, na.rm = TRUE),
    Age_range = paste0(range(Age_years, na.rm = TRUE), collapse = " - "),
    
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    BMI_range = paste0(range(BMI, na.rm = TRUE), collapse = " - "),
    
    HbA1c_mean = mean(Donor_HbA1c, na.rm = TRUE),
    HbA1c_sd = sd(Donor_HbA1c, na.rm = TRUE),
    HbA1c_range = paste0(range(Donor_HbA1c, na.rm = TRUE), collapse = " - ")
  )

# View the results
print(summary_stats)



######## For N=268 ######## 
hipp$DONOR_RRID = substring(hipp$RRID,6)
dat_sub = merge(gen_dat, hipp, by="DONOR_RRID")

# Calculate overall gender distribution percentage
gender_distribution <- dat_sub %>%
  count(Gender) %>%
  mutate(Percentage = n / sum(n) * 100)

# Calculate race distribution percentage by gender (ensuring sum by gender is 100%)
ancestry_gender_distribution <- dat_sub %>%
  group_by(Gender, SuperPopulationClass) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender) %>%  # Ensuring percentages are calculated within each gender group
  mutate(Percentage_by_gender = Count / sum(Count) * 100)

# View results
print("Gender Distribution:")
print(gender_distribution)

print("Ancestry Distribution by Gender:")
print(ancestry_gender_distribution)

# Calculate summary statistics for each race by gender
dat.tab <- dat_sub[, c("Gender", "SuperPopulationClass", "Age_years", 'BMI', "Donor_HbA1c")]
summary_stats <- dat.tab %>%
  group_by(Gender, SuperPopulationClass) %>%
  summarise(
    Age_mean = mean(Age_years, na.rm = TRUE),
    Age_sd = sd(Age_years, na.rm = TRUE),
    Age_range = paste0(range(Age_years, na.rm = TRUE), collapse = " - "),
    
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    BMI_range = paste0(range(BMI, na.rm = TRUE), collapse = " - "),
    
    HbA1c_mean = mean(Donor_HbA1c, na.rm = TRUE),
    HbA1c_sd = sd(Donor_HbA1c, na.rm = TRUE),
    HbA1c_range = paste0(range(Donor_HbA1c, na.rm = TRUE), collapse = " - ")
  )

# View the results
print(summary_stats)


# Calculate summary statistics by gender
summary_stats <- dat_sub %>%
  group_by(Gender) %>%
  summarise(
    Age_mean = mean(Age_years, na.rm = TRUE),
    Age_sd = sd(Age_years, na.rm = TRUE),
    Age_range = paste0(range(Age_years, na.rm = TRUE), collapse = " - "),
    
    BMI_mean = mean(BMI, na.rm = TRUE),
    BMI_sd = sd(BMI, na.rm = TRUE),
    BMI_range = paste0(range(BMI, na.rm = TRUE), collapse = " - "),
    
    HbA1c_mean = mean(Donor_HbA1c, na.rm = TRUE),
    HbA1c_sd = sd(Donor_HbA1c, na.rm = TRUE),
    HbA1c_range = paste0(range(Donor_HbA1c, na.rm = TRUE), collapse = " - ")
  )

# View the results
print(summary_stats)