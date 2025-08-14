## Running the Scripts in Order
- Requried software: R version 4.5.0 
- Packages required: Each R code file contains a list of R packages needed at the beginning
- R version the R code has been tested on: R version 4.5.0
- Installation guide: R software and packages can be downloaded from https://cran.r-project.org/. Then please follow the instruction given at https://cran.r-project.org/ to have R installed on your local machine. Installation of R will not take more than 10 minutes.
- Any required non-standard hardware: None
- Expected run time for each R script: less than 10 minutes  but it heavily depends on the CPU specifications
- Instruction to run the R script: Copy and paste the script on R console

Each script’s filename begins with a number indicating its execution order. Follow this sequence to ensure proper workflow:

Step 1: Data Cleaning

- Run 00_CleanData_Meta_Scale.R to clean the meta-data.
- Run 00_CleanData_Genetic.R to clean the genetic data.

Alternatively, you can skip the data cleaning step and use the pre-processed data available in the DATA folder:

- Data/data_process_meta.RData
- Data/data_process_gen_dat.RData

Step 2: Data Analysis

- Run 01_Figure1B&C.R to generate the first figure.
- Continue executing scripts in numerical order to maintain the correct sequence.


Notes: Same report shared for figures



## Summary of Figures Generated from the Same Code

Some figures are generated from the same code, so duplicated results are not listed separately. 

The duplicated figures and their corresponding codes are:

- **Figure 6D** is the same as:
  - Extended Data Figure 4E & F (Genetic ancestry)
  - Extended Data Figure 5 (Global comparison: INS / GLU traits ~ Genetic ancestry)
  - Extended Data Figure 7 B (Global comparison: Cell composition ~ Genetic ancestry)
  - Extended Data Figure 7 D (Global comparison: Hormone content ~ Genetic ancestry)

- **Figure 6B** is the same as:
  - Extended Data Figure 7 B (Cell composition ~ Sex)

- **Figure 6C** is the same as:
  - Extended Data Figure 7 B (Cell composition ~ Reported race)

- **Figure 6F & J** is the same as:
  - Extended Data Figure 7 B (Cell composition ~ Center)
  - Extended Data Figure 7 D (Hormone content ~ Sex)

- **Figure 6G & K** is the same as:
  - Extended Data Figure 7 D (Hormone content ~ Reported race)



