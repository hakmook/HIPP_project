## Running the Scripts in Order

Each script’s filename begins with a number indicating its execution order. Follow this sequence to ensure proper workflow:

Step 1: Data Cleaning

- Run 00_CleanData_Meta.R to clean the meta-data.
- Then, run 01_CleanData_Genetic.R to clean the genetic data.

Alternatively, you can skip the data cleaning step and use the pre-processed data available in the DATA folder:

- DATA/data_process_meta.RData
- DATA/data_process_gen_dat.RData

Step 2: Data Analysis

- Run 02_Table1.R to generate the first table.
- Continue executing scripts in numerical order to maintain the correct sequence.






