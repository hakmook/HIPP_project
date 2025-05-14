## Running the Scripts in Order

Each script’s filename begins with a number indicating its execution order. Follow this sequence to ensure proper workflow:

Step 1: Data Cleaning

- Run 00_CleanData_Meta_Scale.R to clean the meta-data.
- Run 00_CleanData_Genetic.R to clean the genetic data.

Alternatively, you can skip the data cleaning step and use the pre-processed data available in the DATA folder:

- Data/data_process_meta.RData
- Data/data_process_gen_dat.RData

Step 2: Data Analysis

- Run 01_Figure1B_1C.R to generate the first figure.
- Continue executing scripts in numerical order to maintain the correct sequence.






