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


Notes: Same report shared for figures



## Summary of Figures That Are the Same

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



