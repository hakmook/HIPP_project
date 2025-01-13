## Running the Scripts in Order

The value at the beginning of each file name indicates the order in which the scripts should be executed. For example:

Start with data cleaning:

- Run 00_CleanData_Meta.R to clean the meta-data.
- Then, run 01_CleanData_Genetic.R to clean the genetic data.

Proceed with subsequent steps:
- After cleaning the data, run 02_Table1.R to generate the first table.
- Always follow the numerical order to ensure that the scripts are executed in the correct sequence.