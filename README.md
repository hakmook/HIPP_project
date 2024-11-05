## Data Analysis for Human Islet Phenotyping Program

### Overview

	project
	|- README          # the top level description of content (this doc)
	|+- CONTRIBUTING    # instructions for how to contribute to your project
	|
	|- Data           # raw and primary data, are not changed once created
	| |- references/  # reference files to be used in analysis
	| |- raw/         # raw data, will not be altered
	| +- process/     # cleaned data, will not be altered once created;
 	|
	|- Codes/          # any programmatic code
	|
	|- Results        # all output from workflows and analyses
	| |- tables/      # text version of tables to be rendered with kable in R
	| |- figures/     # graphs, likely designated for manuscript figures
	| +- pictures/    # diagrams, images, and other non-graph graphics
	|
	|- Manuscripts/   # exploratory data analysis for study
	| |- notebook/    # preliminary analyses
	| +- scratch/     # temporary files that can be safely deleted or lost
