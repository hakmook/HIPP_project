## Data Analysis for Human Islet Phenotyping Program

### Overview

	Project Structure: This guide provides an overview of the structure and contents of the project repository.
	|- README         # Overview of the project contents and setup (this document)
	|- CONTRIBUTING   # Guidelines for contributing to the project
	|
	|- Data           # Contains all raw and primary data, unaltered once created
	| |- references/  # Reference files for use in analysis
	| |- raw/         # Raw data, preserved in original format
	| +- process/     # Cleaned data, fixed after initial processing
 	|
	|- Codes/         # Programmatic scripts and code for data processing and analysis
	|
	|- Results        # Outputs from workflows and analyses
	| |- reports/     # Analysis reports in formats like HTML, Word, or PDF (rendered with kable in R)
	| |- tables/      # Text-formatted tables for easy referencing
	| |- figures/     # Graphs intended for manuscript use
	| +- pictures/    # Diagrams, images, and other graphics
	|
	|- Manuscripts    # Manuscripts and drafts ready for submission
