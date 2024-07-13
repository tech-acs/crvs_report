# crvs_report: A CRVS Report R Pipeline

This repository contains the scripts and configuration files needed to run the Civil Registration and Vital Statistics (CRVS) report generation pipeline using R.

## Overview

The CRVS Report R pipeline processes raw data related to births, deaths, marriages, and divorces to generate various statistical tables and figures.
The key components of this pipeline are:

- A configuration file `config.yml` that specifies data paths, output paths, variable mappings, and table selection for each pipeline run.
- An R script `import_pipeline.R` that performs data processing and table generation.

## Clone or Download the Repository

To get started with the CRVS Report R pipeline, you need to clone or download the repository from GitHub.

### Clone the Repository

If you have Git installed, you can clone the repository using the following command:

```sh
git clone https://github.com/tech-acs/crvs_report.git
```

### Download the Repository

Alternatively, you can download the repository as a ZIP file:

1. Go to the repository page on GitHub: https://github.com/tech-acs/crvs_report
2. Click the "Code" button.
3. Select "Download ZIP".
4. Extract the downloaded ZIP file to your desired location.

## Configuration

The `config.yml` file contains several sections:

- Data Paths: Specifies the paths for raw and processed data.
- Output Paths: Specifies where the output tables and figures will be saved.
- Table Selection: Allows for selecting which tables to run.
- Variable Mappings: Maps raw data variables to standardized variable names for births, deaths, marriages, and divorces.
- Table Dictionary: Defines the tables to be generated, including their IDs, names, corresponding functions, and function arguments.

## Running the Pipeline

To run the pipeline, ensure you have R and the necessary packages installed.

Then, execute the `import_pipeline.R` script:

```sh
Rscript import_pipeline.R
```

This will process the raw data, apply the variable mappings, generate the selected tables, and save the outputs to the specified paths.

## Customizing the Pipeline

To customize the pipeline, you can modify the `config.yml` file:

- Data Paths: Change the paths for raw and processed data.
- Output Paths: Update where the output tables and figures will be saved.
- Table Selection: Select which tables to run by specifying their IDs.
- Variable Mappings: Adjust the mappings to align with your raw data variables.
- Table Dictionary: Add or modify tables and their corresponding functions and arguments.
