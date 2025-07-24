# RechumaGWAS
all files and instructions for adding to and editing the gwas_viewer 
# GWAS Viewer Application

## Project Overview

This repository contains the code for the GWAS Viewer, a Shiny application designed to visualize and explore large-scale GWAS results. The application is supported by a set of Plumber APIs that provide fast, indexed access to the underlying data.

This document provides a step-by-step guide for adding a new GWAS dataset to the application.

---

## How to Add a New Dataset

Adding a new dataset involves a multi-step data preparation pipeline. Follow these steps in order.

### Prerequisites

1.  **Raw Data:** Your new GWAS summary statistics must be in a flat file format (e.g., `.csv`, `.tsv`) with at least the following columns: `Chromosome`, `Position`, `P_value`.
2.  **Required Columns:** The data preparation scripts require the following columns to be present and correctly named:
    * `Chromosome`
    * `Position`
    * `P_value`
    * `LOG10P` (If this column doesn't exist, you must create it by calculating `-log10(P_value)`)

### Step 1: Add the New Dataset to the Main Database

The first step is to add your new, cleaned summary statistics as a new table inside the main `gwas_data_v2.sqlite` database.

1.  **Write a simple R script** to read your flat file.
2.  **Connect to the main database:**
    ```R
    library(DBI)
    library(RSQLite)
    con <- dbConnect(RSQLite::SQLite(), "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_data_v2.sqlite")
    ```
3.  **Write your data to a new table.** The table name should be a simple, unique identifier for your study (e.g., `my_new_study_2025`). This ID will be used throughout the app.
    ```R
    # 'new_gwas_data' is the data frame you loaded from your file
    dbWriteTable(con, name = "my_new_study_2025", value = new_gwas_data)
    dbDisconnect(con)
    ```

### Step 2: Re-run the Data Preparation Pipeline

Now that the raw data is in the main database, you must re-run the three main processing scripts. These scripts will update all the filtered chromosome, index, and summary files to include your new data.

Run these scripts from your R console in the following order:

1.  **Update the Chromosome Files:**
    ```R
    source("data_prep_scripts/create_all_filtered_dbs.R")
    ```
2.  **Update the Index Files:**
    ```R
    source("data_prep_scripts/create_all_indexes.R")
    ```
3.  **Update the Summary File:**
    ```R
    source("data_prep_scripts/create_summary_db.R")
    ```

### Step 3: Pre-render the New Plots

You must generate the Manhattan and QQ plot images for your new study.

1.  **Add your new study** to the script that generates the plots.
2.  **Run the plot generation script.** This will create the new `.png` files and save them in the `www` folder of the frontend application.

### Step 4: Update the Shiny App Configuration

The frontend app needs to be told that a new study exists so it can be added to the dropdown menu.

1.  **Open the `app.R` file** for the frontend application.
2.  **Find the `dataset_catalog` list** at the top of the file.
3.  **Add a new entry** for your study. The `id` must exactly match the table name you created in Step 1.
    ```R
    dataset_catalog <- list(
      # ... other studies ...
      list(id = "my_new_study_2025", trait = "My New Study 2025")
    )
    ```

### Step 5: Re-deploy the APIs and the App

Finally, you must re-deploy the updated components to the Posit Connect server.

1.  **Re-deploy the APIs:** Because the underlying data has changed, you must re-deploy all three of your backend API bundles (`api_1`, `api_2`, `api_3`).
2.  **Re-deploy the Frontend:** Because you've updated the `app.R` and added new plot images to the `www` folder, you must re-deploy the frontend application.

After these steps are complete, your new dataset will be fully integrated and visible in the live application.
