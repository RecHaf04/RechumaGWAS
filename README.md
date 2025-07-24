# RechumaGWAS
all files and instructions for adding to and editing the gwas_viewer 
# GWAS Viewer Application

## Project Overview

This repository contains the code for the GWAS Viewer, a Shiny application designed to visualize and explore large-scale GWAS results. The application is supported by a set of Plumber APIs that provide fast, indexed access to the underlying data.



## How to Add a New Dataset

Adding a new dataset involves a lot of steps, more than there are in the deepest subways station. Follow these steps in order.

### Prerequisites

1.  **Raw Data:** GWAS summary statistics must be in a flat file format (e.g., `.csv`, `.tsv`) with at least the following columns: `Chromosome`, `Position`, `P_value`.
2.  **Required Columns:** The data preparation scripts require the following columns to be present and named:
    * `Chromosome`
    * `Position`
    * `P_value`
    * `LOG10P` 

### Step 1: Add the New Dataset to the Main Database

The first step is to add your new, cleaned summary statistics as a new table inside the main `gwas_data_v2.sqlite` database.
 Run the file titled "create gwas_data_v2.sqlite" Currently it is 185.5 gb. 

### Step 2: Re-run the Data Preparation Pipeline

Now that the raw data is in the main database, you must re-run the three main processing scripts

Run these scripts from your R console in the following order:

  source("data_prep_scripts/create_all_filtered_dbs.R")
    source("data_prep_scripts/create_all_indexes.R")
    source("data_prep_scripts/create_summary_db.R")


### Step 3: Pre-render the New Plots

You must generate the Manhattan and QQ plot images for your new study.

1.  **Add your new study** to the script that generates the plots.
2.  **Run the plot generation script.** This will create the new `.png` files and save them in the `www` folder of the frontend application.

### Step 4: Update the Shiny App Configuration

The frontend app needs to be told that a new study exists so it can be added to the dropdown menu.

1.  **Open the `app.R` file** for the frontend application.
2.  **Find the `dataset_catalog` list** at the top of the file.
3.  **Add a new entry** for your study. The `id` must exactly match the table name you created for the new dataset.
    ```R
    dataset_catalog <- list(
      # ... other studies ...
      list(id = "my_new_study_2025", trait = "My New Study 2025")
    )
    ```

### Step 5: Re-deploy the APIs and the App

Finally, you must re-deploy the updated components to the Posit Connect server.

1.  **Re-deploy the APIs:** Because the underlying data has changed, you must re-deploy all three of your backend API bundles (`api_1`, `api_2`, `api_3`). There is a max limit of 8gb per API, so if needed, just make an api_4 folder.
2.  **Re-deploy the Frontend:** Because you've updated the `app.R` and added new plot images to the `www` folder, you must re-deploy the frontend application.

When it inevitably fails, congrats! You are now one of my elite employees. Lol good luck fixing things.
