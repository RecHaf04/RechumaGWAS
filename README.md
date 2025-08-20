# GWAS Interactive Visualization App

This application provides a high-performance, interactive web interface for exploring and comparing results from multiple Genome-Wide Association Studies (GWAS). It is built with R Shiny and is designed for fast, responsive data exploration.

## Core Features

-   **Interactive Manhattan Plot**: Hybrid plot system displays a static, pre-rendered PNG for fast initial loading with an invisible overlay of significant points for interactivity.
-   **High Performance**: Powered by a pre-computed local SQLite database of significant hits, ensuring the app remains fast and responsive. The backend API uses a sharded database architecture with pre-built indexes.
-   **Dynamic Hover and Click**: Hover over significant points to view MarkerNames and click to auto-populate the cross-phenotype search bar.
-   **Cross-Phenotype Search**: Search for SNP associations across all studies by MarkerName or genomic position.
-   **Top Significant Hits**: Display dataset-specific points by descending -log10P
-   **Locus/Forest Plots**: For genome-wide significant SNPs, display pre-rendered LocusZoom and Forest plots. (in process)
-   **Calibration Mode**: Tool to allow for precise alignement of the interactive layer. 

---
## Project Structure & Architecture


-   `app.R`: The main Shiny application script containing all UI and Server logic.
-   **Data Prep Scripts**: Offline scripts used to process raw data and generate all necessary assets.
-   **Backend Data (`gwas_data_v2.sqlite`, `chr_?.sqlite`)**: The full GWAS summary statistics are stored in a master database and sharded into separate databases for each chromosome.
-   **Plumber APIs**: Provide endpoints to query the `chr_?.sqlite` databases.
-   `www/`: Containing pre-rendered static PNGs for the Manhattan and QQ plots of each study.
-   `interactive_hits.sqlite`: Local database containing only the significant hits for all studies, bundled with the Shiny app.
-   `calibration_settings.csv`: A configuration file that stores the precise alignment values for each study's interactive plot layer.

---

## How to Add a New Dataset

This guide outlines the complete pipeline for adding a new GWAS dataset, or for modeling the application using new datasets. Follow these steps in the correct order.

### **Stage 1: Update Master Database & Generate Backend Files**

1.  **Add to Master Database**: The first step is to add your new, cleaned summary statistics as a new table inside the main `gwas_data_v2.sqlite` database. The name of the new table must be the `StudyID` for the new dataset. Currently with all online datasets, the file is 185.5 GB. 
```r
source("make_gwas_data_v2.R")
```

2.  **Create Chromosome Databases**: Run the script that processes the master `gwas_data_v2.sqlite` and generates the updated, sharded `chr_?.sqlite` files and a summary file. This script should overwrite the old chromosome-specific databases.
    ```r
    source("data_prep_scripts/create_chromosome_dbs.R")
    source("data_prep_scripts/create_summary_sqlite.R")
    ```

3.  **Create All Indexes**: Run the script that creates the necessary `Position` and `MarkerName` indexes on the new `chr_?.sqlite` files, as well as the companion `chr_?_index.sqlite` files.
    ```r
    source("data_prep_scripts/run_indexing.R")
    ```

### **Stage 2: Deploy Backend & Generate Frontend Assets**

4.  **Re-deploy Plumber APIs**: With the backend databases updated and re-indexed, you must re-deploy all three of your backend API bundles (`api_1`, `api_2`, `api_3`). 
```r
#In R Console
rsconnect::deployApp(appDir = "api_1", appName = "api_1")
rsconnect::deployApp(appDir = "api_2", appName = "api_2")
rsconnect::deployApp(appDir = "api_3", appName = "api_3")
```

5.  **Render New Plot Images**: Generate the static PNGs for your new study.
    -   Update your `render_all_plots.R` script to include the new study in its list.
    -   Run the script. It will query gwas_data_v2.sqlite, create the new `.png` files for any files without already preexisting images in the folder, and save them in the `www/` folder.
```r
source("/data_prep_scripts/render_all_plots")
```

6.  **Re-create the Interactive Hits Database**: This database powers the app's interactive layer.
    -   Update your `prepare_interactive_db.R` script to include the new `StudyID`.
    -   Run the script to generate a new `interactive_hits.sqlite` file:
        ```r
        source("prepare_interactive_db.R")
        ```

### **Stage 3: Update & Calibrate the Shiny App**

7.  **Update App Metadata**:
    -   Open `app.R` and add a new row to the `study_metadata` tibble for your new study. Ensure the `StudyID` here exactly matches the name used in all other steps.

8.  **Calibrate Plot Alignment**:
    -   set `calibration_mode` to TRUE
    -   Move the `calibration_settings.csv` to one level up int the directory to prevent the app from restarting when you save.
    -   Run app locally, and adjust the sliders until the points perfectly overlap the png.
    -   Click `Save Current Settings` which automatically updates `calibration_settings.csv`

### **Stage 4: Final Deployment**

9.  **Finalize and Deploy**:
    -  set `calibration_mode` to FALSE
    -  MOVE `calibration_settings.csv` back into the `shiny_frontend` folder
    -  redeploy the app
    -  ```R
       rsconnect::deployApp(appName = "gwas_viewer")
       ```
    -  rsc
    -  
    -      -   Re-deploy the final, updated Shiny application.
