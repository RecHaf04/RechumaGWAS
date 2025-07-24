library(DBI)
library(RSQLite)
library(dplyr)
library(here)

source_db_path <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_data_v2.sqlite"

output_dir <- "backend_api/chromosome_dbs"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
log10p_threshold <- 1 
target_chromosomes <- c(1:22, "X", "Y")
create_all_filtered_databases <- function() {
  
  # Connect to the large source database
  cat("Connecting to the main database:", source_db_path, "\n")
  con <- dbConnect(RSQLite::SQLite(), dbname = source_db_path)
  on.exit(dbDisconnect(con)) # Ensure connection is closed when done
  
  all_tables <- dbListTables(con)
  cat("Found", length(all_tables), "study tables to process.\n\n")
  
  for (chrom in target_chromosomes) {
    
    # --- RESUME LOGIC: Check if the output file already exists ---
    output_file_path <- file.path(output_dir, paste0("chr_", chrom, ".sqlite"))
    if (file.exists(output_file_path)) {
      cat("====================================================\n")
      cat("Output file for chromosome", chrom, "already exists. Skipping.\n")
      next # This command skips to the next chromosome
    }
    cat("====================================================\n")
    cat("STARTING PROCESSING FOR CHROMOSOME:", chrom, "\n")
    cat("====================================================\n")
    
    # This list will hold data chunks for the current chromosome
    all_data_for_chrom <- list()
    
    for (table_name in all_tables) {
      
      cat("  -> Querying table '", table_name, "' for chromosome '", chrom, "'...\n", sep="")
      
      # Build the query with the correct column names and filter
      query <- paste0("SELECT * FROM \"", table_name, "\" WHERE Chromosome = ? AND LOG10P > ?")
      
      filtered_data_chunk <- tryCatch({
        dbGetQuery(con, query, params = list(as.character(chrom), log10p_threshold))
      }, error = function(e) {
        cat("    !!!! WARNING: Could not process table '", table_name, "'. Error: ", e$message, "\n", sep="")
        return(NULL)
      })
      if (!is.null(filtered_data_chunk) && nrow(filtered_data_chunk) > 0) {
        filtered_data_chunk$StudyID <- table_name
        all_data_for_chrom[[table_name]] <- filtered_data_chunk
        cat("    -> Found", nrow(filtered_data_chunk), "rows.\n")
      }
    }
    if (length(all_data_for_chrom) > 0) {
      cat("\n  Combining data from", length(all_data_for_chrom), "studies for chromosome", chrom, "...\n")
      final_chrom_data <- bind_rows(all_data_for_chrom)
      
      cat("  Writing", nrow(final_chrom_data), "total rows to", output_file_path, "\n\n")
      
      new_con <- dbConnect(RSQLite::SQLite(), dbname = output_file_path)
      dbWriteTable(new_con, name = "gwas_data", value = final_chrom_data, overwrite = TRUE)
      dbDisconnect(new_con)
      
    } else {
      cat("\n  No data found for chromosome", chrom, "with LOG10P >", log10p_threshold, "\n\n")
    }
  }
    cat("processing complete")
}
    create_all_filtered_databases()
    
