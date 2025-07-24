library(DBI)
library(RSQLite)
library(dplyr)
db_dir <- "backend_api/chromosome_dbs"


chunk_size <- 1000000



  
  for (chrom in target_chromosomes) {
    
    cat("====================================================\n")
    cat("Checking for Chromosome:", chrom, "\n")
    
    # Define the paths for the source data and the new index file
    source_db_path <- file.path(db_dir, paste0("chr_", chrom, ".sqlite"))
    index_db_path <- file.path(db_dir, paste0("chr_", chrom, "_index.sqlite"))
    if (file.exists(index_db_path)) {
      cat("Index file for chromosome", chrom, "already exists. Skipping.\n")
      next
    }
    
    # --- Check if the source data file exists before trying to index ---
    if (!file.exists(source_db_path)) {
      cat("Source data file for chromosome", chrom, "not found. Skipping.\n")
      next
    }
    cat("-> Indexing data for Chromosome", chrom, "...\n")
    
    # --- Start of single-chromosome indexing logic ---
    
    # Connect to the chromosome data file
    con <- dbConnect(RSQLite::SQLite(), dbname = source_db_path)
    
    # Find the genomic range for this chromosome
    range_query <- "SELECT MIN(Position) as min_pos, MAX(Position) as max_pos FROM gwas_data"
    genomic_range <- dbGetQuery(con, range_query)
    
    min_pos <- genomic_range$min_pos
    max_pos <- genomic_range$max_pos
    if (is.na(min_pos)) {
      cat("-> No data to index for Chromosome", chrom, ". Skipping.\n")
      dbDisconnect(con)
      next
    }
    
    cat("-> Range found: from", min_pos, "to", max_pos, "\n")
    
    index_records <- list()
    current_pos <- min_pos
    while (current_pos <= max_pos) {
      chunk_start <- current_pos
      chunk_end <- current_pos + chunk_size - 1
      
      studies_query <- "SELECT DISTINCT StudyID FROM gwas_data WHERE Position >= ? AND Position <= ?"
      studies_in_chunk <- dbGetQuery(con, studies_query, params = list(chunk_start, chunk_end))
      
      if (nrow(studies_in_chunk) > 0) {
        studies_string <- paste(studies_in_chunk$StudyID, collapse = ",")
        record <- data.frame(chunk_start = chunk_start, chunk_end = chunk_end, studies = studies_string)
        index_records[[length(index_records) + 1]] <- record
      }
      current_pos <- current_pos + chunk_size
    }
    
    # Disconnect from the source data file
    dbDisconnect(con)
    
    # Write the collected index records to a new database file
    if (length(index_records) > 0) {
      final_index_data <- bind_rows(index_records)
      cat("-> Writing", nrow(final_index_data), "index records for Chromosome", chrom, "...\n")
      index_con <- dbConnect(RSQLite::SQLite(), dbname = index_db_path)
      dbWriteTable(index_con, name = "position_index", value = final_index_data, overwrite = TRUE)
      dbExecute(index_con, "CREATE INDEX idx_position ON position_index (chunk_start, chunk_end)")
      dbDisconnect(index_con)
      
      cat("-> Successfully created index file:", index_db_path, "\n")
    } else {
      cat("-> No data found to index for Chromosome", chrom, ".\n")
    }
  }
  cat("====================================================\n")
  cat("All indexing tasks are complete.\n")
}

# --- Run the Script ---
create_genomic_index()
