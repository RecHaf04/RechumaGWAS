library(DBI)
library(RSQLite)
library(dplyr)
library(here)

# --- 2. CONFIGURATION ---
setwd(here::here())
DB_DIR <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_viewer/backend_api/api_3" 
CHUNK_SIZE <- 100000
# --- 3. MAIN LOGIC ---
message("Starting creation of per-chromosome indexes...")
all_chr_files <- list.files(path = DB_DIR, pattern = "^chr_[0-9XY]+\\.sqlite$", full.names = TRUE)
for (db_path in all_chr_files) {
  index_path <- sub("\\.sqlite", "_index.sqlite", db_path)
  message("\nProcessing: ", basename(db_path))
  
  if (file.exists(index_path)) {
    message(" -> Index file already exists. Skipping.")
    next
  }
  
  tryCatch({
    con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
    on.exit(dbDisconnect(con))
    message(" -> Creating Position and MarkerName indexes on main data file (if they don't exist)...")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_position ON gwas_data (Position)")
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_marker_name ON gwas_data (MarkerName)")
    
    # Get the full range of positions for this chromosome
    pos_range <- dbGetQuery(con, "SELECT MIN(Position) as min_pos, MAX(Position) as max_pos FROM gwas_data")
    
    # Define the chunks to process
    chunks <- seq(from = pos_range$min_pos, to = pos_range$max_pos, by = CHUNK_SIZE)
    
    # Create the new, separate index file and table
    index_con <- dbConnect(RSQLite::SQLite(), dbname = index_path)
    on.exit(dbDisconnect(index_con), add = TRUE)
    dbExecute(index_con, "CREATE TABLE position_index (chunk_start INTEGER, chunk_end INTEGER, studies TEXT)")
    
    message(" -> Building position_index in ", basename(index_path), "...")
    
    # Loop through each chunk, find the studies present, and write to the index
    for (i in 1:length(chunks)) {
      chunk_start <- chunks[i]
      chunk_end <- chunk_start + CHUNK_SIZE - 1
      
      query <- "SELECT DISTINCT StudyID FROM gwas_data WHERE Position >= ? AND Position < ?"
      studies_in_chunk <- dbGetQuery(con, query, params = list(chunk_start, chunk_end))
      
      if (nrow(studies_in_chunk) > 0) {
        studies_str <- paste(studies_in_chunk$StudyID, collapse = ",")
        dbExecute(
          index_con,
          "INSERT INTO position_index (chunk_start, chunk_end, studies) VALUES (?, ?, ?)",
          params = list(chunk_start, chunk_end, studies_str)
        )
      }
    }
    
    message(" -> Index file created successfully for ", basename(db_path))
    
  }, error = function(e) {
    message("  !!!! FAILED to process ", basename(db_path), ". Error: ", e$message)
  })
}
message("\nAll indexing tasks complete. ✅")
