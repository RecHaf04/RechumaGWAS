library(plumber)
library(DBI)
library(RSQLite)
library(stringr)
library(dplyr)

# --- Configuration ---
DB_DIR <- "." # Look for data in the current directory

#* @apiTitle GWAS Final API

# --- Endpoint 1: PheWAS Search (by Position) ---
#* @param chromosome:string The chromosome to search (e.g., "6" or "chr6")
#* @param position:int The base pair position to search
#* @post /phewas
function(chromosome, position) {
  # Standardize chromosome name by removing "chr" prefix
  chr_num <- sub("^chr", "", chromosome, ignore.case = TRUE)
  
  data_db_path <- file.path(DB_DIR, paste0("chr_", chr_num, ".sqlite"))
  if (!file.exists(data_db_path)) {
    return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API"))
  }
  index_db_path <- file.path(DB_DIR, paste0("chr_", chr_num, "_index.sqlite"))
  if (!file.exists(index_db_path)) {
    return(data.frame(Message = "Index database not found for this chromosome."))
  }
  
  # Query the position index to find which studies to search
  index_con <- dbConnect(RSQLite::SQLite(), dbname = index_db_path)
  index_query <- "SELECT studies FROM position_index WHERE chunk_start <= ? AND chunk_end >= ?"
  relevant_studies <- dbGetQuery(index_con, index_query, params = list(position, position))
  dbDisconnect(index_con)
  
  if (nrow(relevant_studies) == 0) {
    return(data.frame(Message = "Position not found in any study."))
  }
  
  # Now query the main data file for only those specific studies
  data_con <- dbConnect(RSQLite::SQLite(), dbname = data_db_path)
  on.exit(dbDisconnect(data_con))
  
  studies_vector <- str_split(relevant_studies$studies[1], ",", simplify = TRUE)[1,]
  placeholders <- paste(rep("?", length(studies_vector)), collapse = ",")
  data_query <- paste0("SELECT * FROM gwas_data WHERE Position = ? AND StudyID IN (", placeholders, ")")
  
  # CORRECTED: The 'params' argument should be a single list
  params_list <- append(list(position), as.list(studies_vector))
  result <- dbGetQuery(data_con, data_query, params = params_list)
  
  return(result)
}


# --- Endpoint 2: MarkerName Search (Simplified & Fast) ---
#* @param marker_name:string The MarkerName to search for
#* @post /marker
function(marker_name) {
  # Extract chromosome from MarkerName
  target_chr_num <- sub("^chr", "", str_split(marker_name, ":")[[1]][1], ignore.case = TRUE)
  
  data_db_path <- file.path(DB_DIR, paste0("chr_", target_chr_num, ".sqlite"))
  if (!file.exists(data_db_path)) {
    return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API"))
  }
  con <- dbConnect(RSQLite::SQLite(), dbname = data_db_path)
  on.exit(dbDisconnect(con))
  
  # The run_indexing.R script already created a fast index on MarkerName
  # This query will now be nearly instantaneous
  marker_with_chr <- if (!startsWith(marker_name, "chr")) paste0("chr", marker_name) else marker_name
  marker_without_chr <- if (startsWith(marker_name, "chr")) sub("^chr", "", marker_name) else marker_name
  
  query <- "SELECT * FROM gwas_data WHERE MarkerName = ? OR MarkerName = ?"
  result <- dbGetQuery(con, query, params = list(marker_with_chr, marker_without_chr))
  
  if (nrow(result) == 0) {
    return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API"))
  }
  return(result)
}

# --- Other endpoints like /region, /summary, /chromosomes would go here ---
# Your existing code for these is fine and can be pasted here.
# --- Endpoint 3: Region Search (for plot clicks) ---
#* @param chromosome:string The chromosome to search
#* @param start_pos:int The starting base pair position
#* @param end_pos:int The ending base pair position
#* @param study_id:string The study to search within
#* @post /region
function(chromosome, start_pos, end_pos, study_id) {
  # ... (This function's code remains exactly the same) ...
  data_db_path <- file.path(DB_DIR, paste0("chr_", chromosome, ".sqlite"))
  if (!file.exists(data_db_path)) { return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API")) }
  data_con <- dbConnect(RSQLite::SQLite(), dbname = data_db_path)
  on.exit(dbDisconnect(data_con))
  query <- "SELECT * FROM gwas_data WHERE Position >= ? AND Position <= ? AND StudyID = ?"
  result <- dbGetQuery(data_con, query, params = list(start_pos, end_pos, study_id))
  return(result)
}
# --- Endpoint 4: Summary Data ---
#* @param study_id:string The identifier for the study
#* @post /summary
function(study_id) {
  # ... (This function's code remains exactly the same) ...
  db_path <- file.path(DB_DIR, "summary_data.sqlite")
  if (!file.exists(db_path)) { return(data.frame(Message = "SUMMARY_DATA_NOT_IN_THIS_API")) }
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(dbDisconnect(con))
  query <- "SELECT * FROM summary_hits WHERE StudyID = ?"
  result <- dbGetQuery(con, query, params = list(study_id))
  return(result)
}
# --- Endpoint 5: Get Chromosomes for a Study ---
#* @param study_id:string The identifier for the study
#* @post /chromosomes
function(study_id) {
  
  # Find all chromosome files in this API's bundle
  all_chr_files <- list.files(path = DB_DIR, pattern = "^chr_.*\\.sqlite$", full.names = TRUE)
  all_chr_files <- all_chr_files[!grepl("_index", all_chr_files)]
  
  all_chroms <- c()
  
  # Loop through each file to see if the study has data on that chromosome
  for (db_path in all_chr_files) {
    con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
    # This is a very fast query to just check for existence
    query <- "SELECT 1 FROM gwas_data WHERE StudyID = ? LIMIT 1"
    result <- dbGetQuery(con, query, params = list(study_id))
    dbDisconnect(con)
    
    if (nrow(result) > 0) {
      # If the study exists in this file, add the chromosome number to our list
      # We extract the chromosome number from the filename
      chrom_num <- gsub("chr_|\\.sqlite", "", basename(db_path))
      all_chroms <- c(all_chroms, chrom_num)
    }
  }
  
  if (length(all_chroms) > 0) {
    return(unique(all_chroms))
  } else {
    # This tells the app to check the next API if no chromosomes were found in this one
    return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API"))
  }
}
