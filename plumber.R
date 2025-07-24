#plumber.R - Final Universal Template

# --- Libraries ---
library(plumber)
library(DBI)
library(RSQLite)
library(stringr)

# --- Configuration ---
DB_DIR <- "." # Look for data in the current directory

#* @apiTitle GWAS Final API

#* @param chromosome:string The chromosome to search
#* @param position:int The base pair position to search
#* @post /phewas
function(chromosome, position) {
  
  # Check if this API bundle has the requested chromosome
  data_db_path <- file.path(DB_DIR, paste0("chr_", chromosome, ".sqlite"))
  if (!file.exists(data_db_path)) {
    return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API"))
  }
  # --- Step 1: Query the INDEX file ---
  index_db_path <- file.path(DB_DIR, paste0("chr_", chromosome, "_index.sqlite"))
  if (!file.exists(index_db_path)) {
    return(data.frame(Message = "Index database not found for this chromosome."))
  }
  
  index_con <- dbConnect(RSQLite::SQLite(), dbname = index_db_path)
  index_query <- "SELECT studies FROM position_index WHERE chunk_start <= ? AND chunk_end >= ?"
  relevant_studies_record <- dbGetQuery(index_con, index_query, params = list(position, position))
  dbDisconnect(index_con)
  
  if (nrow(relevant_studies_record) == 0) {
    return(data.frame(Message = "Position not found in any study for this chromosome."))
  }
  studies_to_search_str <- relevant_studies_record$studies[1]
  
  # --- Step 2: Query the DATA file ---
  data_con <- dbConnect(RSQLite::SQLite(), dbname = data_db_path)
  on.exit(dbDisconnect(data_con))
  
  studies_vector <- str_split(studies_to_search_str, ",", simplify = TRUE)[1,]
  placeholders <- paste(rep("?", length(studies_vector)), collapse = ",")
  data_query <- paste0(
    "SELECT * FROM gwas_data WHERE Position = ? AND StudyID IN (", placeholders, ")"
  )
  params_list <- c(list(position), as.list(studies_vector))
  result <- dbGetQuery(data_con, data_query, params = params_list)
  
  if (nrow(result) > 0) {
    return(result)
  } else {
    return(data.frame(Message = "Position not found in any study for this chromosome."))
  }
}

# --- Endpoint 2: Region Search (for plot clicks) ---
#* @param chromosome:string The chromosome to search
#* @param start_pos:int The starting base pair position
#* @param end_pos:int The ending base pair position
#* @param study_id:string The study to search
#* @post /region
function(chromosome, start_pos, end_pos, study_id) {
  
  data_db_path <- file.path(DB_DIR, paste0("chr_", chromosome, ".sqlite"))
  if (!file.exists(data_db_path)) {
    return(data.frame(Message = "CHROMOSOME_NOT_IN_THIS_API"))
  }
  
  data_con <- dbConnect(RSQLite::SQLite(), dbname = data_db_path)
  on.exit(dbDisconnect(data_con))
  
  # Query for all SNPs within the given range AND for the specific study
  query <- "SELECT * FROM gwas_data WHERE Position >= ? AND Position <= ? AND StudyID = ?"
  result <- dbGetQuery(data_con, query, params = list(start_pos, end_pos, study_id))
  
  return(result)
}

  
# --- Endpoint 2: Summary Data ---
#* @param study_id:string The identifier for the study
#* @post /summary
function(study_id) {
  
  db_path <- file.path(DB_DIR, "summary_data.sqlite")
  if (!file.exists(db_path)) {
    return(data.frame(Message = "SUMMARY_DATA_NOT_IN_THIS_API"))
  }
  
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  on.exit(dbDisconnect(con))
  
  query <- "SELECT * FROM summary_hits WHERE StudyID = ?"
  result <- dbGetQuery(con, query, params = list(study_id))
  
  return(result)
}
