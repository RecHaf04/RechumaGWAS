# prepare_summary_db.R
library(DBI); library(RSQLite); library(dplyr); library(here)


# Configuration
source_db_path <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_data_v2.sqlite"
output_dir <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_viewer/backend_api/chromosome_dbs/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
top_n_rows <- 1000

create_summary_database <- function() {
  
  # Connect to the large source database
  cat("Connecting to the main database:", source_db_path, "\n")
  con <- dbConnect(RSQLite::SQLite(), dbname = source_db_path)
  on.exit(dbDisconnect(con))
  all_tables <- dbListTables(con)
  cat("Found", length(all_tables), "study tables to process.\n\n")
  
  # This list will hold the top hits from each study
  all_summary_data <- list()
  for (table_name in all_tables) {
    cat(" -> Getting top", top_n_rows, "hits from '", table_name, "'...\n", sep="")
    
    # Build a query to get the top N rows, ordered by LOG10P descending
    query <- paste0(
      "SELECT * FROM \"", table_name, "\"",
      " ORDER BY LOG10P DESC LIMIT ", top_n_rows
    )
    top_hits <- tryCatch({
      dbGetQuery(con, query)
    }, error = function(e) {
      cat("  !!!! WARNING: Could not process table '", table_name, "'. Error: ", e$message, "\n", sep="")
      return(NULL)
    })
    if (!is.null(top_hits) && nrow(top_hits) > 0) {
      top_hits$StudyID <- table_name
      all_summary_data[[table_name]] <- top_hits
    }
  }
  if (length(all_summary_data) > 0) {
    cat("\nCombining all summary data...\n")
    final_summary_data <- bind_rows(all_summary_data)
    new_db_path <- file.path(output_dir, "summary_data.sqlite")
    cat("Writing", nrow(final_summary_data), "total rows to", new_db_path, "\n\n")
    
    new_con <- dbConnect(RSQLite::SQLite(), dbname = new_db_path)
    # The data is written to a table named "summary_hits"
    dbWriteTable(new_con, name = "summary_hits", value = final_summary_data, overwrite = TRUE)
    dbDisconnect(new_con)
  } else {
    cat("\nNo summary data could be generated.\n\n")
  }
  
  cat("Summary database processing complete.\n")
}
create_summary_database()
