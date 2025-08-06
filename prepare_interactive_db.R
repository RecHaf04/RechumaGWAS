library(DBI)
library(RSQLite)
library(dplyr)
library(jsonlite)
library(httr)

API_URL_GROUP_1 <- "https://rstudio-connect.hpc.mssm.edu/content/ced643da-26dd-4b88-9f39-aec6c8c4c000/"
API_URL_GROUP_2 <- "https://rstudio-connect.hpc.mssm.edu/content/4f6a0804-9030-4ed0-9fd2-186379b35a10/"
API_URL_GROUP_3 <- "https://rstudio-connect.hpc.mssm.edu/content/30618e94-0ce0-466b-9ce0-ff2568f40399/"

ALL_API_URLS <- c(API_URL_GROUP_1, API_URL_GROUP_2, API_URL_GROUP_3)


  study_metadata <- tibble::tribble(
    ~StudyID, ~PheCode, ~Ancestry, ~Cases, ~Controls, ~TraitName,
    "staph_aureus", "ID_002.1", "European", 1204, 50000, "Staphylococcus aureus",
    "staphylococcus", "ID_002", "European", 850, 45000, "Staphylococcus",
    "escherichia_coli", "ID_003", "Mixed", 2500, 98000, "Escherichia coli",
    "streptococcus", "ID_004", NA, NA, NA, "Streptococcus",
    "group_a_streptococcus", "ID_004.2", NA, NA, NA, "Group A Streptococcus",
    "group_b_streptococcus", "ID_004.3", NA, NA, NA, "Group B Streptococcus",
    "mycobacteria", "ID_005", NA, NA, NA, "Mycobacteria",
    "mycobacterium_tuberculosis", "ID_005.1", NA, NA, NA, "Mycobacterium tuberculosis",
    "neisseria", "ID_006", NA, NA, NA, "Neisseria",
    "neisseria_gonorrhea", "ID_006.2", NA, NA, NA, "Neisseria gonorrhea",
    "H_pylori", "ID_008", NA, NA, NA, "Helicobacter pylori",
    "clostridium", "ID_015", NA, NA, NA, "Clostridium",
    "clostridium_difficile", "ID_015.2", NA, NA, NA, "Clostridium difficile",
    "chlamydia", "ID_016", NA, NA, NA, "Chlamydia",
    "chlamydia_trachomatis", "ID_016.1", NA, NA, NA, "Chlamydia trachomatis",
    "treponema", "ID_019", NA, NA, NA, "Treponema",
    "treponema_pallidum", "ID_019.1", NA, NA, NA, "Treponema pallidum (syphilis)",
    "borrelia", "ID_020", NA, NA, NA, "Borrelia",
    "lyme_disease", "ID_020.1", NA, NA, NA, "Lyme disease",
    "herpesvirus", "ID_052", NA, NA, NA, "Herpesvirus",
    "herpes_simplex", "ID_052.1", NA, NA, NA, "Herpes simplex",
    "varicella_zoster", "ID_052.3", NA, NA, NA, "Varicella zoster virus",
    "infectious_mono", "ID_052.4", NA, NA, NA, "Infectious mononucleosis",
    "cytomegalovirus", "ID_052.5", NA, NA, NA, "Cytomegalovirus (CMV)",
    "varicella_chickenpox", "ID_052.31", NA, NA, NA, "Varicella (chickenpox)",
    "herpes_zoster", "ID_052.32", NA, NA, NA, "Herpes zoster",
    "hepatovirus", "ID_054", NA, NA, NA, "Hepatovirus",
    "hepatitis_a", "ID_054.1", NA, NA, NA, "Hepatitis A",
    "hepatitis_b", "ID_054.2", NA, NA, NA, "Hepatitis B",
    "hepatitis_c", "ID_054.3", NA, NA, NA, "Hepatitis C",
    "hepatitis_b_with_delta", "ID_054.21", NA, NA, NA, "Hepatitis B with delta",
    "chronic_hepatitis_c", "ID_054.31", NA, NA, NA, "Chronic Hepatitis C",
    "acute_hepatitis_c", "ID_054.32", NA, NA, NA, "Acute Hepatitis C",
    "poxvirus", "ID_055", NA, NA, NA, "Poxvirus",
    "molluscum_cont", "ID_055.1", NA, NA, NA, "Molluscum contagiosum",
    "hpv", "ID_056", NA, NA, NA, "Human papillomavirus",
    "plantar_wart", "ID_056.1", NA, NA, NA, "Plantar wart",
    "anogenital_warts", "ID_056.2", NA, NA, NA, "Anogenital warts",
    "retrovirus", "ID_057", NA, NA, NA, "Retrovirus",
    "hiv", "ID_057.1", NA, NA, NA, "Human immunodeficiency virus",
    "pneumo", "ID_058", NA, NA, NA, "Pneumoviridae",
    "corona", "ID_059", NA, NA, NA, "Coronavirus",
    "cov2", "ID_059.1", NA, NA, NA, "Sars-CoV-2",
    "influenza", "ID_061", NA, NA, NA, "Influenza virus",
    "other_viral", "ID_069", NA, NA, NA, "Other specified viral infections",
    "candidiasis", "ID_070", NA, NA, NA, "Candidiasis",
    "aspergillosis", "ID_074", NA, NA, NA, "Aspergillosis",
    "pneumocystosis", "ID_076", NA, NA, NA, "Pneumocystosis",
    "parasites", "ID_084", NA, NA, NA, "Parasites",
    "trichomoniasis", "ID_084.4", NA, NA, NA, "Trichomoniasis",
    "toxoplasmosis", "ID_084.5", NA, NA, NA, "Toxoplasmosis",
    "giardiasis", "ID_084.7", NA, NA, NA, "Giardiasis",
    "pediculosis_acarisis_other", "ID_086.1", NA, NA, NA, "Pediculosis, Acarisis, Other Infections",
    "scabies", "ID_086.2", NA, NA, NA, "Scabies",
    "std", "ID_088", NA, NA, NA, "Sexually transmitted disease",
    "infections", "ID_089", NA, NA, NA, "Infections",
    "bacterial_infections", "ID_089.1", NA, NA, NA, "Bacterial infections",
    "viral_infections", "ID_089.2", NA, NA, NA, "Viral infections",
    "fungal_infections", "ID_089.3", NA, NA, NA, "Fungal infections",
    "gangrene", "ID_091", NA, NA, NA, "Gangrene",
    "bacteremia_sepsis_sirs", "ID_092", NA, NA, NA, "Bacteremia, Sepsis, and SIRS",
    "systemic_inflammatory_response", "ID_092.1", NA, NA, NA, "Systemic inflammatory response syndrome",
    "sepsis", "ID_092.2", NA, NA, NA, "Sepsis",
    "bacteremia", "ID_092.8", NA, NA, NA, "Bacteremia",
    "drug_resistance", "ID_097", NA, NA, NA, "Drug resistant microorganisms",
    "mrsa", "ID_097.1", NA, NA, NA, "Methicillin-resistant Staphylococcus aureus",
    "beta_lactam_resistance", "ID_097.3", NA, NA, NA, "Resistance to beta-lactam antibiotics"
  )
all_study_ids <- study_metadata$StudyID

# --- Main Logic ---
destination_db_path <- "interactive_hits.sqlite"
if (file.exists(destination_db_path)) {
  message("An existing database file was found and will be replaced.")
  file.remove(destination_db_path)
}
con_dest <- dbConnect(RSQLite::SQLite(), destination_db_path)

message(sprintf("Starting database creation for %d studies...", length(all_study_ids)))

for (i in seq_along(all_study_ids)) {
  study <- all_study_ids[i]
  message(sprintf("\n(%d/%d) Processing study: %s", i, length(all_study_ids), study))
  
  full_data <- NULL
  for (base_url in ALL_API_URLS) {
    message("--- Trying to download from API...")
    response <- POST(url = paste0(base_url, "summary"), body = list(study_id = study), encode = "json")
    if (http_status(response)$category == "Success") {
      content <- fromJSON(content(response, "text", "UTF-8"))
      if (!is.data.frame(content) || "Message" %in% names(content)) next
      full_data <- content
      message("--- Download successful.")
      break
    }
  }
  
  if (!is.null(full_data) && nrow(full_data) > 0) {
    significant_data <- full_data %>% filter(P_value < 1e-4)
    if (nrow(significant_data) > 0) {
      dbWriteTable(con_dest, study, significant_data, append = TRUE)
      message("--- Found and wrote ", nrow(significant_data), " significant hits to the database.")
    } else {
      message("--- No significant hits (P < 1e-5) found for this study.")
    }
  } else {
    message("--- FAILED to download data for this study or data was empty.")
  }
}

dbDisconnect(con_dest)
message("\n--- Interactive database creation complete! ✅ ---")
