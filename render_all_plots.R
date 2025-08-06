rm(list = ls())
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(readr)
library(tibble)

# --- 2. CONFIGURATION ---
setwd(here::here())
SOURCE_DB_PATH <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_data_v2.sqlite"
OUTPUT_DIR <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_viewer/shiny_frontend/www"
CHR_MAP_PATH <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_viewer/shiny_frontend/chr_map_data.rds" 
LAMBDA_META_PATH <- "/sc/arion/projects/AsgariLab/abhijith/gwas_All_ID/saige_gwas/lambda_biome.tsv"


if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}



lambda_metadata <- read_tsv(LAMBDA_META_PATH)

# Complete list of all studies compiled from your screenshots
study_map <- tribble(
  ~phecode, ~TableName, ~TraitName,
  "ID_054.32", "acute_hepatitis_c", "Acute Hepatitis C",
  "ID_056.2", "anogenital_warts", "Anogenital warts",
  "ID_074", "aspergillosis", "Aspergillosis",
  "ID_092.8", "bacteremia", "Bacteremia",
  "ID_092", "bacteremia_sepsis_sirs", "Bacteremia, Sepsis, and SIRS",
  "ID_089.1", "bacterial_infections", "Bacterial infections",
  "ID_097.3", "beta_lactam_resistance", "Resistance to beta-lactam antibiotics",
  "ID_020", "borrelia", "Borrelia",
  "ID_070", "candidiasis", "Candidiasis",
  "ID_016", "chlamydia", "Chlamydia",
  "ID_016.1", "chlamydia_trachomatis", "Chlamydia trachomatis",
  "ID_054.31", "chronic_hepatitis_c", "Chronic Hepatitis C",
  "ID_015", "clostridium", "Clostridium",
  "ID_015.2", "clostridium_difficile", "Clostridium difficile",
  "ID_059", "corona", "Coronavirus",
  "ID_059.1", "cov2", "Sars-CoV-2",
  "ID_052.5", "cytomegalovirus", "Cytomegalovirus (CMV)",
  "ID_097", "drug_resistance", "Drug resistant microorganisms",
  "ID_050", "enterovirus", "Enterovirus",
  "ID_003", "escherichia_coli", "Escherichia coli",
  "ID_089.3", "fungal_infections", "Fungal infections",
  "ID_091", "gangrene", "Gangrene",
  "ID_084.7", "giardiasis", "Giardiasis",
  "ID_004.2", "group_a_streptococcus", "Group A Streptococcus",
  "ID_004.3", "group_b_streptococcus", "Group B Streptococcus",
  "ID_008", "H_pylori", "Helicobacter pylori",
  "ID_054.1", "hepatitis_a", "Hepatitis A",
  "ID_054.2", "hepatitis_b", "Hepatitis B",
  "ID_054.21", "hepatitis_b_with_delta", "Hepatitis B with delta",
  "ID_054.3", "hepatitis_c", "Hepatitis C",
  "ID_054", "hepatovirus", "Hepatovirus",
  "ID_052.1", "herpes_simplex", "Herpes simplex",
  "ID_052", "herpesvirus", "Herpesvirus",
  "ID_052.32", "herpes_zoster", "Herpes zoster",
  "ID_057.1", "hiv", "Human immunodeficiency virus",
  "ID_056", "hpv", "Human papillomavirus",
  "ID_089", "infections", "Infections",
  "ID_052.4", "infectious_mono", "Infectious mononucleosis",
  "ID_061", "influenza", "Influenza virus",
  "ID_020.1", "lyme_disease", "Lyme disease",
  "ID_055.1", "molluscum_cont", "Molluscum contagiosum",
  "ID_097.1", "mrsa", "Methicillin-resistant Staphylococcus aureus",
  "ID_005", "mycobacteria", "Mycobacteria",
  "ID_005.1", "mycobacterium_tuberculosis", "Mycobacterium tuberculosis",
  "ID_006", "neisseria", "Neisseria",
  "ID_006.2", "neisseria_gonorrhea", "Neisseria gonorrhea",
  "ID_069", "other_viral", "Other specified viral infections",
  "ID_084", "parasites", "Parasites",
  "ID_086.1", "pediculosis", "Pediculosis",
  "ID_086", "pediculosis_acarisis_other", "Pediculosis, acariasis and other infestations",
  "ID_056.1", "plantar_wart", "Plantar wart",
  "ID_058", "pneumo", "Pneumoviridae",
  "ID_076", "pneumocystosis", "Pneumocystosis",
  "ID_055", "poxvirus", "Poxvirus",
  "ID_057", "retrovirus", "Retrovirus",
  "ID_086.2", "scabies", "Scabies",
  "ID_092.2", "sepsis", "Sepsis",
  "ID_088", "std", "Sexually transmitted disease",
  "ID_002.1", "staph_aureus", "Staphylococcus aureus",
  "ID_002", "staphylococcus", "Staphylococcus",
  "ID_004", "streptococcus", "Streptococcus",
  "ID_092.1", "systemic_inflammatory_response", "Systemic inflammatory response syndrome",
  "ID_084.5", "toxoplasmosis", "Toxoplasmosis",
  "ID_019", "treponema", "Treponema",
  "ID_019.1", "treponema_pallidum", "Treponema pallidum (syphilis)",
  "ID_084.4", "trichomoniasis", "Trichomoniasis",
  "ID_052.31", "varicella_chickenpox", "Varicella (chickenpox)",
  "ID_052.3", "varicella_zoster", "Varicella zoster virus",
  "ID_089.2", "viral_infections", "Viral infections"
)

 

# --- 4. MAIN PLOTTING LOGIC ---
con <- dbConnect(RSQLite::SQLite(), dbname = SOURCE_DB_PATH)

# Clean all names in the map first to remove any hidden whitespace
study_map <- study_map %>%
  mutate(TableName = trimws(TableName))

for (i in 1:nrow(study_map)) {
  # This section now uses the correct variable names
  current_phecode <- study_map$phecode[i]
  current_table_name <- study_map$TableName[i]
  current_trait_name <- study_map$TraitName[i]
  
  cat("====================================================\n")
  cat(sprintf("(%d/%d) Processing: %s\n", i, nrow(study_map), current_trait_name))
  manhattan_path <- file.path(OUTPUT_DIR, paste0(current_table_name, ".png"))
  qq_path <- file.path(OUTPUT_DIR, paste0("qq_", current_table_name, ".png"))
  
  # --- Manhattan Plot Rendering (if missing) ---
  if (file.exists(manhattan_path)) {
    cat("  -> Manhattan plot already exists. Skipping.\n")
  } else {
    tryCatch({
      cat("  -> Loading data for Manhattan plot...\n")
      # This line now correctly uses 'current_table_name'
      gwas_data <- dbGetQuery(con, paste0("SELECT Chromosome, Position, P_value, LOG10P FROM \"", current_table_name, "\""))
      
      cat("calc coordinates")
      gwas_data$Chromosome <- factor(gwas_data$Chromosome, levels = c(as.character(1:22), "X", "Y"))
      gwas_data <- gwas_data %>% arrange(Chromosome, Position)
      
      map_from_data <- gwas_data %>% 
        group_by(Chromosome) %>% 
        summarise(chr_len = max(Position)) %>%
        mutate(total = lag(cumsum(as.numeric(chr_len)), default=0))
      
      plot_data <- gwas_data %>%
        left_join(map_from_data, by = "Chromosome") %>%
        mutate(BP_CUM = Position + total)
      
      axis_set <- map_from_data %>%
        mutate(center = total + (chr_len / 2))
      
      
      
      cat("  -> Generating high-quality Manhattan plot...\n")
      manhattan_plot <- ggplot(plot_data, aes(x = BP_CUM, y = LOG10P, color = as.factor(as.numeric(as.factor(Chromosome)) %% 2))) +
        geom_point(alpha = 0.75, size = 0.5) +
        scale_color_manual(values = c("0" = "#276FBF", "1" = "#183059")) +
        geom_hline(yintercept = -log10(5e-8), color = "red", linetype = "dashed") +
        geom_hline(yintercept = -log10(5e-5), color = "red", linetype = "dashed") +
        labs(title = paste("Manhattan Plot:", current_trait_name), x = "Chromosome", y = expression(-log[10](P))) +
        scale_x_continuous(label = axis_set$Chromosome, breaks = axis_set$center, expand = c(0.01, 0.01)) +
        theme_minimal() +
        theme(
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text.x = element_text(angle = 0, size = 10)
        )
      ggsave(manhattan_path, manhattan_plot, width = 12, height = 6, dpi = 200, bg = "white")
      cat("  -> Manhattan plot saved.\n")
    }, error = function(e) {
      cat("  !!!! FAILED to generate Manhattan plot. Error:", e$message, "\n")
    })
  }
  
  # --- QQ Plot Rendering (if missing) ---
  if (file.exists(qq_path)) {
    cat("  -> QQ plot already exists. Skipping.\n")
  } else {
    tryCatch({
      cat("  -> Loading data for QQ plot...\n")
      # This line now correctly uses 'current_table_name'
      gwas_data <- dbGetQuery(con, paste0("SELECT P_value FROM \"", current_table_name, "\""))
      
      plot_data_qq <- gwas_data %>%
        arrange(P_value) %>%
        mutate(observed = -log10(P_value), expected = -log10(ppoints(n())))
      
      lambda_gc <- lambda_metadata %>%
        filter(phecode == current_phecode, group == "meta") %>%
        pull(value)
      
      if (length(lambda_gc) == 0) { lambda_gc <- NA }
      qq_plot <- ggplot(plot_data_qq, aes(x = expected, y = observed)) +
        geom_point(alpha = 0.5, size = 1.5, color = "#183059") +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        labs(title = paste("QQ Plot:", current_trait_name), x = expression(Expected~-log[10](P)), y = expression(Observed~-log[10](P))) +
        {
          if (!is.na(lambda_gc)) {
            annotate("text", x = 1, y = max(plot_data_qq$observed, na.rm = TRUE) * 0.9,
                     label = bquote(lambda[GC] == .(round(lambda_gc, 3))), size = 5)
          }
        } +
        theme_bw()
      
      ggsave(qq_path, qq_plot, width = 6, height = 6, dpi = 150, bg = "white")
      cat("  -> QQ plot saved.\n")
    }, error = function(e) {
      cat("  !!!! FAILED to generate QQ plot. Error:", e$message, "\n")
    })
  }
}

dbDisconnect(con)
cat("====================================================\n")
cat("All plot rendering tasks are complete")
