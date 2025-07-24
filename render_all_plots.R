library(DBI)
library(RSQLite)
library(ggplot2)
library(dplyr)
library(ragg)
library(tidyr)
library(data.table)

dataset_catalog <- list(
  list(id = "staph_aureus", folder_name = "gwas_ID_002.1_Staphylococcus_aureus_minCodeCount1"),
  list(id = "staphylococcus", folder_name = "gwas_ID_002_Staphylococcus_minCodeCount1"),
  list(id = "escherichia_coli", folder_name = "gwas_ID_003_Escherichia_coli_minCodeCount1"),
  list(id = "group_a_streptococcus", folder_name = "gwas_ID_004.2_Group_A_Streptococcus_minCodeCount1"),
  list(id = "group_b_streptococcus", folder_name = "gwas_ID_004.3_Group_B_Streptococcus_minCodeCount1"),
  list(id = "streptococcus", folder_name = "gwas_ID_004_Streptococcus_minCodeCount1"),
  list(id = "mycobacterium_tuberculosis", folder_name = "gwas_ID_005.1_Mycobacterium_tuberculosis_minCodeCount1"),
  list(id = "mycobacteria", folder_name = "gwas_ID_005_Mycobacteria_minCodeCount1"),
  list(id = "neisseria_gonorrhea", folder_name = "gwas_ID_006.2_Neisseria_gonorrhea_minCodeCount1"),
  list(id = "neisseria", folder_name = "gwas_ID_006_Neisseria_minCodeCount1"),
  list(id = "H_pylori", folder_name = "gwas_ID_008_Helicobacter_[H._pylori]_minCodeCount1"),
  list(id = "clostridium_difficile", folder_name = "gwas_ID_015.2_Clostridium_difficile_minCodeCount1"),
  list(id = "clostridium", folder_name = "gwas_ID_015_Clostridium_minCodeCount1"),
  list(id = "chlamydia_trachomatis", folder_name = "gwas_ID_016.1_Chlamydia_trachomatis_minCodeCount1"),
  list(id = "chlamydia", folder_name = "gwas_ID_016_Chlamydia_minCodeCount1"),
  list(id = "treponema_pallidum", folder_name = "gwas_ID_019.1_Treponema_pallidum_[syphilis]_minCodeCount1"),
  list(id = "treponema", folder_name = "gwas_ID_019_Treponema_minCodeCount1"),
  list(id = "lyme_disease", folder_name = "gwas_ID_020.1_Lyme_disease_minCodeCount1"),
  list(id = "borrelia", folder_name = "gwas_ID_020_Borrelia_minCodeCount1"),
  list(id = "enterovirus", folder_name = "gwas_ID_050_Enterovirus_minCodeCount1"),
  list(id = "herpes_simplex", folder_name = "gwas_ID_052.1_Herpes_simplex_minCodeCount1"),
  list(id = "varicella_chickenpox", folder_name = "gwas_ID_052.31_Varicella_[chickenpox]_minCodeCount1"),
  list(id = "herpes_zoster", folder_name = "gwas_ID_052.32_Herpes_zoster_minCodeCount1"),
  list(id = "varicella_zoster", folder_name = "gwas_ID_052.3_Varicella_zoster_virus_minCodeCount1"),
  list(id = "infectious_mono", folder_name = "gwas_ID_052.4_Infectious_mononucleosis_minCodeCount1"),
  list(id = "cytomegalovirus", folder_name = "gwas_ID_052.5_Cytomegalovirus_[CMV]_minCodeCount1"),
  list(id = "herpesvirus", folder_name = "gwas_ID_052_Herpesvirus_minCodeCount1"),
  list(id = "hepatitis_a", folder_name = "gwas_ID_054.1_Hepatitis_A_minCodeCount1"),
  list(id = "hepatitis_b_with_delta", folder_name = "gwas_ID_054.21_Hepatitis_B_with_delta_minCodeCount1"),
  list(id = "hepatitis_b", folder_name = "gwas_ID_054.2_Hepatitis_B_minCodeCount1"),
  list(id = "chronic_hepatitis_c", folder_name = "gwas_ID_054.31_Chronic_hepatitis_C_minCodeCount1"),
  list(id = "acute_hepatitis_c", folder_name = "gwas_ID_054.32_Acute_hepatitis_C_minCodeCount1"),
  list(id = "hepatitis_c", folder_name = "gwas_ID_054.3_Hepatitis_C_minCodeCount1"),
  list(id = "hepatovirus", folder_name = "gwas_ID_054_Hepatovirus_minCodeCount1"),
  list(id = "molluscum_cont", folder_name = "gwas_ID_055.1_Molluscum_contagiosum_minCodeCount1"),
  list(id = "poxvirus", folder_name = "gwas_ID_055_Poxvirus_minCodeCount1"),
  list(id = "plantar_wart", folder_name = "gwas_ID_056.1_Plantar_wart_minCodeCount1"),
  list(id = "anogenital_warts", folder_name = "gwas_ID_056.2_Anogenital_(venereal)_warts_minCodeCount1"),
  list(id = "hpv", folder_name = "gwas_ID_056_Human_papillomavirus_minCodeCount1"),
  list(id = "hiv", folder_name = "gwas_ID_057.1_Human_immunodeficiency_virus_minCodeCount1"),
  list(id = "retrovirus", folder_name = "gwas_ID_057_Retrovirus_minCodeCount1"),
  list(id = "pneumo", folder_name = "gwas_ID_058_Pneumoviridae_minCodeCount1"),
  list(id = "cov2", folder_name = "gwas_ID_059.1_Sars-CoV-2*_minCodeCount1"),
  list(id = "corona", folder_name = "gwas_ID_059_Coronavirus_minCodeCount1"),
  list(id = "influenza", folder_name = "gwas_ID_061_Influenza_virus_minCodeCount1"),
  list(id = "other_viral", folder_name = "gwas_ID_069_Other_specified_viral_infections_minCodeCount1"),
  list(id = "candidiasis", folder_name = "gwas_ID_070_Candidiasis_minCodeCount1"),
  list(id = "aspergillosis", folder_name = "gwas_ID_074_Aspergillosis_minCodeCount1"),
  list(id = "pneumocystosis", folder_name = "gwas_ID_076_Pneumocystosis_minCodeCount1"),
  list(id = "trichomoniasis", folder_name = "gwas_ID_084.4_Trichomoniasis_minCodeCount1"),
  list(id = "toxoplasmosis", folder_name = "gwas_ID_084.5_Toxoplasmosis_minCodeCount1"),
  list(id = "giardiasis", folder_name = "gwas_ID_084.7_Giardiasis_minCodeCount1"),
  list(id = "parasites", folder_name = "gwas_ID_084_Parasites_minCodeCount1"),
  list(id = "pediculosis", folder_name = "gwas_ID_086.1_Pediculosis_minCodeCount1"),
  list(id = "scabies", folder_name = "gwas_ID_086.2_Scabies_minCodeCount1"),
  list(id = "pediculosis_acarisis_other", folder_name = "gwas_ID_086_Pediculosis,_acariasis_and_other_infestations_minCodeCount1"),
  list(id = "std", folder_name = "gwas_ID_088_Sexually_transmitted_disease_minCodeCount1"),
  list(id = "bacterial_infections", folder_name = "gwas_ID_089.1_Bacterial_infections_minCodeCount1"),
  list(id = "viral_infections", folder_name = "gwas_ID_089.2_Viral_infections_minCodeCount1"),
  list(id = "fungal_infections", folder_name = "gwas_ID_089.3_Fungal_infections_minCodeCount1"),
  list(id = "infections", folder_name = "gwas_ID_089_Infections_minCodeCount1"),
  list(id = "gangrene", folder_name = "gwas_ID_091_Gangrene_minCodeCount1"),
  list(id = "systemic_inflammatory_response", folder_name = "gwas_ID_092.1_Systemic_inflammatory_response_syndrome_minCodeCount1"),
  list(id = "sepsis", folder_name = "gwas_ID_092.2_Sepsis_minCodeCount1"),
  list(id = "bacteremia", folder_name = "gwas_ID_092.8_Bacteremia_minCodeCount1"),
  list(id = "bacteremia_sepsis_sirs", folder_name = "gwas_ID_092_Bacteremia,_Sepsis,_and_SIRS_minCodeCount1"),
  list(id = "mrsa", folder_name = "gwas_ID_097.1_Methicillin_resistant_Staphylococcus_aureus_minCodeCount1"),
  list(id = "beta_lactam_resistance", folder_name = "gwas_ID_097.3_Resistance_to_beta_lactam_antibiotics_minCodeCount1"),
  list(id = "drug_resistance", folder_name = "gwas_ID_097_Drug_resistant_microorganisms_minCodeCount1")
)


lambda_data <- data.table::fread("/sc/arion/projects/AsgariLab/abhijith/gwas_All_ID/saige_gwas/lambda_biome.tsv")
lambda_data$phecode <- trimws(lambda_data$phecode)

source_db_path <- "/sc/arion/projects/AsgariLab/RechumaHafter/gwas_data_v2.sqlite"
project_dir <- file.path("/sc/arion/projects/AsgariLab/RechumaHafter/gwas_viewer/shiny_frontend")
output_dir <- file.path(project_dir, "www")

if (!dir.exists(output_dir)) { 
  dir.create(output_dir, recursive = TRUE) 
  }

con <- dbConnect(RSQLite::SQLite(), source_db_path)

for (dataset in dataset_catalog) {
  table_name <- dataset$id
  folder_name <- dataset$folder_name
  
  # --- Manhattan Plot ---
  manhattan_file <- file.path(output_dir, paste0(table_name, ".png"))
  if (!file.exists(manhattan_file)) {
    
    message("Rendering Manhattan plot for: ", table_name)
   tryCatch({
    gwas_data <- dbGetQuery(con, sprintf("SELECT Chromosome, Position, P_value FROM `%s`", table_name))
   
      gwas_data_cleaned <- gwas_data %>%
        mutate(P_value = as.numeric(as.character(P_value)), 
               Chromosome = as.character(Chromosome)) %>%
        filter(!is.na(P_value), P_value > 0) %>%
        mutate(Chr_numeric = case_when(Chromosome == "X" ~ 23, Chromosome == "Y" ~24,
       TRUE ~ as.numeric(Chromosome)
           )) %>%
        filter(!is.na(Chromosome))
        
      if (nrow(gwas_data_cleaned) < 2000) {
        message("Dataset is too small, using all points.")
        plot_data_final <- gwas_data_cleaned
        
      } else {
        message("Dataset is large, applying thinning...")
        top_hits <- gwas_data_cleaned %>% 
          filter(P_value < 1e-5) 
       data_to_thin <- gwas_data_cleaned %>% 
         filter(P_value >= 1e-5)
       thinned_background <- data_to_thin %>%
          mutate(
            log10p = -log10(P_value),
            x_bin = cut(Position, breaks = 1500, labels = FALSE),
            y_bin = cut(log10p, breaks = 500, labels = FALSE)
          ) %>%
          group_by(Chromosome, x_bin, y_bin) %>%
          slice_sample(n = 5) %>%
          ungroup()
       
        plot_data_final <- bind_rows(top_hits, thinned_background) %>%
          distinct(Chromosome, Position, .keep_all = TRUE)
      }
      message("Final plot will contain ", nrow(plot_data_final), " points.")
      
      data_for_plot <- plot_data_final %>%
        group_by(Chr_numeric) %>%
        summarise(chr_len = max(Position), .groups = 'drop') %>%
        arrange(Chr_numeric) %>%
        mutate(total = cumsum(as.numeric(chr_len)) - chr_len) %>%
        select(-chr_len) %>%
        left_join(plot_data_final, ., by = "Chr_numeric") %>%
        mutate(BP_cum = Position + total)
      
      axis_df <- data_for_plot %>% 
        group_by(Chromosome) %>% 
        summarize(center = (max(BP_cum) + min(BP_cum)) / 2, .groups = 'drop')
    
        
     p <- ggplot(data_for_plot, aes(x = BP_cum, y = -log10(P_value))) + 
       geom_point(aes(color = as.factor(Chr_numeric %% 2 )), alpha = 0.7, size = 1) + 
       scale_color_manual(values = c("0" = "dodgerblue4", "1" = "skyblue3")) +
       scale_x_continuous(label = axis_df$Chromosome, breaks = axis_df$center) +
       geom_hline(yintercept = -log10(5e-8), color = "red", linetype = "dashed") +
       
       geom_hline(yintercept = -log10(5e-5), color = "red", linetype = "dashed") +
       labs(x = "Chromosome", title = paste("Manhattan Plot:", table_name), y= "-log10(P_value)") +
       theme_minimal(base_size = 14) +
       
       theme(legend.position = "none", panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
     
     ggsave(manhattan_file, plot = p, width = 16, height = 7, dpi = 150, device = ragg::agg_png)
     
     message("--- Successfully saved Manhattan plot. ---")
     
      },
      error = function(e) { message("--- FAILED Manhattan plot: ", e$message) })
  } else { message("plot eists, skipping", table_name) 
    }
        
    
  
  
       
       
       
  qq_file <- file.path(output_dir, paste0("qq_", table_name, ".png"))
  if (!file.exists(qq_file)) {
    message("--- Rendering QQ plot for: ", table_name, " ---")
    tryCatch({
    
      if (!exists("gwas_data_cleaned")) {
        if (!exists("gwas_data")) {
        gwas_data <- dbGetQuery(con, sprintf("SELECT P_value FROM `%s`", table_name))}
        gwas_data_cleaned <- gwas_data %>%
          mutate(P_value = as.numeric(as.character(P_value))) %>%
          filter(!is.na(P_value))
      }
      
      qq_data <- gwas_data_cleaned %>% 
      arrange(P_value) %>% 
        mutate(observed = -log10(P_value), expected = -log10(ppoints(n())))
      
      id_code <- trimws(sub("gwas_(ID_[0-9\\.]+)_.*", "\\1", folder_name))
      
      
     lambda_value <- "N/A"
       meta_info <- lambda_data[phecode == id_code & group == 'meta']
     if (nrow(meta_info) > 0) {
       lambda_value <-round(meta_info$value[1], 3)
     } else { 
       other_info <- lambda_data[phecode == id_code]
       if(nrow(other_info) > 0){
         lambda_value <- round(other_info$value[1], 3)
       }
     }
      
      lambda_text <- paste("Lambda GC: ", lambda_value)
      
      
       qq_p <- ggplot(qq_data, aes(x = expected, y = observed)) + 
       geom_point(alpha = 0.5) + 
         geom_abline(color = "red") +
        annotate("text", x = 0.5, y = max(qq_data$observed, na.rm = TRUE), label = lambda_text, hjust = 0, size = 5) +
         labs(title = paste("QQ Plot:", table_name)) +
         theme_minimal(base_size = 14)
       
      ggsave(qq_file, plot = qq_p, width = 16, height = 9, dpi = 150, device = ragg::agg_png)
      message("--- Successfully saved QQ plot. ---")
    }, error = function(e) { message("--- FAILED QQ plot: ", e$message) })
  } else {
    message("QQ plot for ", table_name, " already exists. Skipping.")
  }
  if (exists("gwas_data")) { rm(gwas_data); gc() }

}


dbDisconnect(con)
message("\n--- Plot Rendering Complete ---")
