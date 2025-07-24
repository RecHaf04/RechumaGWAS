#app.R - Final Full Version for Multi-API Architecture

# --- 1. LOAD PACKAGES ---
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(htmlwidgets)

# --- 2. DATA & CONFIG ---
# This list populates your dropdown menu.
dataset_catalog <- list(
  list(id = "staph_aureus", trait = "Staph Aureus"),
  list(id = "staphylococcus", trait = "Staphylococcus"),
  list(id = "escherichia_coli", trait = "Escherichia Coli"),
  list(id = "group_a_streptococcus", trait = "Group A Streptococcus"),
  list(id = "group_b_streptococcus", trait = "Group B Streptococcus"),
  list(id = "streptococcus", trait = "Streptococcus"),
  list(id = "mycobacterium_tuberculosis", trait = "Mycobacterium Tuberculosis"),
  list(id = "mycobacteria", trait = "Mycobacteria"),
  list(id = "neisseria_gonorrhea", trait = "Neisseria Gonorrhea"),
  list(id = "neisseria", trait = "Neisseria"),
  list(id = "H_pylori", trait = "H. pylori"),
  list(id = "clostridium_difficile", trait = "Clostridium Difficile"),
  list(id = "clostridium", trait = "Clostridium"),
  list(id = "chlamydia_trachomatis", trait = "Chlamydia Trachomatis"),
  list(id = "chlamydia", trait = "Chlamydia"),
  list(id = "treponema_pallidum", trait = "Treponema Pallidum (Syphilis)"),
  list(id = "treponema", trait = "Treponema"),
  list(id = "lyme_disease", trait = "Lyme Disease"),
  list(id = "borrelia", trait = "Borrelia"),
  list(id = "enterovirus", trait = "Enterovirus"),
  list(id = "herpes_simplex", trait = "Herpes Simplex"),
  list(id = "varicella_chickenpox", trait = "Varicella (Chickenpox)"),
  list(id = "herpes_zoster", trait = "Herpes Zoster"),
  list(id = "varicella_zoster", trait = "Varicella Zoster Virus"),
  list(id = "infectious_mono", trait = "Infectious Mononucleosis"),
  list(id = "cytomegalovirus", trait = "Cytomegalovirus (CMV)"),
  list(id = "herpesvirus", trait = "Herpesvirus"),
  list(id = "hepatitis_a", trait = "Hepatitis A"),
  list(id = "hepatitis_b_with_delta", trait = "Hepatitis B with Delta"),
  list(id = "hepatitis_b", trait = "Hepatitis B"),
  list(id = "chronic_hepatitis_c", trait = "Chronic Hepatitis C"),
  list(id = "acute_hepatitis_c", trait = "Acute Hepatitis C"),
  list(id = "hepatitis_c", trait = "Hepatitis C"),
  list(id = "hepatovirus", trait = "Hepatovirus"),
  list(id = "molluscum_cont", trait = "Molluscum Contagiosum"),
  list(id = "poxvirus", trait = "Poxvirus"),
  list(id = "plantar_wart", trait = "Plantar Wart"),
  list(id = "anogenital_warts", trait = "Anogenital Warts"),
  list(id = "hpv", trait = "Human Papillomavirus"),
  list(id = "hiv", trait = "Human Immunodeficiency Virus"),
  list(id = "retrovirus", trait = "Retrovirus"),
  list(id = "pneumo", trait = "Pneumoviridae"),
  list(id = "cov2", trait = "Sars-CoV-2"),
  list(id = "corona", trait = "Coronavirus"),
  list(id = "influenza", trait = "Influenza Virus"),
  list(id = "other_viral", trait = "Other Specified Viral Infections"),
  list(id = "candidiasis", trait = "Candidiasis"),
  list(id = "aspergillosis", trait = "Aspergillosis"),
  list(id = "pneumocystosis", trait = "Pneumocystosis"),
  list(id = "trichomoniasis", trait = "Trichomoniasis"),
  list(id = "toxoplasmosis", trait = "Toxoplasmosis"),
  list(id = "giardiasis", trait = "Giardiasis"),
  list(id = "parasites", trait = "Parasites"),
  list(id = "scabies", trait = "Scabies"),
  list(id = "pediculosis_acarisis_other", trait = "Pediculosis, Acariasis, Other"),
  list(id = "std", trait = "Sexually Transmitted Disease"),
  list(id = "bacterial_infections", trait = "Bacterial Infections"),
  list(id = "viral_infections", trait = "Viral Infections"),
  list(id = "fungal_infections", trait = "Fungal Infections"),
  list(id = "infections", trait = "Infections"),
  list(id = "gangrene", trait = "Gangrene"),
  list(id = "systemic_inflammatory_response", trait = "Systemic Inflammatory Response"),
  list(id = "sepsis", trait = "Sepsis"),
  list(id = "bacteremia", trait = "Bacteremia"),
  list(id = "bacteremia_sepsis_sirs", trait = "Bacteremia, Sepsis, and SIRS"),
  list(id = "mrsa", trait = "Methicillin-resistant Staphylococcus aureus"),
  list(id = "beta_lactam_resistance", trait = "Resistance to Beta-lactam Antibiotics"),
  list(id = "drug_resistance", trait = "Drug Resistant Microorganisms")
)
# This file needs to be in the same directory as app.R
chr_map <- readRDS("chr_map.rds")

# --- 3. UI ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "GWAS Viewer",
  sidebar = sidebar(
    selectInput("study_selector", h4("Select a Study"),
                choices = setNames(sapply(dataset_catalog, `[[`, "id"), sapply(dataset_catalog, `[[`, "trait"))),
    hr(),
    h4("Cross-Phenotype Search"),
    textInput("chr_search", "Chromosome:", placeholder = "e.g., 1 or X"),
    numericInput("pos_search", "Position (HG38):", value = NULL, min = 0),
    actionButton("search_snp_button", "Search Location", class = "btn-success w-100"),
  ),
  navset_card_tab(
    id = "main_tabs",
    nav_panel("Manhattan Plot", 
              withSpinner(imageOutput("manhattan_plot", height = "550px", click = "manhattan_click")),
              withSpinner(DT::dataTableOutput("peak_info_table"))
    ),
    nav_panel("QQ Plot", 
              withSpinner(imageOutput("qq_plot", height = "280px"))
    ),
              
       nav_panel("Summary Data", card(
      card_header("Top Significant Hits"),
      withSpinner(DT::dataTableOutput("summary_table"))
    )),
    nav_panel("Cross-Phenotype Search", card(
      card_header("Results for Searched SNP"), withSpinner(DT::dataTableOutput("cross_pheno_table")),
      downloadButton("download_cross_pheno", "Download Results", class = "btn-success mt-3")
    )),
    nav_panel("Citations & Data",
              h4("Data Attribution"),
              p("If you use data or images from this tool, please cite..."))
  )
)
# --- 4. SERVER ---
server <- function(input, output, session) {
  
  # IMPORTANT: Fill these in with your deployed API URLs
  # Make sure the URLs include the trailing slash "/"
  API_URL_GROUP_1 <- "https://rstudio-connect.hpc.mssm.edu/content/ced643da-26dd-4b88-9f39-aec6c8c4c000/"
  API_URL_GROUP_2 <- "https://rstudio-connect.hpc.mssm.edu/content/4f6a0804-9030-4ed0-9fd2-186379b35a10/"
  API_URL_GROUP_3 <- "https://rstudio-connect.hpc.mssm.edu/content/30618e94-0ce0-466b-9ce0-ff2568f40399/"
  
  ALL_API_URLS <- c(API_URL_GROUP_1, API_URL_GROUP_2, API_URL_GROUP_3)
  
  # --- API Calling Helper Function ---
  # This intelligent function tries each API until it finds the right one
  call_api <- function(endpoint, body_params) {
    for (base_url in ALL_API_URLS) {
      api_url <- paste0(base_url, endpoint)
      response <- tryCatch({
        POST(url = api_url, body = body_params, encode = "json", timeout(30))
      }, error = function(e) {
        # If one API fails to connect, just try the next one
        return(NULL)
      })
      
      if (!is.null(response) && http_status(response)$category == "Success") {
        content <- fromJSON(content(response, "text", encoding = "UTF-8"))
        
        # Check if this API told us the data is elsewhere
        if (is.data.frame(content) && "Message" %in% names(content) &&
            (content$Message[1] == "CHROMOSOME_NOT_IN_THIS_API" || content$Message[1] == "SUMMARY_DATA_NOT_IN_THIS_API")) {
          next # Try the next API URL
        }
        return(content) # We found the data
      }
    }
    return(data.frame(Status = "Could not retrieve data from any API.")) # Failed after trying all
  }
  
  # --- Manhattan Plot Click Logic ---
  
 clicked_snp_data <- eventReactive(input$manhattan_click, {
   req(input$manhattan_click, input$study_selector)
   
   click_info <- input$manhattan_click
   
   # --- FIX: Scale the plot coordinate to the genomic coordinate ---
   # 1. Get the total span of the genomic x-axis from our map file
   last_chr_info <- chr_map[nrow(chr_map), ]
   total_genome_span_bp <- last_chr_info$total + last_chr_info$chr_len
   
   # 2. Scale the click coordinate to the cumulative genomic position
   clicked_bp_cum <- (click_info$x / click_info$domain$right) * total_genome_span_bp
   
   # 3. Find the correct chromosome and real position from the cumulative position
   clicked_chr_info <- chr_map %>% 
     filter(total <= clicked_bp_cum) %>% 
     filter(total == max(total))
   
   if (nrow(clicked_chr_info) == 0) { return(data.frame()) }
   
   clicked_chr <- clicked_chr_info$Chromosome
   real_pos <- round(clicked_bp_cum - clicked_chr_info$total)
   search_radius <- 50000 # Search +/- 50kb
   
   results <- call_api(
     endpoint = "region",
     body_params = list(
       chromosome = clicked_chr,
       start_pos = real_pos - search_radius,
       end_pos = real_pos + search_radius,
       study_id = input$study_selector
     )
   )
   
   if (!is.data.frame(results)) { return(data.frame()) }
   
   if (nrow(results) > 0 && !"Status" %in% names(results)) {
     results <- results %>%
       arrange(P_value) %>%
       select(StudyID, P_value, LOG10P, everything(), -Chromosome, -Position, -Direction)
   }
   return(results)
 })
  
  output$peak_info_table <- DT::renderDataTable({ datatable(clicked_snp_data(), options = list(pageLength = 5, scrollX = TRUE)) })
  
  # --- Summary Data Logic ---
  summary_data <- reactive({
    req(input$study_selector)
   results <- call_api(
      endpoint = "summary",
      body_params = list(study_id = input$study_selector)
    )
    if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
      results <- results %>%
        select(Chromosome, Position, P_value, LOG10P, everything(), MarkerName)
    }
    return(results)
  })
  
  output$summary_table <- DT::renderDataTable({ datatable(summary_data(), options = list(scrollX = TRUE)) })
  
  # --- Cross-Phenotype Search Button Logic ---
  cross_pheno_results <- eventReactive(input$search_snp_button, {
    req(input$pos_search)
    results <- call_api(
      endpoint = "phewas",
      body_params = list(
        chromosome = input$chr_search,
        position = input$pos_search
      )
    )
    
    if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
      results <- results %>%
        arrange(desc(LOG10P)) %>%
        select(StudyID, P_value, LOG10P, everything(), -Chromosome, -Position, -Direction)
    }
    return(results)
  })
  
  output$cross_pheno_table <- DT::renderDataTable({ datatable(cross_pheno_results(), options = list(pageLength = 10, scrollX = TRUE)) })
  
  # --- Other Outputs (Plots, Downloads) ---
  output$manhattan_plot <- renderImage({
    req(input$study_selector)
    list(src = file.path("www", paste0(input$study_selector, ".png")), contentType = 'image/png', width = "100%") 
  }, deleteFile = FALSE)
  
  output$qq_plot <- renderImage({
    req(input$study_selector)
    list(src = file.path("www", paste0("qq_", input$study_selector, ".png")), contentType = 'image/png', height = "350px")
  }, deleteFile = FALSE)
  
  output$download_cross_pheno <- downloadHandler(
    filename = function() { paste0("cross-pheno-results-chr", input$chr_search, "-", input$pos_search, ".csv") },
    content = function(file) { write.csv(cross_pheno_results(), file, row.names = FALSE) }
  )
}

# --- 5. RUN ---
shinyApp(ui, server)
