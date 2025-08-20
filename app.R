
# --- 1. LOAD PACKAGES ---
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(shinycssloaders)
library(httr)
library(jsonlite)
library(htmlwidgets)
library(readr)
library(tibble)
library(plotly)
library(DBI)
library(RSQLite)
library(shinyjs)
calibration_mode = FALSE
# --- 2. DATA & CONFIG ---

# This is the central summary details file, linked by PheCode.
# For now, we create it as a dummy data frame. Later, you can replace this
# with: study_metadata <- read_csv("your_summary_details_file.csv")
study_metadata <- tibble::tribble(
  ~StudyID, ~PheCode, ~Ancestry, ~Cases, ~Controls, ~TraitName,
  "staph_aureus", "ID_002.1", "European", 1204, 50000, "Staphylococcus aureus",
  "staphylococcus", "ID_002", "European", 850, 45000, "Staphylococcus",
  "escherichia_coli", "ID_003", "Mixed", 2500, 98000, "Escherichia Coli",
  "streptococcus", "ID_004", NA, NA, NA, "Streptococcus",
  "group_a_streptococcus", "ID_004.2", NA, NA, NA, "Group A Streptococcus",
  "group_b_streptococcus", "ID_004.3", NA, NA, NA, "Group B Streptococcus",
  "mycobacteria", "ID_005", NA, NA, NA, "Mycobacteria",
  "mycobacterium_tuberculosis", "ID_005.1", NA, NA, NA, "Mycobacterium Tuberculosis",
  "neisseria", "ID_006", NA, NA, NA, "Neisseria",
  "neisseria_gonorrhea", "ID_006.2", NA, NA, NA, "Neisseria Gonorrhea",
  "H_pylori", "ID_008", NA, NA, NA, "Helicobacter pylori",
  "clostridium", "ID_015", NA, NA, NA, "Clostridium",
  "clostridium_difficile", "ID_015.2", NA, NA, NA, "Clostridium Difficile",
  "chlamydia", "ID_016", NA, NA, NA, "Chlamydia",
  "chlamydia_trachomatis", "ID_016.1", NA, NA, NA, "Chlamydia Trachomatis",
  "treponema", "ID_019", NA, NA, NA, "Treponema",
  "treponema_pallidum", "ID_019.1", NA, NA, NA, "Treponema Pallidum (Syphilis)",
  "borrelia", "ID_020", NA, NA, NA, "Borrelia",
  "lyme_disease", "ID_020.1", NA, NA, NA, "Lyme Disease",
  "herpesvirus", "ID_052", NA, NA, NA, "Herpesvirus",
  "herpes_simplex", "ID_052.1", NA, NA, NA, "Herpes Simplex",
  "varicella_zoster", "ID_052.3", NA, NA, NA, "Varicella Zoster Virus",
  "infectious_mono", "ID_052.4", NA, NA, NA, "Infectious Mononucleosis",
  "cytomegalovirus", "ID_052.5", NA, NA, NA, "Cytomegalovirus (CMV)",
  "varicella_chickenpox", "ID_052.31", NA, NA, NA, "Varicella (Chickenpox)",
  "herpes_zoster", "ID_052.32", NA, NA, NA, "Herpes Zoster",
  "hepatovirus", "ID_054", NA, NA, NA, "Hepatovirus",
  "hepatitis_a", "ID_054.1", NA, NA, NA, "Hepatitis A",
  "hepatitis_b", "ID_054.2", NA, NA, NA, "Hepatitis B",
  "hepatitis_c", "ID_054.3", NA, NA, NA, "Hepatitis C",
  "hepatitis_b_with_delta", "ID_054.21", NA, NA, NA, "Hepatitis B with Delta",
  "chronic_hepatitis_c", "ID_054.31", NA, NA, NA, "Chronic Hepatitis C",
  "acute_hepatitis_c", "ID_054.32", NA, NA, NA, "Acute Hepatitis C",
  "poxvirus", "ID_055", NA, NA, NA, "Poxvirus",
  "molluscum_cont", "ID_055.1", NA, NA, NA, "Molluscum Contagiosum",
  "hpv", "ID_056", NA, NA, NA, "Human Papilloma Virus",
  "plantar_wart", "ID_056.1", NA, NA, NA, "Plantar Warts",
  "anogenital_warts", "ID_056.2", NA, NA, NA, "Anogenital Warts",
  "retrovirus", "ID_057", NA, NA, NA, "Retrovirus",
  "hiv", "ID_057.1", NA, NA, NA, "Human Immunodeficiency Virus",
  "pneumo", "ID_058", NA, NA, NA, "Pneumoviridae",
  "corona", "ID_059", NA, NA, NA, "Coronavirus",
  "cov2", "ID_059.1", NA, NA, NA, "Sars-CoV-2",
  "influenza", "ID_061", NA, NA, NA, "Influenza Virus",
  "other_viral", "ID_069", NA, NA, NA, "Other Specified Viral Infections",
  "candidiasis", "ID_070", NA, NA, NA, "Candidiasis",
  "aspergillosis", "ID_074", NA, NA, NA, "Aspergillosis",
  "pneumocystosis", "ID_076", NA, NA, NA, "Pneumocystosis",
  "parasites", "ID_084", NA, NA, NA, "Parasites",
  "trichomoniasis", "ID_084.4", NA, NA, NA, "Trichomoniasis",
  "toxoplasmosis", "ID_084.5", NA, NA, NA, "Toxoplasmosis",
  "giardiasis", "ID_084.7", NA, NA, NA, "Giardiasis",
  "pediculosis_acarisis_other", "ID_086.1", NA, NA, NA, "Pediculosis, Acarisis and Other Infections",
  "scabies", "ID_086.2", NA, NA, NA, "Scabies",
  "std", "ID_088", NA, NA, NA, "Sexually Transmitted Disease",
  "infections", "ID_089", NA, NA, NA, "Infections",
  "bacterial_infections", "ID_089.1", NA, NA, NA, "Bacterial Infections",
  "viral_infections", "ID_089.2", NA, NA, NA, "Viral Infections",
  "fungal_infections", "ID_089.3", NA, NA, NA, "Fungal Infections",
  "gangrene", "ID_091", NA, NA, NA, "Gangrene",
  "bacteremia_sepsis_sirs", "ID_092", NA, NA, NA, "Bacteremia, Sepsis, and SIRS",
  "systemic_inflammatory_response", "ID_092.1", NA, NA, NA, "Systemic Inflammatory Response Syndrome",
  "sepsis", "ID_092.2", NA, NA, NA, "Sepsis",
  "bacteremia", "ID_092.8", NA, NA, NA, "Bacteremia",
  "drug_resistance", "ID_097", NA, NA, NA, "Drug Resistant Microorganisms",
  "mrsa", "ID_097.1", NA, NA, NA, "Methicillin-resistant Staphylococcus Aureus",
  "beta_lactam_resistance", "ID_097.3", NA, NA, NA, "Resistance to beta-lactam antibiotics"
)
# Create a named list for the dropdown menu choices from the metadata
dataset_catalog <- setNames(study_metadata$StudyID, study_metadata$TraitName)
print(paste("Building UI. At this moment, calibration_mode is:", calibration_mode))

# --- 3. USER INTERFACE (UI) ---

ui <- page_sidebar(
  useShinyjs(),
  fillable = FALSE,
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  title = tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
    "GWAS Data for Infectious Disease Phenotypes",
    tags$a(
      href = "https://icahn.mssm.edu/research/institute-genomic-health",
      tags$img(
      
        src = "MS_Icahn_RGB_Hrztl_no_reg.png",
        height = "60px"
      )
    )
  ),
  #  sidebar
  sidebar = sidebar(
    selectInput("study_selector", h4("Select a Study"),
                choices = dataset_catalog),
    div(id = "calibration_panel_div",
        checkboxInput("calibration_mode_toggle", "Enter Alignment Calibration Mode", TRUE),
        conditionalPanel(
          "input.calibration_mode_toggle == true",
          sliderInput("left_padding", "Adjust Left Side (X-Axis)", min = -200000000, max = 200000000, value = -66666666),
          sliderInput("right_padding", "Adjust Right Side (X-Axis)", min = -200000000, max = 200000000, value = -136000000),
          sliderInput("bottom_padding", "Adjust Bottom (Y-Axis)", min = -5, max = 5, value = 1, step = 0.1),
          sliderInput("top_padding_factor", "Adjust Top (Y-Axis)", min = 1.0, max = 1.5, value = 1.05, step = 0.01),
          hr(),
          actionButton("save_calibration", "Save Current Settings", class = "btn-primary w-100")
        )
    ),

    hr(),
    h4("Search by MarkerName"),
    textInput("marker_search", "MarkerName:", placeholder = "e.g., chr1:12345:A:G"),
    actionButton("search_marker_button", "Search", class = "btn-success w-100"),
    actionLink("clear_marker_search", "Clear"),
    hr(),
    h4("Search by Position"),
    textInput("chr_search", "Chromosome:", placeholder = "e.g., 1 or 22"),
    numericInput("pos_search", "Position (HG38):", value = NULL, min = 0),
    actionButton("search_snp_button", "Search", class = "btn-success w-100"),
    actionLink("clear_position_search", "Clear")
    
  ),
  # Tabs & Pages
  navset_card_tab(
    id = "main_tabs",
    nav_panel("Plots and Details",
              div(
                # LAYOUT FIX: Added margin-bottom to push content below it down.
                style = "position: relative; width: 100%; max-width: 1200px; margin: auto; aspect-ratio: 1200 / 400; margin-bottom: 20px;",
                div(
                  style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                  imageOutput("manhattan_plot_base", height = "100%", width = "100%")
                ),
                div(
                  style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                  plotlyOutput("manhattan_plot_interactive_layer", height = "100%", width = "100%")
                )
              ),
             
        fluidRow(
          style = "margin-top: 140px",
          column(6,
                 card(
                   h4("Study Details"),
                   tableOutput("study_details_table"),
                 )
          ),
          column(6,
                 card(
                   withSpinner(imageOutput("qq_plot", height = "auto", width = "100%"))
                 )
          ) 
        )
        ),     
  nav_panel("Top Significant Hits", card(
      card_header("Top Significant Hits"),
      withSpinner(DT::dataTableOutput("summary_table")),
      downloadButton("download_summary_btn", "Download Table", class = "btn-success mt-3")
    )),
   
  nav_panel("Cross-Phenotype Search", card(
      card_header("Results for Searched SNP"), withSpinner(DT::dataTableOutput("cross_pheno_table")),
      downloadButton("download_cross_pheno", "Download Results", class = "btn-success mt-3")
    )),
  nav_panel("Plot Variance", 
            card(uiOutput("locus_plots_ui") 
            )),
  nav_panel("About",
              card(
                h4("About This Application"),
                p("This application provides a high-performance, interactive web interface for exploring and comparing results from multiple GWAS datasets with Infectious Disease phenotypes. It is designed for fast, responsive data exploration between the current datasets uploaded and processed in the server, and is still undergoing updates and improvements."),
                p("The core features include: "),
                p("Interactive Manhattan Plot with pre-rendered image overlayed with invisible significant points that display Marker Name and autopopulate search bar on click."),
                p("Pre-rendered QQ plots for each study, and key metadata within the Study Details section."),
                p("Table display of Top Significant Hits (-log10P above 5) shown in descending order."),
                p("Cross-Phenotype search results for cross-study associations of specific SNPs across all studies by Marker Name or genomic position. Search results restricted to SNPS with a -log10P value above 1."),
                p("LocusZoom and Forest Plots available upon click for genome-wide significant points."),
                 p("Data Sources: NIH All of Us, BioMe BioBank, UK BioBank"),
                p("If you use data or images from this tool, please cite..."),
                p("Data Visualization, Comparision Tool, and Site Creation by Rechuma Hafter")
                
      )
    )
  ) 
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  if (!calibration_mode){
    shinyjs::hide("calibration_panel_div")
  }
  if (calibration_mode) {
    # --- DEVELOPMENT MODE ---
    app_dir <- getwd()
    parent_dir <- dirname(app_dir)
    calibration_file_path <- file.path(parent_dir, "calibration_settings.csv")
    
    cat("--- CALIBRATION MODE IS ON ---\n")
    cat("Settings will be saved to the parent directory:\n")
    cat(calibration_file_path, "\n\n")
    
  } else {
    # --- PRODUCTION/DEPLOYMENT MODE ---
    calibration_file_path <- "calibration_settings.csv"
    
    cat("--- CALIBRATION MODE IS OFF (DEPLOYMENT) ---\n")
    if (file.exists(calibration_file_path)) {
      cat("SUCCESS: Found 'calibration_settings.csv' in the app directory.\n")
      
      # NEW DIAGNOSTIC: Attempt to read the file and print its contents to the log.
      tryCatch({
        deployed_settings <- readr::read_csv(calibration_file_path)
        cat("--- CONTENTS OF DEPLOYED CSV FILE ---\n")
        print(deployed_settings)
        cat("-------------------------------------\n\n")
      }, error = function(e) {
        cat("ERROR: Found the file, but FAILED TO READ IT. Error message:", e$message, "\n\n")
      })
      
    } else {
      cat("ERROR: DID NOT FIND 'calibration_settings.csv' in the app directory.\n")
    }
  }
  snp_info_to_display <- reactiveVal(NULL)
  cross_pheno_results <- reactiveVal(data.frame())
  initial_cal_df <- tryCatch({
    readr::read_csv(calibration_file_path, col_types = cols(
      StudyID = col_character(),
      left_padding = col_double(),
      right_padding = col_double(),
      bottom_padding = col_double(),
      top_padding_factor = col_double()
    ))
  }, error = function(e) {
      tibble(
      StudyID = character(), left_padding = double(), right_padding = double(),
      bottom_padding = double(), top_padding_factor = double()
    )
  })
  calibration_settings <- reactiveVal(initial_cal_df)
   # Connect to the local database of significant hits for fast plotting
interactive_con <- dbConnect(RSQLite::SQLite(), "interactive_hits.sqlite")
onStop(function() { dbDisconnect(interactive_con) })
  
  # Load the pre-calculated chromosome map for positioning
map_data <- readRDS("chr_map_data.rds")
chr_map <- map_data$chr_map
total_genome_length <- map_data$total_genome_length
  
  # IMPORTANT: Fill these in with your deployed API URLs
  API_URL_GROUP_1 <- "https://rstudio-connect.hpc.mssm.edu/content/ced643da-26dd-4b88-9f39-aec6c8c4c000/"
  API_URL_GROUP_2 <- "https://rstudio-connect.hpc.mssm.edu/content/4f6a0804-9030-4ed0-9fd2-186379b35a10/"
  API_URL_GROUP_3 <- "https://rstudio-connect.hpc.mssm.edu/content/30618e94-0ce0-466b-9ce0-ff2568f40399/"
  ALL_API_URLS <- c(API_URL_GROUP_1, API_URL_GROUP_2, API_URL_GROUP_3)
  
  # --- 4.2 API Calling Helper Function ---
call_api <- function(endpoint, body_params) {
  for (base_url in ALL_API_URLS) {
    cat("\n -> Trying API:", base_url, "\n")
    api_url <- paste0(base_url, endpoint)
    response <- tryCatch({
      POST(url = api_url, body = body_params, encode = "json", timeout(90))
    }, error = function(e) {
      # Return a specific message if the connection itself fails
      return(list(Status = paste("API connection error:", e$message)))
    })
    
    if (!is.null(response) && http_status(response)$category == "Success") {
      content <- fromJSON(content(response, "text", encoding = "UTF-8"))
      if (is.data.frame(content) && "Message" %in% names(content) &&
          (grepl("NOT_IN_THIS_API", content$Message[1]))) {
        next 
      }
      return(content) 
    } else if (!is.null(response)) {
      return(data.frame(Status = http_status(response)$message))
    }
  }
  return(data.frame(Status = "Could not retrieve data from any API.")) 

  cat("     FAILURE: Could not retrieve data from any API.\n")     # <-- DELETE THIS LINE
  cat("----------------------------------\n\n")                    # <-- DELETE THIS LINE
  return(data.frame(Status = "Could not retrieve data from any API.")) # <-- DELETE THIS LINE

  # If the loop finishes without returning, it means all APIs failed
  cat("   FAILURE: Could not retrieve data from any API.\n")
  cat("----------------------------------\n\n")
  return(data.frame(Status = "Could not retrieve data from any API.")) 

} 
  
  ## 4.3: Main Plot Rendering (Hybrid Plot)
  
  # Layer 1: Renders the static, pre-generated PNG image
  output$manhattan_plot_base <- renderImage({
    req(input$study_selector)
    list(src = file.path("www", paste0(input$study_selector, ".png")),
      contentType = 'image/png',
    width = "100%"
  )
      }, deleteFile = FALSE)
  # Layer 2: Renders the invisible layer of significant points for hover/click
output$manhattan_plot_interactive_layer <- renderPlotly({
    req(input$study_selector)
  req(calibration_settings()) 
      # --- NORMAL INTERACTIVE PLOT ---
  query <- sprintf("SELECT * FROM `%s`", input$study_selector)
  interactive_points <- dbGetQuery(interactive_con, query)
      
  if (!is.data.frame(interactive_points) || nrow(interactive_points) == 0) return(NULL)
      
  interactive_points <- interactive_points %>%
      mutate(Chromosome = as.character(Chromosome)) %>%
      inner_join(select(chr_map, Chromosome, cumulative_start), by = "Chromosome") %>%
      mutate(BP_cumulative = Position + cumulative_start)
  all_saved_settings <- calibration_settings()
  current_saved_settings <- all_saved_settings %>% filter(StudyID == input$study_selector)
     # Default values in case a study is missing from the CSV
  if (nrow(current_saved_settings) == 0) {
       current_saved_settings <- list(
         left_padding = -66666667,
         right_padding = -136111111,
         bottom_padding = 2,
         top_padding_factor = 1.1
       )
     }
  use_live_sliders <- calibration_mode && isTRUE(input$calibration_mode_toggle)
  x_range_min <- if (use_live_sliders) input$left_padding else current_saved_settings$left_padding
  x_range_max <- if (use_live_sliders) total_genome_length + input$right_padding else total_genome_length + current_saved_settings$right_padding
  y_range_min <- if (use_live_sliders) input$bottom_padding else current_saved_settings$bottom_padding
  y_range_max_factor <- if (use_live_sliders) input$top_padding_factor else current_saved_settings$top_padding_factor
  
  plot_ly(
    data = interactive_points, x = ~BP_cumulative, y = ~LOG10P,
    type = 'scatter', mode = 'markers',
    marker = list(color = if (calibration_mode) 'red' else 'rgba(0,0,0,0)', size = 3),
        text = ~MarkerName, customdata = ~MarkerName,
        hoverinfo = 'text', source = "interactive_layer_source"
      ) %>%
  layout(
    xaxis = list(range = c(x_range_min, x_range_max),
                       showgrid = FALSE,
                       zeroline = FALSE,
                       showticklabels = FALSE, 
                       title = "",
                 automargin = FALSE
    ),
    yaxis = list(range = c(y_range_min, max(interactive_points$LOG10P, na.rm = TRUE) * y_range_max_factor),
                       showgrid = FALSE,
                       zeroline = FALSE,
                       showticklabels = FALSE,
                       title = "",
                 automargin = FALSE
          ),
          margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
          paper_bgcolor = 'transparent', 
          plot_bgcolor = 'transparent',
          showlegend = FALSE
        ) %>%
        config(displayModeBar = FALSE) %>%
        event_register('plotly_click')
    
})
  
  ## 4.4: Interactive Logic
output$study_details_table <- renderTable({
    study_metadata %>%
      filter(StudyID == input$study_selector) %>%
      select(PheCode, Ancestry, Cases, Controls) 
  }, bordered = TRUE, hover = TRUE)
  # Instantly populates the search box when a point on the plot is clicked
observeEvent(event_data("plotly_click", source = "interactive_layer_source"), {
    clicked_marker <- event_data("plotly_click", source = "interactive_layer_source")$customdata
    updateTextInput(session, "marker_search", value = clicked_marker)
    query <- sprintf("SELECT P_value FROM `%s` WHERE MarkerName = ?", input$study_selector)
    snp_data <- dbGetQuery(interactive_con, query, params = list(clicked_marker))
    is_sig <- !is.null(snp_data$P_value) && snp_data$P_value < 5e-8
    
  # 3. Update the reactiveVal to trigger the plot display
snp_info_to_display(
   list(
        marker = clicked_marker,
        is_significant = is_sig
      )
    )
  }, ignoreNULL = TRUE)
  # Marker Name search button logic
observeEvent(input$search_marker_button, {
    req(input$marker_search)
    shinyjs::disable("search_marker_button")
    shinyjs::html("search_marker_button", "Searching...")
    
tryCatch({
  marker_name_clean <- trimws(input$marker_search)
  # First attempt with the original name
  results <- call_api(
    endpoint = "marker",
    body_params = list(marker_name = marker_name_clean)
  )
  # If the first attempt failed, try the other format
  if (is.data.frame(results) && "Status" %in% names(results)) {
    modified_name <- if (startsWith(marker_name_clean, "chr")) sub("^chr", "", marker_name_clean) else paste0("chr", marker_name_clean)
    results <- call_api(endpoint = "marker", body_params = list(marker_name = modified_name))
  }
  if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
    if (!"LOG10P" %in% names(results) && "P_value" %in% names(results)) {
      results <- results %>% mutate(LOG10P = -log10(P_value))
    }
    processed_results <- results %>%
      arrange(desc(LOG10P)) %>%
      select(StudyID, P_value, LOG10P, MarkerName, Chromosome, Position, everything(),
             -Direction, -FreqSE, -MinFreq, -MaxFreq, -HetISq, -HetChiSq, -HetDf)
    cross_pheno_results(processed_results)
  } else {
    cross_pheno_results(data.frame(Message = "MarkerName not found or API error."))
  }
}, finally = {
  shinyjs::html("search_marker_button", "Search")
  shinyjs::enable("search_marker_button")
})
  })
 
output$cross_pheno_table <- DT::renderDataTable({
    datatable(cross_pheno_results(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Position search button logic
observeEvent(input$search_snp_button, {
    req(input$pos_search)
    shinyjs::disable("search_snp_button")
    shinyjs::html("search_snp_button", "Searching...")
    
    tryCatch({
      # Clean the inputs by removing whitespace
      chr_clean <- trimws(input$chr_search)
      pos_clean <- as.numeric(trimws(input$pos_search))
      results <- call_api(
        endpoint = "phewas",
        body_params = list(chromosome = chr_clean, position = pos_clean)
      )
      # If the first attempt failed, try the other format
      if (is.data.frame(results) && "Status" %in% names(results)) {
        modified_chr <- if (startsWith(tolower(chr_clean), "chr")) sub("^chr", "", chr_clean, ignore.case = TRUE) else paste0("chr", chr_clean)
        results <- call_api(endpoint = "phewas", body_params = list(chromosome = modified_chr, position = pos_clean))
      }
      
      # Process the final result
      if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
        if (!"LOG10P" %in% names(results) && "P_value" %in% names(results)) {
          results <- results %>% mutate(LOG10P = -log10(P_value))
        }
        processed_results <- results %>%
          arrange(desc(LOG10P)) %>%
          select(StudyID, P_value, LOG10P, Chromosome, Position, MarkerName, everything(),
                 -Direction, -FreqSE, -MinFreq, -MaxFreq, -HetISq, -HetChiSq, -HetDf)
        cross_pheno_results(processed_results)
      } else {
        cross_pheno_results(data.frame(Message = "Position not found or API error."))
      }
    }, finally = {
      shinyjs::html("search_snp_button", "Search")
      shinyjs::enable("search_snp_button")
    })
  })
output$cross_pheno_table <- DT::renderDataTable({
  datatable(cross_pheno_results(), options = list(pageLength = 10, scrollX = TRUE))
})  
   
 # --- Summary Table Logic --- 
  summary_data <- reactive({
    req(input$study_selector)
    results <- call_api(endpoint = "summary", body_params = list(study_id = input$study_selector))
    if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
      results <- results %>%
        select(Chromosome, Position, P_value, LOG10P, everything(), MarkerName, StudyID, 
               -Direction, -FreqSE, -MinFreq, -MaxFreq, -HetISq, -HetChiSq, -HetDf)
    }
    return(results)
  })
  output$summary_table <- DT::renderDataTable({ datatable(summary_data(), options = list(scrollX = TRUE)) })
  
  # --- Other Outputs (Plots, Downloads) ---
    # Output QQPLOT
  output$qq_plot <- renderImage({
    req(input$study_selector)
    list(src = file.path("www", paste0("qq_", input$study_selector, ".png")), contentType = 'image/png', width = "100%")
  }, deleteFile = FALSE)
  
  # Clears the MarkerName Search
  observeEvent(input$clear_marker_search, {
    updateTextInput(session, "marker_search", value = "")
    cross_pheno_results(data.frame())
  })
  # Clears the Position search
  observeEvent(input$clear_position_search, {
    updateTextInput(session, "chr_search", value = "")
    updateNumericInput(session, "pos_search", value = NA)
    cross_pheno_results(data.frame())
  })
  # Save function for Calibration
  observeEvent(input$save_calibration, {
    req(input$study_selector)
    showNotification("Saving settings...", type = "message", duration = 2)
    cal_data <- calibration_settings()
    current_study <- input$study_selector
    new_settings <- tibble(
      StudyID = current_study, left_padding = input$left_padding, right_padding = input$right_padding,
      bottom_padding = input$bottom_padding, top_padding_factor = input$top_padding_factor
    )
    if (current_study %in% cal_data$StudyID) {
      cal_data[cal_data$StudyID == current_study, ] <- new_settings
    } else {
      cal_data <- bind_rows(cal_data, new_settings)
    }
    readr::write_csv(cal_data, calibration_file_path)
    calibration_settings(cal_data)
    showNotification("Settings saved successfully!", type = "message", duration = 3)
  })
  observeEvent(input$study_selector, {
    req(input$study_selector)
    all_settings <- calibration_settings()
    current_settings <- all_settings %>% filter(StudyID == input$study_selector)
    if (nrow(current_settings) > 0) {
      updateSliderInput(session, "left_padding", value = current_settings$left_padding)
      updateSliderInput(session, "right_padding", value = current_settings$right_padding)
      updateSliderInput(session, "bottom_padding", value = current_settings$bottom_padding)
      updateSliderInput(session, "top_padding_factor", value = current_settings$top_padding_factor)
    } else {
      updateSliderInput(session, "left_padding", value = -99666666)
      updateSliderInput(session, "right_padding", value = -166000000)
      updateSliderInput(session, "bottom_padding", value = 2.1)
      updateSliderInput(session, "top_padding_factor", value = 1.15)
    }
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  # Locus/Forest Plot logic
  output$locus_plots_ui <- renderUI({
    req(input$study_selector)
    query <- sprintf("SELECT MarkerName, Chromosome, Position, P_value, LOG10P FROM `%s` WHERE LOG10P > 7.3", input$study_selector)
    sig_hits <- dbGetQuery(interactive_con, query)
    if (nrow(sig_hits) == 0) {
      return(div(h4("No Genome-Wide Significant Hits for this study.")))
    }
  lead_snps <- sig_hits %>%
      arrange(as.numeric(Chromosome), Position) %>%
      mutate(locus_id = 0, .before = 1) 
    if(nrow(lead_snps) > 0) {
      current_locus <- 1
      lead_snps$locus_id[1] <- current_locus
      if (nrow(lead_snps) > 1) {
        for (i in 2:nrow(lead_snps)) {
          if (lead_snps$Chromosome[i] != lead_snps$Chromosome[i-1] || 
              (lead_snps$Position[i] - lead_snps$Position[i-1] > 1000000)) {
            current_locus <- current_locus + 1
          }
          lead_snps$locus_id[i] <- current_locus
        }
      }
      lead_snps <- lead_snps %>%
        group_by(locus_id) %>%
        slice_max(order_by = LOG10P, n = 1) %>%
        ungroup()
    }
   lapply(1:nrow(lead_snps), function(i) {
      hit <- lead_snps[i, ]
      locus_img_name <- paste0("fakelocus", (i-1) %% 3 + 1, ".png")
      forest_img_name <- paste0("fakeforest", (i-1) %% 3 + 1, ".png")
      fluidRow(
        style = "margin-bottom: 25px; padding-bottom: 15px; border-bottom: 1px solid #eee;",
        h4(paste("Lead SNP for Locus", i, ":", hit$MarkerName)),
        p(paste("Location: chr", hit$Chromosome, ":", hit$Position, " (P-value: ", formatC(hit$P_value, format = "e", digits = 2), ")", sep = "")),
        column(6,
               h5("LocusZoom Plot"),
               tags$img(src = locus_img_name, width = "100%")
        ),
        column(6,
               h5("Forest Plot"),
               tags$img(src = forest_img_name, width = "100%")
        )
      )
    })
  })
  # --- Download Handlers ---
  output$download_summary_btn <- downloadHandler(
    filename = function() {
      paste0(input$study_selector, "_top_hits.csv")
    },
    content = function(file) {
      write.csv(summary_data(), file, row.names = FALSE)
    }
  )
  output$download_cross_pheno <- downloadHandler(
    filename = function() {
      if (!is.null(input$pos_search) && input$pos_search > 0) {
        paste0("cross-pheno-results-chr", input$chr_search, "-", input$pos_search, ".csv")
      } else if (!is.null(input$marker_search) && input$marker_search != "") {
        paste0("cross-pheno-results-marker-", input$marker_search, ".csv")
      } else {
        "cross-pheno-results.csv"
      }
    },
    content = function(file) {
      write.csv(cross_pheno_results(), file, row.names = FALSE)
    }
  )
}

# --- 5. RUN ---
shinyApp(ui, server)
