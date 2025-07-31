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
library(readr)
library(tibble)
library(plotly)
library(DBI)
library(RSQLite)
# --- 2. DATA & CONFIG ---

# This is your central summary details file, linked by PheCode.
# For now, we create it as a dummy data frame. Later, you can replace this
# with: study_metadata <- read_csv("your_summary_details_file.csv")
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
  "varicella_zoster_virus", "ID_052.3", NA, NA, NA, "Varicella zoster virus",
  "infectious_mononucleosis", "ID_052.4", NA, NA, NA, "Infectious mononucleosis",
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
  "molluscum_contagiosum", "ID_055.1", NA, NA, NA, "Molluscum contagiosum",
  "hpv", "ID_056", NA, NA, NA, "Human papillomavirus",
  "plantar_wart", "ID_056.1", NA, NA, NA, "Plantar wart",
  "anogenital_warts", "ID_056.2", NA, NA, NA, "Anogenital warts",
  "retrovirus", "ID_057", NA, NA, NA, "Retrovirus",
  "hiv", "ID_057.1", NA, NA, NA, "Human immunodeficiency virus",
  "pneumoviridae", "ID_058", NA, NA, NA, "Pneumoviridae",
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
  "pediculosis", "ID_086.1", NA, NA, NA, "Pediculosis",
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
# Create a named list for the dropdown menu choices from the metadata
dataset_catalog <- setNames(study_metadata$StudyID, study_metadata$TraitName)

# --- 3. USER INTERFACE (UI) ---

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  title = tags$div(
    style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
    "GWAS Viewer",
    tags$a(
      href = "https://icahn.mssm.edu/research/institute-genomic-health",
      tags$img(
        # Paste the "Raw" GitHub URL here
        src = "https://github.com/RecHaf04/RechumaGWAS/blob/main/MS_Icahn_K_Hrztl_no_reg.png?raw=true",
        height = "60px"
      )
    )
  ),
  
  sidebar = sidebar(
    selectInput("study_selector", h4("Select a Study"),
                choices = dataset_catalog),
    hr(),
    h4("Search by MarkerName"),
    textInput("marker_search", "MarkerName:", placeholder = "e.g., chr1:12345:A:G"),
    actionButton("search_marker_button", "Search", class = "btn-success w-100"),
    hr(),
    h4("Search by Position"),
    textInput("chr_search", "Chromosome:", placeholder = "e.g., 1 or X"),
    numericInput("pos_search", "Position (HG38):", value = NULL, min = 0),
    actionButton("search_snp_button", "Search", class = "btn-success w-100")
  ),
  
  
  navset_card_tab(
    
    id = "main_tabs",
    nav_panel("Plots and Details", 
      
              div(
                style = "position: relative; height: 450px; width: 100%;",
                div(
                  style = "position: absolute; top: 0; left: 0; width: 100%; height: 100%;",
                  imageOutput("manhattan_plot_base", height = "100%", width = "100%")
                ),
                div(
                  style = "position: absolute; top: 18px; left: 30px; width: 100%; height: 100%;",
                  plotlyOutput("manhattan_plot_interactive_layer", height = "100%", width = "100%")
                )
              ),     
           hr(),
              fluidRow(
                style = "margin-top: 25px;",
                column(6, 
                       h4("Study Details"),
                       tableOutput("study_details_table"),
                        ),
                column(6, 
                       h4(),
                       withSpinner(imageOutput("qq_plot", height = "450px"))
                )
              )
    ),
   
     nav_panel("Top Significant Hits", card(
      card_header("Top Significant Hits"),
      withSpinner(DT::dataTableOutput("summary_table"))
    )),
   
     nav_panel("Cross-Phenotype Search", card(
      card_header("Results for Searched SNP"), withSpinner(DT::dataTableOutput("cross_pheno_table")),
      downloadButton("download_cross_pheno", "Download Results", class = "btn-success mt-3")
    ))
  )
)

# --- 4. SERVER ---
server <- function(input, output, session) {
  ## 4.1: Initial Setup & Connections
  
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
      api_url <- paste0(base_url, endpoint)
      response <- tryCatch({
        POST(url = api_url, body = body_params, encode = "json", timeout(30))
      }, error = function(e) { return(NULL) })
      
      if (!is.null(response) && http_status(response)$category == "Success") {
        content <- fromJSON(content(response, "text", encoding = "UTF-8"))
        if (is.data.frame(content) && "Message" %in% names(content) &&
            (grepl("NOT_IN_THIS_API", content$Message[1]))) {
          next 
        }
        return(content) 
      }
    }
    return(data.frame(Status = "Could not retrieve data from any API.")) 
  }
  
  
  ## 4.3: Main Plot Rendering (Hybrid Plot)
  
  # Layer 1: Renders the static, pre-generated PNG image
  output$manhattan_plot_base <- renderImage({
    req(input$study_selector)
    list(
      src = file.path("www", paste0(input$study_selector, ".png")),
      contentType = 'image/png',
      width = "100%",
      height = 450  # Set height in pixels to match the container
    )
  }, deleteFile = FALSE)
  # Layer 2: Renders the invisible layer of significant points for hover/click
  output$manhattan_plot_interactive_layer <- renderPlotly({
    req(input$study_selector)
    
    # Fetch significant points from the FAST local SQLite database
    query <- sprintf("SELECT * FROM `%s`", input$study_selector)
    interactive_points <- dbGetQuery(interactive_con, query)
    
    if (!is.data.frame(interactive_points) || nrow(interactive_points) == 0) return(NULL)
    
    # Create cumulative coordinates for plotting
    interactive_points <- interactive_points %>%
      mutate(Chromosome = as.character(Chromosome)) %>%
      inner_join(select(chr_map, Chromosome, cumulative_start), by = "Chromosome") %>%
      mutate(BP_cumulative = Position + cumulative_start)
    plot_ly(
      data = interactive_points,
      x = ~BP_cumulative, y = ~LOG10P,
      type = 'scatter', mode = 'markers',
      marker = list(color = 'transparent'),
      text = ~MarkerName, customdata = ~MarkerName,
      hoverinfo = 'text', source = "interactive_layer_source"
    ) %>%
      layout(
        xaxis = list(range = c(-95000000, total_genome_length + 100000000), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
        yaxis = list(range = c(-1.6, max(interactive_points$LOG10P, na.rm = TRUE) * 1.15), showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE, title = ""),
        margin = list(l = 0, r = 0, b = 0, t = 0, pad = 0),
        paper_bgcolor = 'transparent', 
        plot_bgcolor = 'transparent'
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
  }, ignoreNULL = TRUE)
  cross_pheno_results <- reactiveVal(data.frame())
  
  # Marker Name search button logic
  observeEvent(input$search_marker_button, {
    req(input$marker_search)
    results <- call_api(
      endpoint = "marker",
      body_params = list(marker_name = input$marker_search)
    )
    if (is.data.frame(results) && !"LOG10P" %in% names(results) && "P_value" %in% names(results)) {
      results <- results %>% mutate(LOG10P = -log10(P_value))
    }
    if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
      results <- results %>%
        arrange(desc(LOG10P)) %>%
        select(StudyID, P_value, LOG10P, everything(), -Chromosome, -Position, -Direction, -FreqSE, -MinFreq, -MaxFreq, -HetISq, -HetChiSq, -HetDf)
    }
    cross_pheno_results(results)
  })
  output$cross_pheno_table <- DT::renderDataTable({
    datatable(cross_pheno_results(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Position search button logic
  observeEvent(input$search_snp_button, {
    req(input$pos_search)
    results <- call_api(
      endpoint = "phewas",
      body_params = list(chromosome = input$chr_search, position = input$pos_search)
    )
    if (is.data.frame(results) && !"LOG10P" %in% names(results) && "P_value" %in% names(results)) {
      results <- results %>% mutate(LOG10P = -log10(P_value))
    }
    if (is.data.frame(results) && nrow(results) > 0 && !"Status" %in% names(results)) {
      results <- results %>%
        arrange(desc(LOG10P)) %>%
        select(StudyID, P_value, LOG10P, everything(), -Chromosome, -Position, -Direction, -FreqSE, -MinFreq, -MaxFreq, -HetISq, -HetChiSq, -HetDf)
    }
    cross_pheno_results(results)
  })
  
  
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
 
  output$qq_plot <- renderImage({
    req(input$study_selector)
    list(src = file.path("www", paste0("qq_", input$study_selector, ".png")), contentType = 'image/png', width = "100%")
  }, deleteFile = FALSE)
  
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
