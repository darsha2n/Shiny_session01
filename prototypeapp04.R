# Final - 21st april Load Libraries
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(leaflet)
library(dplyr)
library(magrittr)
library(ggplot2)
library(sf)
library(DT)
library(smwrGraphs)
library(smwrBase)
library(readr)
library(tidyr)
library(rmarkdown)
library(shinycssloaders)

# Custom color palette
iisc_palette <- c(
  primary = "#005A9C",
  secondary = "#00A86B",
  accent = "#4FC3F7",
  background = "#F0F4F8",
  text = "#2C3E50"
)

# Custom water type colors for consistency
water_type_colors <- c(
  "Permanent Hardness" = "red",
  "Temporary Hardness" = "blue",
  "Alkali Carbonates" = "green",
  "Saline" = "purple",
  "Mixed Type" = "orange",
  "Unable to compute" = "gray"
)


# WQI Calculation Function
calculate_wqi <- function(gpkg_path) {
  layer_name <- st_layers(gpkg_path)$name[1]
  water_sf <- st_read(gpkg_path, layer = layer_name, quiet = TRUE)

  # Ensure WGS84 projection
  if (sf::st_crs(water_sf)$epsg != 4326) {
    water_sf <- st_transform(water_sf, 4326)
  }

  # Validate geometries
  if (!all(st_is_valid(water_sf))) {
    water_sf <- st_make_valid(water_sf)
  }

  # Rename "NA" column to "Sodium" if present
  if ("NA" %in% names(water_sf)) {
    names(water_sf)[names(water_sf) == "NA"] <- "Sodium"
  }

  standards <- list(
    TDS = list(St = 1000, Wi = 0.121),
    EC = list(St = 2500, Wi = 0.121),
    NITRATE = list(St = 50, Wi = 0.152),
    SULPHATE = list(St = 250, Wi = 0.121),
    CHLORIDE = list(St = 250, Wi = 0.093),
    BICARBONATE = list(St = 500, Wi = 0.152),
    FLUORIDE = list(St = 1.2, Wi = 0.030),
    CA = list(St = 100, Wi = 0.060),
    MG = list(St = 50, Wi = 0.060),
    Sodium = list(St = 200, Wi = 0.060),
    K = list(St = 20, Wi = 0.030)
  )

  water_sf$WQI <- NA_real_
  param_names <- names(standards)

  # Replace missing parameters with 0
  for (param in param_names) {
    if (!param %in% names(water_sf)) {
      water_sf[[param]] <- 0
    } else {
      water_sf[[param]][is.na(water_sf[[param]])] <- 0
    }
  }

  for (param in param_names) {
    qi_col <- paste0("qi_", param)
    sli_col <- paste0("SLi_", param)
    water_sf[[qi_col]] <- water_sf[[param]] / standards[[param]]$St
    water_sf[[sli_col]] <- water_sf[[qi_col]] * standards[[param]]$Wi
  }

  sli_cols <- paste0("SLi_", param_names)
  sli_values <- st_drop_geometry(water_sf)[, sli_cols]
  sli_values[] <- lapply(sli_values, as.numeric)
  water_sf$WQI <- rowSums(sli_values, na.rm = TRUE)

  water_sf$Quality <- cut(
    water_sf$WQI,
    breaks = c(-Inf, 0.5, 1, 2, 3, Inf),
    labels = c("Excellent", "Good", "Poor", "Very Poor", "Unsuitable"),
    right = FALSE
  )

  # Write CSV output with WQI and Quality columns
  csv_path <- file.path(getwd(), "water_quality_output.csv")
  write.csv(st_drop_geometry(water_sf), csv_path, row.names = FALSE)

  return(water_sf)
}

# UI
ui <- dashboardPage(

  title = "Ground Water Assessment Dashboard",
  skin = "blue",

  dashboardHeader(
    title = tags$div(

      style = "display: flex; align-items: center; height: 60px; background-color: transparent;",
      tags$span("Ground Water Assessment Dashboard",
                style = "color: white; font-weight: bold; font-size: 18px; margin-left: 30px;")
    ),
    titleWidth = 650
  ),

  dashboardSidebar(
    sidebarMenu(
      menuItem("File Upload", tabName = "upload", icon = icon("upload")),
      menuItem("Data Exploration", tabName = "data_explore", icon = icon("magnifying-glass")),
      menuItem("Ground Water Chemistry", tabName = "chemistry", icon = icon("flask"))

    )
  ),

  dashboardBody(
    tags$head(
      tags$title("Ground Water Dashboard"),
      tags$style(HTML(sprintf('
      .skin-blue .main-header .logo { background-color: %s; color: white; }
      .skin-blue .main-header .navbar { background-color: %s; }
      body { background-color: %s; color: %s; }
      .box { border-top-color: %s; }
      .warning-message { color: %s; font-weight: bold; margin-top: 10px; }
      .water-type-box { border: 1px solid #ccc; padding: 15px; border-radius: 5px; background-color: #fff; }
    ', iisc_palette["primary"], iisc_palette["secondary"],
                   iisc_palette["background"], iisc_palette["text"],
                   iisc_palette["accent"], iisc_palette["accent"]))), # Added warning message color
      tags$style(HTML("
      .main-header {
        height: 60px !important; /* Adjust this value as needed */
      }
      .main-header .logo {
        height: 60px !important; /* Match the main-header height */
        line-height: 60px !important; /* Vertically center the logo text if any (though you have images) */
      }
      .main-header .navbar {
        min-height: 60px !important; /* Ensure navbar doesn't collapse */
      }
      .main-header .title {
        height: 60px !important; /* Match the main-header height */
        line-height: 60px !important; /* Vertically center the title text */
      }
      .main-header .title > div { /* Target the div containing your logos and text */
        display: flex;
        align-items: center; /* Vertically align items within the div */
        height: 100%; /* Ensure the div takes full height of the title */
      }
    "))
    ),

    tabItems(
      tabItem(tabName = "upload",
              fluidRow(
                box(title = "Upload GPKG File", status = "primary", solidHeader = TRUE,
                    fileInput("gpkg_upload", "Choose GPKG File", accept = c(".gpkg")),
                    actionButton("load_data", "Load Data", icon = icon("database"))
                ),
                box(title = "File Information", status = "success", solidHeader = TRUE,
                    verbatimTextOutput("file_info"))
              )
      ),

      tabItem(tabName = "data_explore",
              fluidRow(
                box(title = "Dataset Overview", status = "primary", solidHeader = TRUE, uiOutput("dataset_summary")),
                box(title = "Column Details", status = "success", solidHeader = TRUE, DTOutput("column_details"))
              ),
              fluidRow(
                box(title = "Data Preview", status = "warning", solidHeader = TRUE, DTOutput("data_preview")),
                box(title = "Data Statistics", status = "info", solidHeader = TRUE, uiOutput("data_statistics"))
              )
      ),

      tabItem(tabName = "chemistry",
              fluidRow(
                box(title = "Filter Data for Chemistry Analysis", status = "info", solidHeader = TRUE, width = 12,
                    fluidRow(
                      column(4, uiOutput("state_ui")),
                      column(4, uiOutput("district_ui")),
                      column(4, uiOutput("block_ui"))
                    ),
                    hr(), # Horizontal line for separation
                    fluidRow(
                      column(12, align = "center",
                             downloadButton("download_chemistry", "Download Chemistry Report (PDF)",
                                            style = "background-color: #005A9C; color: white; border-color: #005A9C; font-size: 16px; padding: 10px 20px; border-radius: 5px;")
                      )
                    )
                )
              ),
              fluidRow(
                box(title = h3("Piper Plot - Water Chemistry Facies", style = "color: #005A9C;"), status = "primary", solidHeader = TRUE, width = 12,
                    withSpinner(plotOutput("piper_plot", height = "700px"), type = 6)
                )
              ),
              fluidRow(
                box(title = h3("Water Type Distribution", style = "color: #005A9C;"), status = "warning", solidHeader = TRUE, width = 12,
                    withSpinner(uiOutput("water_types_section"), type = 6)
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  data_storage <- reactiveValues(csv_data = NULL, sf_data = NULL, water_sf = NULL, data_loaded = FALSE)

  observeEvent(input$load_data, {
    req(input$gpkg_upload)
    tryCatch({
      water_sf <- calculate_wqi(input$gpkg_upload$datapath)
      data_storage$csv_data <- st_drop_geometry(water_sf)
      data_storage$sf_data <- water_sf
      data_storage$water_sf <- water_sf
      data_storage$data_loaded <- TRUE

      # Update State choices immediately after data load
      updateSelectInput(session, "state_chem", choices = c("Select State", unique(data_storage$csv_data$STATE_UT)), selected = "Select State")
      updateSelectInput(session, "state_filter", choices = c("None", unique(data_storage$water_sf$STATE_UT)), selected = "None")

      # Reset subsequent filters
      updateSelectInput(session, "district_chem", choices = "Select District", selected = "Select District")
      updateSelectInput(session, "block_chem", choices = "Select Block", selected = "Select Block")
      updateSelectInput(session, "district_filter", choices = c("None"), selected = "None")
      updateSelectInput(session, "block_filter", choices = c("None"), selected = "None")

      showNotification("GPKG loaded and WQI calculated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error processing GPKG file:", e$message), type = "error")
      data_storage$data_loaded <- FALSE
    })
  })

  output$file_info <- renderPrint({
    req(data_storage$csv_data)
    cat("File Uploaded Successfully!\n",
        "Number of Rows:", nrow(data_storage$csv_data), "\n",
        "Number of Columns:", ncol(data_storage$csv_data))
  })

  output$dataset_summary <- renderUI({
    req(data_storage$csv_data)
    HTML(paste(
      "<h4>Dataset Characteristics</h4>",
      "<p><strong>Total Rows:</strong>", nrow(data_storage$csv_data), "</p>",
      "<p><strong>Total Columns:</strong>", ncol(data_storage$csv_data), "</p>"
    ))
  })

  output$column_details <- renderDT({
    req(data_storage$csv_data)
    column_info <- data.frame(
      Column = names(data_storage$csv_data),
      Type = sapply(data_storage$csv_data, function(x) class(x)[1]),
      Unique_Values = sapply(data_storage$csv_data, function(x) length(unique(x))),
      Missing_Values = sapply(data_storage$csv_data, function(x) sum(is.na(x)))
    )
    datatable(column_info, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$data_preview <- renderDT({
    req(data_storage$csv_data)
    datatable(data_storage$csv_data, options = list(pageLength = 10, scrollX = TRUE))
  })

  output$data_statistics <- renderUI({
    req(data_storage$csv_data)
    numeric_cols <- names(data_storage$csv_data)[sapply(data_storage$csv_data, is.numeric)]
    if (length(numeric_cols) > 0) {
      stats <- data_storage$csv_data %>%
        select(all_of(numeric_cols)) %>%
        summarise(across(everything(), list(Mean = mean, Median = median, Min = min, Max = max, SD = sd), na.rm = TRUE))
      HTML(paste("<h4>Numeric Column Statistics</h4><pre>", capture.output(print(stats)), "</pre>"))
    } else HTML("<p>No numeric columns found.</p>")
  })


  # Reactive UI for Filters
  output$state_ui <- renderUI({
    req(data_storage$csv_data)
    selectInput("state_chem", "State/UT:", choices = c("Select State", unique(data_storage$csv_data$STATE_UT)))
  })

  output$district_ui <- renderUI({
    req(data_storage$csv_data, input$state_chem)
    districts <- unique(data_storage$csv_data[data_storage$csv_data$STATE_UT == input$state_chem, ]$DISTRICT)
    selectInput("district_chem", "District:", choices = c("Select District", districts))
  })

  output$block_ui <- renderUI({
    req(data_storage$csv_data, input$district_chem)
    blocks <- unique(data_storage$csv_data[data_storage$csv_data$DISTRICT == input$district_chem, ]$BLOCK)
    selectInput("block_chem", "Block:", choices = c("Select Block", blocks))
  })


  # Chemistry Tab Server Logic
  PD_for_water_type_reactive <- reactive({

    req(data_storage$csv_data) # Make sure data is loaded
    state_val <- input$state_chem
    district_val <- input$district_chem
    block_val <- input$block_chem

    # Only proceed if all filters have a selected value (not "Select State/District/Block")
    if (is.null(state_val) || state_val == "Select State" ||
        is.null(district_val) || district_val == "Select District" ||
        is.null(block_val) || block_val == "Select Block") {
      return(NULL) # Return NULL if filters are not fully selected
    }

    selected_data <- data_storage$csv_data %>%
      filter(
        STATE_UT == state_val,
        DISTRICT == district_val,
        BLOCK == block_val
      ) %>%
      mutate(
        CA = as.numeric(CA),
        MG = as.numeric(MG),
        Sodium = as.numeric(Sodium), # Use "Sodium" as renamed
        CHLORIDE = as.numeric(CHLORIDE),
        SULPHATE = as.numeric(SULPHATE),
        BICARBONATE = as.numeric(BICARBONATE)
      )

    # Check for missing required ions (CA, MG, Sodium, CHLORIDE, SULPHATE, BICARBONATE)
    missing_ions <- character(0)
    if(any(is.na(selected_data$CA))) missing_ions <- c(missing_ions, "Calcium (Ca)")
    if(any(is.na(selected_data$MG))) missing_ions <- c(missing_ions, "Magnesium (Mg)")
    if(any(is.na(selected_data$Sodium))) missing_ions <- c(missing_ions, "Sodium (Na)") # Use "Sodium"
    if(any(is.na(selected_data$CHLORIDE))) missing_ions <- c(missing_ions, "Chloride (Cl)")
    if(any(is.na(selected_data$SULPHATE))) missing_ions <- c(missing_ions, "Sulfate (SO4)")
    if(any(is.na(selected_data$BICARBONATE))) missing_ions <- c(missing_ions, "Bicarbonate (HCO3)")

    if (length(missing_ions) > 0) {
      # Attach missing ions as an attribute
      attr(selected_data, "missing_ions") <- paste("Warning: Missing data for some essential ions:", paste(missing_ions, collapse = ", "))
      return(selected_data) # Return data even with missing values, but with warning
    } else {
      return(selected_data)
    }
  })

  PD_final_reactive <- reactive({
    data_to_process <- PD_for_water_type_reactive()
    req(data_to_process) # Ensure data_to_process is not NULL

    # Drop rows with NA in the required columns for calculation
    PD <- data_to_process %>%
      drop_na(CA, MG, Sodium, CHLORIDE, SULPHATE, BICARBONATE) # Use Sodium here

    if (nrow(PD) == 0) {
      return(NULL) # Return NULL if no valid rows remain after dropping NAs
    }

    PD <- transform(PD,
                    Ca.meq = conc2meq(CA, "calcium"),
                    Mg.meq = conc2meq(MG, "magnesium"),
                    Na.meq = conc2meq(Sodium, "sodium"), # Use Sodium
                    Cl.meq = conc2meq(CHLORIDE, "chloride"),
                    SO4.meq = conc2meq(SULPHATE, "sulfate"),
                    HCO3.meq = conc2meq(BICARBONATE, "bicarb"))
    PD$SS <- PD$SITE_NAME

    PD <- PD %>%
      mutate(
        total_cations = Ca.meq + Mg.meq + Na.meq,
        total_anions = Cl.meq + SO4.meq + HCO3.meq,
        Ca_pct = round(100 * Ca.meq / total_cations, 1),
        Mg_pct = round(100 * Mg.meq / total_cations, 1),
        Na_pct = round(100 * Na.meq / total_cations, 1),
        Cl_pct = round(100 * Cl.meq / total_anions, 1),
        SO4_pct = round(100 * SO4.meq / total_anions, 1),
        HCO3_pct = round(100 * HCO3.meq / total_anions, 1),
        cation_type = case_when(
          total_cations > 0 & Ca_pct + Mg_pct > 50 ~ "Hard",
          total_cations > 0 & Na_pct > 50 ~ "Alkali",
          total_cations > 0 ~ "Mixed",
          TRUE ~ NA_character_
        ),
        anion_type = case_when(
          total_anions > 0 & HCO3_pct > 50 ~ "Carbonate",
          total_anions > 0 & Cl_pct + SO4_pct > 50 ~ "Non-carbonate",
          total_anions > 0 ~ "Mixed",
          TRUE ~ NA_character_
        ),
        water_type = case_when(
          cation_type == "Hard" & anion_type == "Non-carbonate" ~ "Permanent Hardness",
          cation_type == "Hard" & anion_type == "Carbonate" ~ "Temporary Hardness",
          cation_type == "Alkali" & anion_type == "Carbonate" ~ "Alkali Carbonates",
          cation_type == "Alkali" & anion_type == "Non-carbonate" ~ "Saline",
          !is.na(cation_type) & !is.na(anion_type) ~ "Mixed Type",
          TRUE ~ "Unable to compute"
        )
      )

    return(PD)
  })

  output$piper_plot <- renderPlot({
    PD <- PD_final_reactive()

    # Check if filters are fully selected and data is available
    if (is.null(input$state_chem) || input$state_chem == "Select State" ||
        is.null(input$district_chem) || input$district_chem == "Select District" ||
        is.null(input$block_chem) || input$block_chem == "Select Block") {
      par(mar = c(0, 0, 0, 0)) # Set margins to 0
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "Please select State, District, and Block to view the Piper Plot.", cex = 1.2, col = iisc_palette["text"])
      return(NULL)
    }

    if (is.null(PD) || nrow(PD) == 0) {
      par(mar = c(0, 0, 0, 0)) # Set margins to 0
      plot(NA, xlim=c(0,1), ylim=c(0,1), xaxt='n', yaxt='n', xlab='', ylab='')
      text(0.5, 0.5, "No valid data available for the selected filters to generate the Piper Plot.\nEnsure all required ion values (Ca, Mg, Na, Cl, SO4, HCO3) are present.", cex = 1.2, col = iisc_palette["text"])
      return(NULL)
    }

    PD$color <- water_type_colors[PD$water_type]

    # Dynamically adjust point size and legend position based on number of points
    point_size <- ifelse(nrow(PD) < 50, 1.2, 0.8) # Larger points for fewer data points
    legend_pos <- ifelse(nrow(PD) < 50, "bottomleft", "topright") # Adjust legend position

    with(PD, piperPlot(
      Ca.meq, Mg.meq, Na.meq,
      Cl.meq, HCO3.meq, SO4.meq,
      Plot = list(name = water_type, color = color, cex = point_size), # Apply point size
      xAn.title = "Chloride" # Keep this title
    ))

    # Add main title using title() AFTER piperPlot
    title(main = paste("Piper Diagram for", input$block_chem, "Block,\n", input$district_chem, "District, ", input$state_chem),
          line = 3, cex.main = 1.5) # Adjust line and cex.main for positioning and size


    # Custom legend for better appearance
    legend(legend_pos,
           legend = names(water_type_colors),
           col = water_type_colors,
           pch = 16,
           title = "Water Chemistry Type",
           bty = "n", # No box around legend
           cex = 1.0, # Adjust legend text size
           pt.cex = point_size + 0.5 # Adjust legend point size
    )
  })


  output$water_types_section <- renderUI({
    # Display a message if filters are not fully selected
    if (is.null(input$state_chem) || input$state_chem == "Select State" ||
        is.null(input$district_chem) || input$district_chem == "Select District" ||
        is.null(input$block_chem) || input$block_chem == "Select Block") {
      return(HTML("<div class='water-type-box'><p class='warning-message'>Please select State, District, and Block to see the water type distribution.</p></div>"))
    }

    PD <- PD_final_reactive()
    if (is.null(PD) || nrow(PD) == 0) {
      return(HTML("<div class='water-type-box'><p class='warning-message'>No valid water chemistry data found for the selected filters. Please ensure all required ion values are present.</p></div>"))
    }

    # Calculate water type distribution
    water_type_dist <- PD %>%
      group_by(water_type) %>%
      summarise(Count = n()) %>%
      ungroup() %>%
      mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
      arrange(desc(Count))

    # Add color column for display
    water_type_dist$Color <- water_type_colors[water_type_dist$water_type]

    # Create HTML table with colored squares
    table_html <- paste0(
      "<table class='table table-striped'>",
      "<thead><tr><th>Water Type</th><th>Count</th><th>Percentage (%)</th><th>Color</th></tr></thead>",
      "<tbody>"
    )
    for (i in 1:nrow(water_type_dist)) {
      table_html <- paste0(table_html,
                           "<tr>",
                           "<td>", water_type_dist$water_type[i], "</td>",
                           "<td>", water_type_dist$Count[i], "</td>",
                           "<td>", water_type_dist$Percentage[i], "</td>",
                           "<td><span style='display: inline-block; width: 20px; height: 20px; background-color:", water_type_dist$Color[i], "; border: 1px solid #ccc; vertical-align: middle;'></span></td>",
                           "</tr>")
    }
    table_html <- paste0(table_html, "</tbody></table>")

    HTML(paste0(
      "<div class='water-type-box'>",
      "<h4>Summary of Water Chemistry Types:</h4>",
      table_html,
      "</div>"
    ))
  })


  # Water Chemistry download Report
  output$download_chemistry <- downloadHandler(
    filename = function() {
      paste("ground_water_chemistry_report_", input$state_chem, "_", input$district_chem, "_", input$block_chem, "_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "groundwater_chemistry_report.Rmd")
      # Make sure this Rmd file exists in your app directory
      # You'll need to create a simple R Markdown file for this to work
      file.copy("groundwater_chemistry_report.Rmd", tempReport, overwrite = TRUE)

      PD <- PD_final_reactive()
      req(PD)

      # Use the globally defined water_type_colors
      PD$color <- water_type_colors[PD$water_type]

      plot_file <- file.path(tempdir(), "piper_plot_for_report.png")
      png(filename = plot_file, width = 1200, height = 1000, res = 150, bg = "white", pointsize = 12)
      par(mar = c(5, 4, 4, 8)) # Adjust margins if necessary
      with(PD, piperPlot(
        Ca.meq, Mg.meq, Na.meq,
        Cl.meq, HCO3.meq, SO4.meq,
        Plot = list(name = water_type, color = color),
        xAn.title = "Chloride"
      ))
      # Add title for the downloaded plot as well
      title(main = paste("Piper Diagram for", input$block_chem, "Block,\n", input$district_chem, "District, ", input$state_chem),
            line = 3, cex.main = 1.5)

      legend(
        x = 0.6, y = 1, # Adjust legend position
        legend = names(water_type_colors),
        col = water_type_colors,
        pch = 16,
        title = "Water Chemistry Type",
        xpd = TRUE, # Allow legend to be drawn outside plot area
        bty = "n"
      )
      dev.off()

      # Prepare water type distribution data for the report
      water_type_dist <- PD %>%
        group_by(water_type) %>%
        summarise(Count = n()) %>%
        ungroup() %>%
        mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
        arrange(desc(Count)) %>%
        mutate(Color = water_type_colors[water_type]) # Add color column

      params_list <- list(
        data = PD,
        state = input$state_chem,
        district = input$district_chem,
        block = input$block_chem,
        image_path = plot_file,
        water_type_distribution = water_type_dist # Pass this to the Rmd
      )

      rmarkdown::render(
        tempReport,
        output_file = file,
        params = params_list,
        envir = new.env(parent = globalenv())
      )
    }
  )

}

# Run App
shinyApp(ui, server)