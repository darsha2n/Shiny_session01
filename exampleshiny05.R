# Full-featured Shiny app with map input, pie charts, and export options

library(shiny)
library(readxl)
library(smwrGraphs)
library(smwrBase)
library(leaflet)
library(ggplot2)
library(ggpubr)
library(sf)
library(ggsave)

ui <- fluidPage(
  titlePanel("Groundwater Analysis - Piper Plot with Map & Charts"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload Groundwater Data (.xlsx or .csv)",
                accept = c(".xlsx", ".csv")),
      checkboxInput("showMap", "Show Map (requires lat/lon)", value = TRUE),
      checkboxInput("showPie", "Show Pie Charts per Sample", value = TRUE),
      downloadButton("downloadPlot", "Download Piper Plot (PDF)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Piper Plot", plotOutput("piper")),
        tabPanel("Pie Charts", uiOutput("pies")),
        tabPanel("Map", leafletOutput("map", height = 500)),
        tabPanel("Summary", verbatimTextOutput("summary"))
      )
    )
  )
)

server <- function(input, output) {
  
  dataInput <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$name)
    
    df <- switch(ext,
                 csv = read.csv(input$file$datapath),
                 xlsx = read_excel(input$file$datapath),
                 stop("Invalid file format. Upload .csv or .xlsx"))
    
    required <- c("Calcium", "Magnesium", "Sodium", "Chloride", "Sulfate", "Bicarbonate")
    if (!all(required %in% colnames(df)))
      stop("Missing columns: Calcium, Magnesium, Sodium, Chloride, Sulfate, Bicarbonate")
    
    df$Ca.meq <- conc2meq(df$Calcium, "calcium")
    df$Mg.meq <- conc2meq(df$Magnesium, "magnesium")
    df$Na.meq <- conc2meq(df$Sodium, "sodium")
    df$Cl.meq <- conc2meq(df$Chloride, "chloride")
    df$SO4.meq <- conc2meq(df$Sulfate, "sulfate")
    df$HCO3.meq <- conc2meq(df$Bicarbonate, "bicarb")
    
    df$Sample <- if ("Sample" %in% names(df)) df$Sample else paste0("GW", seq_len(nrow(df)))
    df
  })
  
  output$piper <- renderPlot({
    df <- dataInput()
    piperPlot(df$Ca.meq, df$Mg.meq, df$Na.meq,
              df$Cl.meq, df$HCO3.meq, df$SO4.meq,
              Plot = list(name = df$Sample, color = setColor(df$Sample)))
  })
  
  output$summary <- renderPrint({
    df <- dataInput()
    summary(df[, c("Ca.meq", "Mg.meq", "Na.meq", "Cl.meq", "SO4.meq", "HCO3.meq")])
  })
  
  output$pies <- renderUI({
    req(input$showPie)
    df <- dataInput()
    plot_list <- lapply(1:nrow(df), function(i) {
      dat <- data.frame(
        Ion = c("Ca", "Mg", "Na", "Cl", "SO4", "HCO3"),
        meq = as.numeric(df[i, c("Ca.meq", "Mg.meq", "Na.meq", "Cl.meq", "SO4.meq", "HCO3.meq")])
      )
      gg <- ggplot(dat, aes(x = "", y = meq, fill = Ion)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        ggtitle(df$Sample[i]) +
        theme_void()
      plotOutput(outputId = paste0("pie", i), height = 300)
    })
    do.call(tagList, plot_list)
  })
  
  observe({
    req(input$showPie)
    df <- dataInput()
    for (i in 1:nrow(df)) {
      local({
        my_i <- i
        output[[paste0("pie", my_i)]] <- renderPlot({
          dat <- data.frame(
            Ion = c("Ca", "Mg", "Na", "Cl", "SO4", "HCO3"),
            meq = as.numeric(df[my_i, c("Ca.meq", "Mg.meq", "Na.meq", "Cl.meq", "SO4.meq", "HCO3.meq")])
          )
          ggplot(dat, aes(x = "", y = meq, fill = Ion)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y") +
            ggtitle(df$Sample[my_i]) +
            theme_void()
        })
      })
    }
  })
  
  output$map <- renderLeaflet({
    req(input$showMap)
    df <- dataInput()
    req("Latitude" %in% colnames(df), "Longitude" %in% colnames(df))
    
    leaflet(df) %>%
      addTiles() %>%
      addCircleMarkers(~Longitude, ~Latitude, label = ~Sample,
                       color = "blue", radius = 5, fillOpacity = 0.7)
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { "piper_plot.pdf" },
    content = function(file) {
      pdf(file)
      df <- dataInput()
      piperPlot(df$Ca.meq, df$Mg.meq, df$Na.meq,
                df$Cl.meq, df$HCO3.meq, df$SO4.meq,
                Plot = list(name = df$Sample, color = setColor(df$Sample)))
      dev.off()
    }
  )
}

shinyApp(ui, server)

