library(shiny)
library(tidyverse)
library(janitor)

ui <- fluidPage(
  h1("Property Stolen and Recovered Data"),
  hr(),
  
  # Table Output
  h3("Raw Data (First few rows)"),
  tableOutput("tb1"),
  hr(),
  
  # Plot 1: Trend over Years
  h3("Trend of Total Value of Property Stolen Over Years"),
  plotOutput("plot_yearly_trend"),
  hr(),
  
  # Plot 2: By State/UT
  h3("Total Value of Property Stolen by State/UT (Aggregated)"),
  plotOutput("plot_by_state")
)

server <- function(input, output, session) {
  
  # Data cleaning remains the same
  data_cleaned <- reactive({
    "10_Property_stolen_and_recovered.csv" %>%
      read_csv() %>%
      clean_names() %>%
      rename(state_ut = area_name)
  })
  
  # Render the table (displaying only a few rows for clarity)
  output$tb1 <- renderTable({
    data_cleaned() %>%
      head(10) # Displaying only the first 10 rows
  })
  
  # Render Plot 1: Trend of Total Value of Property Stolen Over Years
  output$plot_yearly_trend <- renderPlot({
    plot_data_yearly <- data_cleaned() %>%
      group_by(year) %>%
      summarise(total_value_stolen = sum(value_of_property_stolen, na.rm = TRUE)) %>%
      ungroup()
    
    ggplot(plot_data_yearly, aes(x = year, y = total_value_stolen)) +
      geom_line(color = "darkblue", size = 1.2) +
      geom_point(color = "darkblue", size = 2.5) +
      labs(
        title = "Total Value of Property Stolen Across India (Yearly Trend)",
        x = "Year",
        y = "Total Value of Property Stolen (in Rs.)" # Adjust unit if known
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  # Render Plot 2: Total Value of Property Stolen by State/UT (Aggregated)
  output$plot_by_state <- renderPlot({
    plot_data_state <- data_cleaned() %>%
      group_by(state_ut) %>%
      summarise(total_value_stolen = sum(value_of_property_stolen, na.rm = TRUE)) %>%
      ungroup() %>%
      # Filter out 'Total' if it's a summary row in your data
      filter(state_ut != "Total" & state_ut != "TOTAL") %>%
      arrange(desc(total_value_stolen)) %>%
      head(20) # Showing top 20 states for better readability
    
    ggplot(plot_data_state, aes(x = reorder(state_ut, total_value_stolen), y = total_value_stolen)) +
      geom_col(fill = "forestgreen") +
      coord_flip() + # Flips coordinates for better readability
      labs(
        title = "Top 20 States/UTs by Total Value of Property Stolen",
        x = "State/UT",
        y = "Total Value of Property Stolen (in Rs.)" # Adjust unit if known
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
  })
}

shinyApp(ui, server)