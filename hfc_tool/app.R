#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(shiny)

# Function to calculate HFC totals from user input
calculate_hfc_totals <- function(user_input, categories) {
  # Process each category separately
  results_list <- lapply(categories, function(cat) {
    # Filter for this category
    cat_data <- user_input %>%
      filter(category == cat)
    
    # Break down mixtures into components
    mixture_breakdown <- cat_data %>%
      filter(type == "mixture") %>%
      left_join(mixture_compositions, by = c("hfc" = "mixture")) %>%
      mutate(component_quantity = fraction * quantity) %>%
      group_by(year, component) %>%
      summarize(total_from_mixture = sum(component_quantity, na.rm = TRUE), .groups = "drop")
    
    # Get component quantities
    component_data <- cat_data %>%
      filter(type == "component") %>%
      select(year, hfc, quantity) %>%
      rename(component = hfc, total_from_component = quantity)
    
    # Combine
    result <- mixture_breakdown %>%
      full_join(component_data, by = c("year", "component")) %>%
      mutate(
        total_from_mixture = replace_na(total_from_mixture, 0),
        total_from_component = replace_na(total_from_component, 0),
        total = total_from_mixture + total_from_component
      ) %>%
      select(year, component, total) %>%
      rename(!!cat := total)
    
    return(result)
  })
  
  # Merge all categories together
  final_result <- results_list[[1]]
  if (length(results_list) > 1) {
    for (i in 2:length(results_list)) {
      final_result <- full_join(final_result, results_list[[i]], by = c("year", "component"))
    }
  }
  
  # Replace NA with 0 and arrange
  final_result <- final_result %>%
    mutate(across(all_of(categories), ~replace_na(., 0))) %>%
    arrange(year, component)
  
  return(final_result)
}

# UI
ui <- fluidPage(
  titlePanel("HFC Emissions Tool"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("country", 
                  "Select Country:", 
                  choices = sort(unique(edgar$name)),
                  selected = NULL),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        # Tab 1: Scoping Statement (combined view)
        tabPanel("Scoping Statement",
                 fluidRow(
                   column(12,
                          h4("Time Series Data"),
                          DT::DTOutput("timeseries_table")
                   )
                 ),
                 fluidRow(
                   column(12,
                          plotOutput("line_plot", height = "400px")
                   )
                 ),
                 fluidRow(
                   column(6,
                          plotOutput("treemap_app", height = "400px")
                   ),
                   column(6,
                          plotOutput("treemap_subapp", height = "400px")
                   )
                 )
        ),
        
        # Tab 2: Production & Destruction
        tabPanel("Production & Destruction",
                 br(),
                 fileInput("prod_file", "Upload Production & Destruction Data (CSV)",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 helpText("CSV format: year, hfc, type, category, quantity"),
                 helpText("Categories: produced, feedstock_produced, destroyed"),
                 br(),
                 tableOutput("prod_table")
        ),
        
        # Tab 3: Imports
        tabPanel("Imports",
                 br(),
                 fileInput("imports_file", "Upload Imports Data (CSV)",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 helpText("CSV format: year, hfc, type, category, quantity"),
                 helpText("Categories: new, recovered, feedstock"),
                 br(),
                 tableOutput("imports_table")
        ),
        
        # Tab 4: Exports
        tabPanel("Exports",
                 br(),
                 fileInput("exports_file", "Upload Exports Data (CSV)",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                 helpText("CSV format: year, hfc, type, category, quantity"),
                 helpText("Categories: new, recovered"),
                 br(),
                 tableOutput("exports_table")
        )
      ),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reactive data processing for scoping
  scoping_data <- reactive({
    req(input$country)
    
    edgar %>%
      left_join(applications, by = "sub_application") %>%
      filter(name == input$country) %>%
      mutate(
        co2_eq = gwp * hfc_emissions,
        year = as.numeric(as.character(year))
      ) %>%
      group_by(year, application, sub_application) %>%
      summarize(mmt_co2_eq = sum(co2_eq, na.rm = TRUE) / 10^6, .groups = "drop") %>%
      ungroup()
  })
  
  # Create consistent color palette for applications
  app_colors <- reactive({
    req(scoping_data())
    apps <- unique(scoping_data()$application)
    n_apps <- length(apps)
    colors <- viridis(n_apps, option = "D")
    names(colors) <- apps
    colors
  })
  
  # Scoping Statement Outputs
  output$timeseries_table <- DT::renderDT({
    scoping_data() %>%
      arrange(year, application, sub_application)
  })
  
  output$line_plot <- renderPlot({
    data_for_plot <- scoping_data() %>%
      group_by(year, application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq, na.rm = TRUE), .groups = "drop")
    
    ggplot(data_for_plot, aes(x = year, y = mmt_co2_eq, color = application, group = application)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2.5) +
      scale_color_manual(values = app_colors()) +
      labs(
        title = paste("HFC Emissions Over Time -", input$country),
        x = "Year",
        y = "Emissions (MMT CO₂ equivalent)",
        color = "Application"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank()
      )
  })
  
  output$treemap_app <- renderPlot({
    most_recent_year <- max(scoping_data()$year, na.rm = TRUE)
    
    data_for_treemap <- scoping_data() %>%
      filter(year == most_recent_year) %>%
      group_by(application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq, na.rm = TRUE), .groups = "drop") %>%
      filter(mmt_co2_eq > 0)
    
    ggplot(data_for_treemap, aes(area = mmt_co2_eq, fill = application, label = application)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", size = 12, grow = TRUE) +
      scale_fill_manual(values = app_colors()) +
      labs(
        title = paste("By Application (", most_recent_year, ")", sep = ""),
        fill = "Application"
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
      )
  })
  
  output$treemap_subapp <- renderPlot({
    most_recent_year <- max(scoping_data()$year, na.rm = TRUE)
    
    data_for_treemap <- scoping_data() %>%
      filter(year == most_recent_year) %>%
      group_by(application, sub_application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq, na.rm = TRUE), .groups = "drop") %>%
      filter(mmt_co2_eq > 0)
    
    ggplot(data_for_treemap, aes(area = mmt_co2_eq, fill = application, 
                                 label = sub_application, subgroup = application)) +
      geom_treemap() +
      geom_treemap_subgroup_border(colour = "white", size = 3) +
      geom_treemap_text(colour = "white", place = "centre", size = 10, grow = TRUE) +
      scale_fill_manual(values = app_colors()) +
      labs(
        title = paste("By Sub-Application (", most_recent_year, ")", sep = ""),
        fill = "Application"
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 12, hjust = 0.5)
      )
  })
  
  # Production & Destruction reactive
  prod_data <- reactive({
    req(input$prod_file)
    
    user_input <- read.csv(input$prod_file$datapath, stringsAsFactors = FALSE)
    
    # Validate categories
    expected_categories <- c("produced", "feedstock_produced", "destroyed")
    
    calculate_hfc_totals(user_input, expected_categories)
  })
  
  output$prod_table <- renderTable({
    prod_data()
  })
  
  # Imports reactive
  imports_data <- reactive({
    req(input$imports_file)
    
    user_input <- read.csv(input$imports_file$datapath, stringsAsFactors = FALSE)
    
    # Validate categories
    expected_categories <- c("new", "recovered", "feedstock")
    
    calculate_hfc_totals(user_input, expected_categories)
  })
  
  output$imports_table <- renderTable({
    imports_data()
  })
  
  # Exports reactive
  exports_data <- reactive({
    req(input$exports_file)
    
    user_input <- read.csv(input$exports_file$datapath, stringsAsFactors = FALSE)
    
    # Validate categories
    expected_categories <- c("new", "recovered")
    
    calculate_hfc_totals(user_input, expected_categories)
  })
  
  output$exports_table <- renderTable({
    exports_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
