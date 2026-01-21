library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(treemapify)
library(viridis)
library(DT)
library(rhandsontable)

# Convert year to numeric
edgar <- edgar %>%
  mutate(year = as.numeric(as.character(year)))

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
                 br(),
                 fluidRow(
                   column(12,
                          plotOutput("line_plot", height = "400px")
                   )
                 ),
                 br(),
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
                 h4("Enter Production & Destruction Data"),
                 helpText("Enter data in the table below. Right-click for options to add/remove rows."),
                 rHandsontableOutput("prod_input_table"),
                 br(),
                 actionButton("calc_prod", "Calculate Totals", class = "btn-primary"),
                 br(), br(),
                 h4("Calculated Totals"),
                 tableOutput("prod_table")
        ),
        
        # Tab 3: Imports
        tabPanel("Imports",
                 br(),
                 h4("Enter Imports Data"),
                 helpText("Enter data in the table below. Right-click for options to add/remove rows."),
                 rHandsontableOutput("imports_input_table"),
                 br(),
                 actionButton("calc_imports", "Calculate Totals", class = "btn-primary"),
                 br(), br(),
                 h4("Calculated Totals"),
                 tableOutput("imports_table")
        ),
        
        # Tab 4: Exports
        tabPanel("Exports",
                 br(),
                 h4("Enter Exports Data"),
                 helpText("Enter data in the table below. Right-click for options to add/remove rows."),
                 rHandsontableOutput("exports_input_table"),
                 br(),
                 actionButton("calc_exports", "Calculate Totals", class = "btn-primary"),
                 br(), br(),
                 h4("Calculated Totals"),
                 tableOutput("exports_table")
        )
      ),
      width = 9
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Create initial empty dataframes for input tables (without type column)
  initial_prod <- data.frame(
    year = rep(2020L, 5),
    hfc = rep("", 5),
    category = rep("produced", 5),
    quantity = rep(0, 5),
    stringsAsFactors = FALSE
  )
  
  initial_imports <- data.frame(
    year = rep(2020L, 5),
    hfc = rep("", 5),
    category = rep("new", 5),
    quantity = rep(0, 5),
    stringsAsFactors = FALSE
  )
  
  initial_exports <- data.frame(
    year = rep(2020L, 5),
    hfc = rep("", 5),
    category = rep("new", 5),
    quantity = rep(0, 5),
    stringsAsFactors = FALSE
  )
  
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
    colors <- viridis(n_apps, option = "mako", begin = 0, end = 0.8)
    names(colors) <- apps
    colors
  })
  
  # Scoping Statement Outputs
  output$timeseries_table <- DT::renderDT({
    scoping_data() %>%
      arrange(year, application, sub_application) %>%
      mutate(
        year = as.integer(year),
        mmt_co2_eq = round(mmt_co2_eq, 4)
      )
  }, 
  server = FALSE,
  extensions = 'Buttons',
  options = list(
    dom = 'Bfrtip',
    buttons = list(
      list(extend = 'copy', exportOptions = list(modifier = list(page = 'all'))),
      list(extend = 'csv', exportOptions = list(modifier = list(page = 'all')))
    )
  ))
  
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
      geom_treemap_text(colour = "white", place = "centre", size = 12, grow = TRUE, reflow = TRUE) +
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
  
  # Production & Destruction
  output$prod_input_table <- renderRHandsontable({
    if (!is.null(input$prod_input_table)) {
      df <- hot_to_r(input$prod_input_table)
    } else {
      df <- initial_prod
    }
    
    # Ensure year is integer
    df$year <- as.integer(df$year)
    
    rhandsontable(df, rowHeaders = NULL) %>%
      hot_col("year", type = "numeric", format = "0", colWidths = 75) %>%
      hot_col("hfc", type = "dropdown", , colWidths = 225,
              source = c("", unique(c(mixture_compositions$component, mixture_compositions$mixture)))) %>%
      hot_col("category", type = "dropdown", , colWidths = 150,
              source = c("produced", "feedstock_produced", "destroyed")) %>%
      hot_col("quantity", type = "numeric", , colWidths = 75) %>%
      hot_table(highlightRow = TRUE, highlightCol = TRUE)
  })
  
  prod_data <- eventReactive(input$calc_prod, {
    req(input$prod_input_table)
    user_input <- hot_to_r(input$prod_input_table)
    user_input <- user_input[user_input$hfc != "" & !is.na(user_input$quantity), ]
    
    if (nrow(user_input) == 0) {
      return(data.frame(year = integer(), component = character(), 
                        produced = numeric(), feedstock_produced = numeric(), 
                        destroyed = numeric()))
    }
    
    # Add type column based on HFC
    user_input <- user_input %>%
      mutate(type = ifelse(hfc %in% mixture_compositions$mixture, "mixture", "component"))
    
    expected_categories <- c("produced", "feedstock_produced", "destroyed")
    calculate_hfc_totals(user_input, expected_categories)
  })
  
  output$prod_table <- renderTable({
    prod_data()
  })
  
  # Imports
  output$imports_input_table <- renderRHandsontable({
    if (!is.null(input$imports_input_table)) {
      df <- hot_to_r(input$imports_input_table)
    } else {
      df <- initial_imports
    }
    
    # Ensure year is integer
    df$year <- as.integer(df$year)
    
    rhandsontable(df, rowHeaders = NULL) %>%
      hot_col("year", type = "numeric", format = "0", colWidths = 75) %>%
      hot_col("hfc", type = "dropdown", colWidths = 225,
              source = c("", unique(c(mixture_compositions$component, mixture_compositions$mixture)))) %>%
      hot_col("category", type = "dropdown", colWidths = 150,
              source = c("new", "recovered", "feedstock")) %>%
      hot_col("quantity", type = "numeric", colWidths = 75) %>%
      hot_table(highlightRow = TRUE, highlightCol = TRUE)
  })
  
  imports_data <- eventReactive(input$calc_imports, {
    req(input$imports_input_table)
    user_input <- hot_to_r(input$imports_input_table)
    user_input <- user_input[user_input$hfc != "" & !is.na(user_input$quantity), ]
    
    if (nrow(user_input) == 0) {
      return(data.frame(year = integer(), component = character(), 
                        new = numeric(), recovered = numeric(), feedstock = numeric()))
    }
    
    # Add type column based on HFC
    user_input <- user_input %>%
      mutate(type = ifelse(hfc %in% mixture_compositions$mixture, "mixture", "component"))
    
    expected_categories <- c("new", "recovered", "feedstock")
    calculate_hfc_totals(user_input, expected_categories)
  })
  
  output$imports_table <- renderTable({
    imports_data()
  })
  
  # Exports
  output$exports_input_table <- renderRHandsontable({
    if (!is.null(input$exports_input_table)) {
      df <- hot_to_r(input$exports_input_table)
    } else {
      df <- initial_exports
    }
    
    # Ensure year is integer
    df$year <- as.integer(df$year)
    
    rhandsontable(df, rowHeaders = NULL) %>%
      hot_col("year", type = "numeric", format = "0", colWidths = 75) %>%
      hot_col("hfc", type = "dropdown", colWidths = 225,
              source = c("", unique(c(mixture_compositions$component, mixture_compositions$mixture)))) %>%
      hot_col("category", type = "dropdown", , colWidths = 150,
              source = c("new", "recovered")) %>%
      hot_col("quantity", type = "numeric", colWidths = 75) %>%
    hot_table(highlightRow = TRUE, highlightCol = TRUE)
  })
  
  exports_data <- eventReactive(input$calc_exports, {
    req(input$exports_input_table)
    user_input <- hot_to_r(input$exports_input_table)
    user_input <- user_input[user_input$hfc != "" & !is.na(user_input$quantity), ]
    
    if (nrow(user_input) == 0) {
      return(data.frame(year = integer(), component = character(), 
                        new = numeric(), recovered = numeric()))
    }
    
    # Add type column based on HFC
    user_input <- user_input %>%
      mutate(type = ifelse(hfc %in% mixture_compositions$mixture, "mixture", "component"))
    
    expected_categories <- c("new", "recovered")
    calculate_hfc_totals(user_input, expected_categories)
  })
  
  output$exports_table <- renderTable({
    exports_data()
  })
}

# Run the app
shinyApp(ui = ui, server = server)