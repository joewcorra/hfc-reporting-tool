library(shiny)
library(pins)
library(tidyverse)
library(treemapify)
library(sunburstR)
library(viridis)
library(DT)
library(rhandsontable)

# Convert year to numeric
edgar <- edgar %>%
  mutate(year = as.numeric(as.character(year)))

# Category choices by flow type
category_choices <- list(
  imports     = c("new", "recovered", "feedstock"),
  exports     = c("new", "recovered"),
  production  = c("produced", "feedstock_produced", "destroyed")
)

# Function to calculate HFC totals from user input
calculate_hfc_totals <- function(user_input, flow_col = "flow") {
  # We'll group by year, component, flow, category, application
  user_input %>%
    mutate(type = ifelse(hfc %in% mixture_compositions$mixture, "mixture", "component")) %>%
    { bind_rows(
      # Expand mixtures
      filter(., type == "mixture") %>%
        left_join(mixture_compositions, by = c("hfc" = "mixture")) %>%
        mutate(quantity = fraction * quantity) %>%
        select(year, component, quantity, flow, category, application),
      # Keep pure components
      filter(., type == "component") %>%
        rename(component = hfc) %>%
        select(year, component, quantity, flow, category, application)
    )
    } %>%
    group_by(year, component, flow, category, application) %>%
    summarize(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
    arrange(year, flow, component)
}

# UI--------------------------------------
ui <- fluidPage(
  titlePanel("HFC Emissions Tool"),
  
  # JavaScript to handle contingent category dropdowns
  tags$head(tags$script(HTML("
    // Map of flow -> allowed categories
    var categoryMap = {
      'imports':    ['new', 'recovered', 'feedstock'],
      'exports':    ['new', 'recovered'],
      'production': ['produced', 'feedstock_produced', 'destroyed']
    };

    // Column indices (0-based): flow=3, category=4
    var FLOW_COL = 3;
    var CAT_COL  = 4;

    function updateCategoryDropdown(hot, row) {
      var flowVal = hot.getDataAtCell(row, FLOW_COL);
      var allowed = categoryMap[flowVal] || [];
      
      // Get current category value; clear if no longer valid
      var curCat = hot.getDataAtCell(row, CAT_COL);
      if (allowed.length > 0 && !allowed.includes(curCat)) {
        hot.setDataAtCell(row, CAT_COL, allowed[0]);
      }

      // Update the column meta for this cell to restrict dropdown
      // We use cell-level meta overrides
      hot.setCellMeta(row, CAT_COL, 'source', allowed);
      hot.render();
    }

    // Hook into rhandsontable after it renders
    $(document).on('shiny:value', function(e) {
      if (e.name !== 'kigali_input_table') return;
      
      setTimeout(function() {
        var container = document.getElementById('kigali_input_table');
        if (!container) return;
        var hot = container.querySelector('.handsontable');
        if (!hot || !hot.hotInstance) return;
        var htInstance = hot.hotInstance;

        // Apply to all existing rows
        var nrows = htInstance.countRows();
        for (var r = 0; r < nrows; r++) {
          updateCategoryDropdown(htInstance, r);
        }

        // Listen for changes
        htInstance.addHook('afterChange', function(changes, source) {
          if (!changes) return;
          changes.forEach(function(change) {
            var row = change[0], col = change[1];
            if (col === FLOW_COL) {
              updateCategoryDropdown(htInstance, row);
            }
          });
        });
      }, 300);
    });
  "))),
  
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
        
        # Tab 1: Scoping Statement
        tabPanel("Scoping Statement",
                 fluidRow(column(12, h4("Time Series Data"), DT::DTOutput("timeseries_table"))),
                 br(),
                 fluidRow(column(12, plotOutput("line_plot", height = "400px"))),
                 br(),
                 fluidRow(
                   column(6, plotOutput("treemap_app", height = "400px")),
                   column(6, sunburstOutput("sunburst_subapp", height = "400px"))
                 )
        ),
        
        # Tab 2: Kigali Data (single consolidated tab)
        tabPanel("Kigali Data",
                 br(),
                 h4("Enter Kigali Reporting Data"),
                 helpText("Enter all production, import, and export data below. 
                           Right-click for options to add/remove rows. 
                           Category options update automatically based on the selected Flow."),
                 rHandsontableOutput("kigali_input_table"),
                 br(),
                 actionButton("calc_kigali", "Calculate Totals", class = "btn-primary"),
                 br(), br(),
                 h4("Calculated Totals"),
                 DTOutput("kigali_totals_table")
        ),
        
        # Tab 3: Emissions
        tabPanel("Emissions",
                 fluidRow(
                   column(width = 12,
                          h3("HFC Emissions Estimates"),
                          actionButton("calculate_emissions", "Calculate Emissions", 
                                       class = "btn-primary"),
                          br(), br(),
                          DTOutput("emissions_table")
                   )
                 )
        )
        
      ),
      width = 9
    )
  )
)

# Server----------------------------
server <- function(input, output, session) {
  
  # Initial empty dataframe for Kigali input
  initial_kigali <- data.frame(
    year        = rep(2020L, 5),
    hfc         = rep("", 5),
    application = rep("Refrigeration and Air Conditioning", 5),
    flow        = rep("imports", 5),
    category    = rep("new", 5),
    quantity    = rep(0, 5),
    stringsAsFactors = FALSE
  )
  
  # ── Scoping data ──────────────────────────────────────────────────────────
  scoping_data <- reactive({
    req(input$country)
    edgar %>%
      left_join(applications, by = "sub_application") %>%
      filter(name == input$country) %>%
      mutate(co2_eq = gwp * hfc_emissions,
             year   = as.numeric(as.character(year))) %>%
      group_by(year, application, sub_application) %>%
      summarize(mmt_co2_eq = sum(co2_eq, na.rm = TRUE) / 10^6, .groups = "drop") %>%
      ungroup()
  })
  
  app_colors <- reactive({
    req(scoping_data())
    apps   <- unique(scoping_data()$application)
    colors <- viridis(length(apps), option = "mako", begin = 0, end = 0.8)
    names(colors) <- apps
    colors
  })
  
  # ── Scoping outputs ───────────────────────────────────────────────────────
  output$timeseries_table <- DT::renderDT({
    scoping_data() %>%
      arrange(year, application, sub_application) %>%
      mutate(year = as.integer(year), mmt_co2_eq = round(mmt_co2_eq, 4))
  },
  server = FALSE,
  extensions = "Buttons",
  options = list(
    dom     = "Bfrtip",
    buttons = list(
      list(extend = "copy", exportOptions = list(modifier = list(page = "all"))),
      list(extend = "csv",  exportOptions = list(modifier = list(page = "all")))
    )
  ))
  
  output$line_plot <- renderPlot({
    scoping_data() %>%
      group_by(year, application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq), .groups = "drop") %>%
      ggplot(aes(year, mmt_co2_eq, color = application, group = application)) +
      geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
      scale_color_manual(values = app_colors()) +
      labs(title  = paste("HFC Emissions Over Time -", input$country),
           x = "Year", y = "Emissions (MMT CO₂ equivalent)", color = "Application") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank(),
            plot.title = element_text(face = "bold", size = 14))
  })
  
  output$treemap_app <- renderPlot({
    most_recent_year <- max(scoping_data()$year, na.rm = TRUE)
    scoping_data() %>%
      filter(year == most_recent_year) %>%
      group_by(application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq), .groups = "drop") %>%
      filter(mmt_co2_eq > 0) %>%
      ggplot(aes(area = mmt_co2_eq, fill = application, label = application)) +
      geom_treemap() +
      geom_treemap_text(colour = "white", place = "centre", size = 12, grow = TRUE, reflow = TRUE) +
      scale_fill_manual(values = app_colors()) +
      labs(title = paste("By Application (", most_recent_year, ")", sep = "")) +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  })
  
  output$sunburst_subapp <- renderSunburst({
    most_recent_year <- max(scoping_data()$year, na.rm = TRUE)
    data_for_sunburst <- scoping_data() %>%
      filter(year == most_recent_year) %>%
      mutate(application    = str_remove_all(application, "-"),
             sub_application = str_remove_all(sub_application, "-")) %>%
      group_by(application, sub_application) %>%
      summarize(mmt_co2_eq = sum(mmt_co2_eq), .groups = "drop") %>%
      filter(mmt_co2_eq > 0) %>%
      mutate(path = paste(application, sub_application, sep = "-")) %>%
      select(path, mmt_co2_eq)
    sunburst(data_for_sunburst,
             legend  = list(w = 150, h = 25, s = 5, t = 25),
             colors  = list(range = unname(app_colors())),
             withD3  = TRUE)
  })
  
  # ── Kigali data entry ─────────────────────────────────────────────────────
  output$kigali_input_table <- renderRHandsontable({
    df <- if (!is.null(input$kigali_input_table)) {
      hot_to_r(input$kigali_input_table)
    } else {
      initial_kigali
    }
    df$year <- as.integer(df$year)
    
    rhandsontable(df, rowHeaders = NULL) %>%
      hot_col("year",        type = "numeric",  format = "0", colWidths = 65) %>%
      hot_col("hfc",         type = "dropdown",  colWidths = 220,
              source = c("", unique(c(mixture_compositions$component,
                                      mixture_compositions$mixture)))) %>%
      hot_col("application", type = "dropdown",  colWidths = 240,
              source = c("Refrigeration and Air Conditioning")) %>%
      hot_col("flow",        type = "dropdown",  colWidths = 110,
              source = c("imports", "exports", "production")) %>%
      hot_col("category",    type = "dropdown",  colWidths = 160,
              # Full list here; JS narrows it live based on flow selection
              source = c("new", "recovered", "feedstock",
                         "produced", "feedstock_produced", "destroyed")) %>%
      hot_col("quantity",    type = "numeric",   colWidths = 90) %>%
      hot_table(highlightRow = TRUE, highlightCol = TRUE)
  })
  
  kigali_data <- eventReactive(input$calc_kigali, {
    req(input$kigali_input_table)
    df <- hot_to_r(input$kigali_input_table) %>%
      filter(hfc != "", !is.na(quantity), quantity != 0)
    
    if (nrow(df) == 0) {
      return(data.frame(year = integer(), component = character(),
                        flow = character(), category = character(),
                        application = character(), quantity = numeric()))
    }
    
    calculate_hfc_totals(df)
  })
  
  output$kigali_totals_table <- renderDT({
    kigali_data() %>%
      mutate(quantity = round(quantity, 4))
  },
  extensions = "Buttons",
  options = list(
    dom        = "Bfrtip",
    buttons    = c("copy", "csv"),
    pageLength = 25,
    scrollX    = TRUE
  ),
  rownames = FALSE)

  # ── Emissions ─────────────────────────────────────────────────────────────
  output$emissions_table <- renderDT({
    req(input$calculate_emissions)
    datatable(
      hfc_data_complete %>%
        select(year, component, total_emissions, 
               total_emissions_lower, total_emissions_upper),
      options   = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                       pageLength = 25, scrollX = TRUE),
      extensions = "Buttons",
      rownames  = FALSE
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)