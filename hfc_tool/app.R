library(shiny)
library(pins)
library(bslib)
library(tidyverse)
library(treemapify)
library(sunburstR)
library(viridis)
library(DT)
library(rhandsontable)

# App Setup-----------------------------------------------------
## Get Default Data----------------------------------------

# Read data from pins board
hfc_board <- board_folder("data/pins")
# We should have a master list of HFCs....fix this later
edgar <- pin_read(hfc_board, "edgar")
velders <- pin_read(hfc_board, "velders")
uncertainty <- pin_read(hfc_board, "uncertainty")
mixture_compositions <- pin_read(hfc_board, "mixture_compositions")
applications <- pin_read(hfc_board, "applications")
hfc_defaults <- pin_read(hfc_board, "hfc_defaults")

## Functions: Kigali Data Emissions---------------------

# Initial values
init_bank_start <- 0
init_bank_end <- 0
init_bank_contained_history <- rep(0, 10)

# This function computes proportional HFC consumption by application based on
# assumptions derived from Velders et al. 
get_abs_hfc_consumption <- function(scoping_data, velders) {
  abs_hfc_consumption <- scoping_data %>%
    mutate(velders_application = case_when(
      sub_application == "domestic refrigeration" ~ "domestic refrigeration", 
      sub_application %in% c("commercial refrigeration",
                             "industrial refrigeration", 
                             "transport refrigeration") ~ "commercial and industrial refrigeration",
      sub_application == "mobile air conditioning" ~ "mobile air conditioning",
      sub_application == "stationary air conditioning" ~ "stationary air conditioning",
      sub_application == "aerosols" ~ "aerosols",
      sub_application == "closed cell foam" ~ "closed cell foam",
      sub_application == "open cell foam" ~ "open cell foam",
      sub_application %in% c("solvents", "f-gas as solvent") ~ "solvents",
      sub_application %in% c("semiconductor/electronics manufacture", 
                             "production of halocarbons and sf6", 
                             "electrical equipment", 
                             "production of metals") ~ "manufacturing and production",
      sub_application == "fire extinguishers" ~ "fire extinguishers",
      sub_application %in% c("other f-gas use", "other ods") ~ "other")
    ) %>%
    left_join(velders, by = "velders_application") %>%
    mutate(meta_application = case_when(
      str_detect(velders_application, "refrigeration|conditioning") ~ "refrigeration and air conditioning",
      str_detect(velders_application, "foam") ~ "foam",
      velders_application == "fire extinguishers" ~ "fire protection",
      .default = velders_application)) %>%
    group_by(year, meta_application, velders_application) %>%
    # Replace default emission factor NA values with 1 
    replace_na(list(emission_factor_dev_countries = 1)) %>%
    summarize(consumption_by_sector = sum(mmt_co2_eq / 
                                            emission_factor_dev_countries, 
                                          na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(consumption_total = sum(consumption_by_sector, 
                                   na.rm = TRUE), .by = year) %>%
    group_by(year, meta_application) %>%
    summarize(consumption_share = sum(consumption_by_sector) / 
                sum(consumption_total)) %>%
    ungroup()
  
  return(abs_hfc_consumption)
}

# Helper function to calculate one year's values
# Now with a 'scenario' parameter: "point", "lower", or "upper"
calc_year <- function(prev_state, i, data, uncertainties, scenario = "point") {
  # Extract current year's input values
  ef_use <- data$emission_factor_installed_base[i]
  imports <- data$imports[i]
  prod <- data$prod[i]
  exports <- data$exports[i]
  destruction_rate <- data$destruction_at_end_of_life[i]
  
  # Extract uncertainties
  u_kigali <- uncertainties$u_kigali[i]
  u_ef_use <- uncertainties$u_ef_installed_base[i]
  u_destruction <- uncertainties$u_destruction[i]
  
  # Apply uncertainty adjustments based on scenario
  if (scenario == "lower") {
    imports <- imports * (1 - u_kigali)
    prod <- prod * (1 - u_kigali)
    exports <- exports * (1 + u_kigali)  # Lower consumption = higher exports
    ef_use <- ef_use * (1 - u_ef_use)
    destruction_rate <- destruction_rate * (1 - u_destruction)
  } else if (scenario == "upper") {
    imports <- imports * (1 + u_kigali)
    prod <- prod * (1 + u_kigali)
    exports <- exports * (1 - u_kigali)  # Higher consumption = lower exports
    ef_use <- ef_use * (1 + u_ef_use)
    destruction_rate <- destruction_rate * (1 + u_destruction)
  }
  # else scenario == "point", use values as-is
  
  # Constants
  ef_filling <- 0
  new_equip_imports <- 0
  new_equip_exports <- 0
  
  # Start calculations using previous state
  bank_start_of_year <- prev_state$bank_end_of_year
  
  domestic_sales <- imports + prod - exports
  in_use_equip_emissions <- bank_start_of_year * ef_use
  servicing <- pmax(domestic_sales, in_use_equip_emissions, na.rm = TRUE)
  new_equip_filling <- domestic_sales - servicing
  filling_emissions <- new_equip_filling * ef_filling
  new_equip_contained <- new_equip_filling - filling_emissions
  bank_contained <- new_equip_contained + new_equip_imports - new_equip_exports
  
  # Get retired equipment from 10 years ago
  retired_equip <- prev_state$bank_contained_history[1]
  
  reclaimed <- retired_equip * destruction_rate
  destroyed <- 0
  exported_used_equip <- 0
  end_of_life_emissions <- retired_equip - reclaimed - destroyed - exported_used_equip
  
  bank_end_of_year <- bank_start_of_year + bank_contained + servicing - 
    in_use_equip_emissions - retired_equip
  
  total_emissions <- filling_emissions + in_use_equip_emissions + end_of_life_emissions
  
  # Update bank_contained history
  new_history <- c(prev_state$bank_contained_history[-1], bank_contained)
  
  # Return state for next iteration
  list(
    bank_end_of_year = bank_end_of_year,
    bank_contained_history = new_history,
    # Store all results
    bank_start_of_year = bank_start_of_year,
    domestic_sales = domestic_sales,
    in_use_equip_emissions = in_use_equip_emissions,
    servicing = servicing,
    new_equip_filling = new_equip_filling,
    filling_emissions = filling_emissions,
    new_equip_contained = new_equip_contained,
    bank_contained = bank_contained,
    retired_equip = retired_equip,
    reclaimed = reclaimed,
    end_of_life_emissions = end_of_life_emissions,
    bank_end_of_year = bank_end_of_year,
    total_emissions = total_emissions
  )
}

# Function to process one component with one scenario
process_component <- function(component_data, uncertainties, scenario = "point") {
  # Set up initial state
  initial_state <- list(
    bank_end_of_year = init_bank_end,
    bank_contained_history = init_bank_contained_history
  )
  
  # Use accumulate to calculate all years
  all_calcs <- purrr::accumulate(
    1:nrow(component_data),
    function(prev_state, i) {
      calc_year(prev_state, i, component_data, uncertainties, scenario)
    },
    .init = initial_state
  )[-1]
  
  # Extract results into dataframe
  calculated_df <- purrr::map_dfr(all_calcs, function(state) {
    tibble(
      bank_start_of_year = state$bank_start_of_year,
      domestic_sales = state$domestic_sales,
      in_use_equip_emissions = state$in_use_equip_emissions,
      servicing = state$servicing,
      new_equip_filling = state$new_equip_filling,
      filling_emissions = state$filling_emissions,
      new_equip_contained = state$new_equip_contained,
      bank_contained = state$bank_contained,
      retired_equip = state$retired_equip,
      reclaimed = state$reclaimed,
      end_of_life_emissions = state$end_of_life_emissions,
      bank_end_of_year = state$bank_end_of_year,
      total_emissions = state$total_emissions
    )
  })
  
  # Combine with original data
  bind_cols(component_data, calculated_df)
}

# Main processing function
process_all_components <- function(kigali_data, uncertainty_data, 
                                   group_vars = c("component", "meta_application")) {
  # Prepare uncertainty data
  uncertainties <- kigali_data %>%
    left_join(
      uncertainty_data %>%
        filter(parameter == "kigali data") %>%
        select(hfc, u_kigali = uncertainty) %>%
        mutate(hfc = str_remove_all(hfc, "-")),
      by = c("component" = "hfc")
    ) %>%
    left_join(
      uncertainty_data %>%
        filter(parameter == "emission factor installed base") %>%
        select(hfc, u_ef_installed_base = uncertainty) %>%
        mutate(hfc = str_remove_all(hfc, "-")),
      by = c("component" = "hfc")
    ) %>%
    left_join(
      uncertainty_data %>%
        filter(parameter == "% destroyed") %>%
        select(hfc, u_destruction = uncertainty) %>%
        mutate(hfc = str_remove_all(hfc, "-")),
      by = c("component" = "hfc")
    ) %>%
    mutate(
      u_kigali            = replace_na(u_kigali, 0),
      u_ef_installed_base = replace_na(u_ef_installed_base, 0),
      u_destruction       = replace_na(u_destruction, 0)
    )
  
  # Split by group_vars (e.g. component x application)
  component_list <- kigali_data %>%
    arrange(across(all_of(group_vars)), year) %>%
    group_by(across(all_of(group_vars))) %>%
    group_split()
  
  uncertainty_list <- uncertainties %>%
    arrange(across(all_of(group_vars)), year) %>%
    group_by(across(all_of(group_vars))) %>%
    group_split()
  
  # Process each scenario
  point_estimates <- map2_dfr(component_list, uncertainty_list,
                               ~process_component(.x, .y, scenario = "point"))
  lower_estimates <- map2_dfr(component_list, uncertainty_list,
                               ~process_component(.x, .y, scenario = "lower"))
  upper_estimates <- map2_dfr(component_list, uncertainty_list,
                               ~process_component(.x, .y, scenario = "upper"))
  
  point_estimates %>%
    mutate(
      total_emissions_lower  = lower_estimates$total_emissions,
      total_emissions_upper  = upper_estimates$total_emissions,
      in_use_emissions_lower = lower_estimates$in_use_equip_emissions,
      in_use_emissions_upper = upper_estimates$in_use_equip_emissions,
      eol_emissions_lower    = lower_estimates$end_of_life_emissions,
      eol_emissions_upper    = upper_estimates$end_of_life_emissions
    ) %>%
    mutate(
      ef_filling         = 0,
      ef_use             = emission_factor_installed_base,
      new_equip_imports  = 0,
      new_equip_exports  = 0,
      destroyed          = 0,
      exported_used_equip = 0
    )
}


## App Interface Setup--------------------------------

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
  # Group by year, component, flow, category (no application - partitioning happens later)
  user_input %>%
    mutate(type = ifelse(hfc %in% mixture_compositions$mixture, "mixture", "component")) %>%
    { bind_rows(
      # Expand mixtures
      filter(., type == "mixture") %>%
        left_join(mixture_compositions, by = c("hfc" = "mixture")) %>%
        mutate(quantity = fraction * quantity) %>%
        select(year, component, quantity, flow, category),
      # Keep pure components
      filter(., type == "component") %>%
        rename(component = hfc) %>%
        select(year, component, quantity, flow, category)
    )
    } %>%
    group_by(year, component, flow, category) %>%
    summarize(quantity = sum(quantity, na.rm = TRUE), .groups = "drop") %>%
    arrange(year, flow, component)
}

# UI--------------------------------------
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 10px;",
    span("HFC Emissions Tool", style = "font-weight: 600; letter-spacing: 0.02em;")
  ),
  
  # ── Theme ──────────────────────────────────────────────────────────────────
  theme = bs_theme(
    version       = 5,
    bg            = "#f8f9fa",
    fg            = "#1a2332",
    primary       = "#2d6a9f",
    secondary     = "#6c757d",
    base_font     = font_google("Source Sans 3"),
    heading_font  = font_google("Source Sans 3"),
    code_font     = font_google("JetBrains Mono"),
    "navbar-bg"               = "#1a2332",
    "navbar-light-color"      = "#e8edf2",
    "navbar-light-hover-color"= "#ffffff",
    "navbar-light-active-color"= "#ffffff",
    "card-border-color"       = "#dee2e6",
    "card-cap-bg"             = "#f1f4f7"
  ),
  
  # ── Global CSS ─────────────────────────────────────────────────────────────
  header = tags$head(
    tags$style(HTML("
      /* Navbar refinements */
      .navbar { 
        border-bottom: 3px solid #2d6a9f; 
        padding-top: 0.6rem; 
        padding-bottom: 0.6rem;
      }
      .navbar-brand { color: #ffffff !important; font-size: 1.1rem; }
      .navbar .nav-link { 
        color: #c8d4e0 !important; 
        font-size: 0.9rem; 
        padding: 0.5rem 1rem !important;
        transition: color 0.15s;
      }
      .navbar .nav-link:hover,
      .navbar .nav-link.active { color: #ffffff !important; }
      .navbar .nav-link.active { 
        border-bottom: 2px solid #5ba3d9; 
        margin-bottom: -2px; 
      }

      /* Country selector in navbar */
      #country { 
        font-size: 0.85rem; 
        padding: 0.25rem 0.5rem; 
        border-radius: 4px;
        min-width: 200px;
        background-color: #263547;
        border: 1px solid #3d5166;
        color: #e8edf2;
      }
      .navbar .control-label { 
        color: #a0b0c0; 
        font-size: 0.78rem; 
        margin-bottom: 2px;
        display: block;
      }
      
      /* Cards */
      .card { 
        box-shadow: 0 1px 4px rgba(0,0,0,0.07); 
        border-radius: 6px;
        margin-bottom: 1rem;
      }
      .card-header { 
        font-weight: 600; 
        font-size: 0.88rem;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: #4a5568;
        padding: 0.6rem 1rem;
      }
      
      /* Accordion (instructions) */
      .accordion-button { 
        font-weight: 600; 
        font-size: 0.9rem;
        color: #2d6a9f;
        background-color: #eef3f8;
      }
      .accordion-button:not(.collapsed) { 
        background-color: #dde8f3; 
        color: #1a4f7a;
        box-shadow: none;
      }
      .accordion-body { 
        font-size: 0.88rem; 
        color: #4a5568; 
        line-height: 1.6;
        background-color: #f8fafc;
      }
      
      /* DT tables */
      .dataTables_wrapper { font-size: 0.83rem; }
      table.dataTable thead th { 
        background-color: #f1f4f7; 
        color: #2d3748;
        font-weight: 600;
        border-bottom: 2px solid #cbd5e0;
        white-space: nowrap;
      }
      table.dataTable tbody tr:hover { background-color: #eef3f8 !important; }
      .dataTables_filter input { font-size: 0.82rem; }
      .dataTables_length select { font-size: 0.82rem; }
      
      /* Action buttons */
      .btn-primary { 
        background-color: #2d6a9f; 
        border-color: #2d6a9f;
        font-size: 0.88rem;
        padding: 0.4rem 1.1rem;
        border-radius: 4px;
        transition: background-color 0.15s;
      }
      .btn-primary:hover { background-color: #1a4f7a; border-color: #1a4f7a; }
      
      /* Tab content padding */
      .tab-content { padding-top: 1rem; }
      
      /* Disclosure toggle for table */
      .table-toggle-btn {
        font-size: 0.82rem;
        color: #2d6a9f;
        background: none;
        border: none;
        padding: 0;
        cursor: pointer;
        text-decoration: underline;
        text-underline-offset: 2px;
      }
    "))
  ),
  
  # ── JavaScript for contingent category dropdowns ───────────────────────────
  tags$head(tags$script(HTML("
    var categoryMap = {
      'imports':    ['new', 'recovered', 'feedstock'],
      'exports':    ['new', 'recovered'],
      'production': ['produced', 'feedstock_produced', 'destroyed']
    };
    var FLOW_COL = 2;
    var CAT_COL  = 3;

    function updateCategoryDropdown(hot, row) {
      var flowVal = hot.getDataAtCell(row, FLOW_COL);
      var allowed = categoryMap[flowVal] || [];
      var curCat  = hot.getDataAtCell(row, CAT_COL);
      if (allowed.length > 0 && !allowed.includes(curCat)) {
        hot.setDataAtCell(row, CAT_COL, allowed[0]);
      }
      hot.setCellMeta(row, CAT_COL, 'source', allowed);
      hot.render();
    }

    $(document).on('shiny:value', function(e) {
      if (e.name !== 'kigali_input_table') return;
      setTimeout(function() {
        var container  = document.getElementById('kigali_input_table');
        if (!container) return;
        var hot = container.querySelector('.handsontable');
        if (!hot || !hot.hotInstance) return;
        var htInstance = hot.hotInstance;
        var nrows = htInstance.countRows();
        for (var r = 0; r < nrows; r++) { updateCategoryDropdown(htInstance, r); }
        htInstance.addHook('afterChange', function(changes, source) {
          if (!changes) return;
          changes.forEach(function(change) {
            if (change[1] === FLOW_COL) updateCategoryDropdown(htInstance, change[0]);
          });
        });
      }, 300);
    });
  "))),
  
  # ── Country selector (persistent, in navbar) ───────────────────────────────
  nav_spacer(),
  nav_item(
    div(
      style = "padding: 0.25rem 0.5rem;",
      tags$label(`for` = "country", class = "control-label", "Country"),
      selectInput(
        "country",
        label    = NULL,
        choices  = sort(unique(edgar$name)),
        selected = NULL,
        width    = "220px"
      )
    )
  ),
  
  # ── TAB 1: Scoping Statement ───────────────────────────────────────────────
  nav_panel(
    "Scoping Statement",
    icon = icon("chart-line"),
    
    # Instructions accordion
    accordion(
      id    = "instructions_accordion",
      open  = FALSE,
      accordion_panel(
        "Instructions & Guidance",
        icon = icon("circle-info"),
        div(
          style = "max-width: 860px;",
          p(tags$strong("Placeholder:"), " Step-by-step guidance will go here.
            This section will walk users through interpreting the scoping data,
            understanding the emissions estimates, and navigating the tool workflow."),
          tags$ul(
            tags$li("Step 1: Select a country from the dropdown in the top-right."),
            tags$li("Step 2: Review historical HFC emissions patterns in the Scoping Statement tab."),
            tags$li("Step 3: Enter Kigali reporting data in the Kigali Data tab."),
            tags$li("Step 4: Generate emissions estimates in the Emissions tab.")
          )
        )
      )
    ),
    
    # Main visuals row
    layout_columns(
      col_widths = c(7, 5),
      
      # Line chart card
      card(
        card_header("HFC Emissions Over Time"),
        card_body(
          padding = "0.5rem",
          plotOutput("line_plot", height = "380px")
        )
      ),
      
      # Sunburst card
      card(
        card_header("Emissions by Application"),
        card_body(
          padding = "0.5rem",
          sunburstOutput("sunburst_subapp", height = "380px")
        )
      )
    ),
    
    # Collapsible time series table
    accordion(
      id   = "table_accordion",
      open = FALSE,
      accordion_panel(
        "Time Series Data",
        icon = icon("table"),
        card_body(
          padding = "0.25rem",
          DT::DTOutput("timeseries_table")
        )
      )
    )
  ),
  
  # ── TAB 2: Kigali Data ────────────────────────────────────────────────────
  nav_panel(
    "Kigali Data",
    icon = icon("file-import"),
    
    card(
      card_header("Kigali Reporting Data Entry"),
      card_body(
        p(class = "text-muted", style = "font-size: 0.85rem; margin-bottom: 0.75rem;",
          "Enter all production, import, and export data below.",
          "Right-click for options to add or remove rows.",
          "Category options update automatically based on the selected Flow."),
        rHandsontableOutput("kigali_input_table"),
        br(),
        actionButton("calc_kigali", "Calculate Totals", class = "btn-primary")
      )
    ),
    
    card(
      card_header("Calculated Totals"),
      card_body(DTOutput("kigali_totals_table"))
    )
  ),
  
  # ── TAB 3: Emissions ──────────────────────────────────────────────────────
  nav_panel(
    "Emissions",
    icon = icon("smog"),
    
    card(
      card_header("HFC Emissions Estimates"),
      card_body(
        actionButton("calculate_emissions", "Calculate Emissions", class = "btn-primary"),
        br(), br(),
        DTOutput("emissions_table")
      )
    )
  )
)
# Server----------------------------
server <- function(input, output, session) {
  
  # Initial empty dataframe for Kigali input
  initial_kigali <- data.frame(
    year     = rep(2020L, 5),
    hfc      = rep("", 5),
    flow     = rep("imports", 5),
    category = rep("new", 5),
    quantity = rep(0, 5),
    stringsAsFactors = FALSE
  )
  
  ## Scoping data------------
  
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
  
  ## Kigali data entry -------
  output$kigali_input_table <- renderRHandsontable({
    df <- if (!is.null(input$kigali_input_table)) {
      hot_to_r(input$kigali_input_table)
    } else {
      initial_kigali
    }
    df$year <- as.integer(df$year)
    
    rhandsontable(df, rowHeaders = NULL) %>%
      hot_col("year",     type = "numeric",  format = "0", colWidths = 65) %>%
      hot_col("hfc",      type = "dropdown", colWidths = 220,
              source = c("", unique(c(mixture_compositions$component,
                                      mixture_compositions$mixture)))) %>%
      hot_col("flow",     type = "dropdown", colWidths = 110,
              source = c("imports", "exports", "production")) %>%
      hot_col("category", type = "dropdown", colWidths = 160,
              # Full list here; JS narrows it live based on flow selection
              source = c("new", "recovered", "feedstock",
                         "produced", "feedstock_produced", "destroyed")) %>%
      hot_col("quantity", type = "numeric",  colWidths = 90) %>%
      hot_table(highlightRow = TRUE, highlightCol = TRUE)
  })
  
  kigali_data <- eventReactive(input$calc_kigali, {
    req(input$kigali_input_table)
    df <- hot_to_r(input$kigali_input_table) %>%
      filter(hfc != "", !is.na(quantity), quantity != 0)
    
    if (nrow(df) == 0) {
      return(data.frame(year = integer(), component = character(),
                        flow = character(), category = character(),
                        quantity = numeric()))
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
  

  ## Emissions ---------
  
  emissions_data <- eventReactive(input$calculate_emissions, {
    req(kigali_data(), scoping_data())
    
    kd <- kigali_data()
    
    # Pivot to wide format (year x component)
    imports_df <- kd %>%
      filter(flow == "imports") %>%
      group_by(year, component) %>%
      summarize(imports = sum(quantity, na.rm = TRUE), .groups = "drop")
    
    exports_df <- kd %>%
      filter(flow == "exports") %>%
      group_by(year, component) %>%
      summarize(exports = sum(quantity, na.rm = TRUE), .groups = "drop")
    
    prod_df <- kd %>%
      filter(flow == "production") %>%
      group_by(year, component) %>%
      summarize(prod = sum(quantity, na.rm = TRUE), .groups = "drop")
    
    consumption_wide <- imports_df %>%
      full_join(exports_df, by = c("year", "component")) %>%
      full_join(prod_df,    by = c("year", "component")) %>%
      replace_na(list(imports = 0, exports = 0, prod = 0))
    
    # --- Step 1: Partition by application BEFORE any calculations ---
    # Get application shares from Velders/EDGAR scoping data.
    # NOTE: shares are currently identical across HFCs (same proportions applied
    # to every component). Revisit if HFC-specific Velders data becomes available.
    app_shares <- get_abs_hfc_consumption(scoping_data(), velders)
    available_years <- unique(app_shares$year)
    
    # Cross-join consumption with application shares (matched to nearest EDGAR year)
    emissions_input <- consumption_wide %>%
      mutate(share_year = available_years[which.min(abs(available_years - year))][1],
             .by = year) %>%
      left_join(app_shares, by = c("share_year" = "year")) %>%
      # Apply shares to split each flow by application
      mutate(
        imports = imports * consumption_share,
        exports = exports * consumption_share,
        prod    = prod    * consumption_share
      ) %>%
      select(-share_year) %>%
      # --- Step 2: Join application-specific defaults ---
      # Currently defaults are uniform across applications; join will be on hfc only.
      # When application-specific parameters are available, add application to the join key.
      left_join(
        hfc_defaults %>% rename(component = hfc),
        by = "component"
      ) %>%
      arrange(meta_application, component, year)
    
    # --- Step 3: Run emissions model for each (component x application) slice ---
    process_all_components(emissions_input, uncertainty, 
                           group_vars = c("component", "meta_application"))
  })
  
  output$emissions_table <- renderDT({
    req(emissions_data())
    datatable(
      emissions_data() %>%
        select(year, component, meta_application, consumption_share,
               total_emissions, total_emissions_lower, total_emissions_upper) %>%
        mutate(across(where(is.numeric), ~round(.x, 4))),
      options    = list(dom = "Bfrtip", buttons = c("copy", "csv", "excel"),
                        pageLength = 25, scrollX = TRUE),
      extensions = "Buttons",
      rownames   = FALSE
    )
  })
}
## Run the app-------
shinyApp(ui = ui, server = server)
