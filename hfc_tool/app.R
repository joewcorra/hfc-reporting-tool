library(shiny)
library(pins)
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

    // Column indices (0-based): flow=2, category=3
    var FLOW_COL = 2;
    var CAT_COL  = 3;

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
