library(tidyverse)
library(pins)


hfc_board <- board_folder("data/pins")

# We should have a master list of HFCs....maybe from the data dictionary?
edgar <- pin_read(hfc_board, "edgar")
velders <- pin_read(hfc_board, "velders")
uncertainty <- pin_read(hfc_board, "uncertainty")
mixture_compositions <- pin_read(hfc_board, "mixture_compositions")
applications <- pin_read(hfc_board, "applications")
hfc_defaults <- pin_read(hfc_board, "hfc_defaults")
    
# Tab Emissions Scoping Statement------------------------------

user_input <- "united states"

scoping <- edgar %>%
  left_join(applications, by = "sub_application") %>%
  filter(name == user_input) %>%
  mutate(co2_eq = gwp * hfc_emissions * 1000) %>%
  group_by(year, application, sub_application) %>%
  summarize(mmt_co2_eq = sum(co2_eq, na.rm = TRUE) / 10^6) %>%
  ungroup() 

 scoping %>%
   pivot_wider(names_from = year, values_from = mmt_co2_eq) 
 # next: make this table pretty; get subtotals by application & annual totals
 
 
 # Figures: Line plot (application~year); pie (current year by application); 
 # pie (current year by subapplication)
 
 # Emission Model Assumptions---------------------------------

 abs_hfc_consumption <- scoping %>%
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
   summarize(consumption_share = sum(consumption_by_sector) / sum(consumption_total)) %>%
   ungroup()
   
  
# Function: Kigali Data Emissions---------------------
 
 # Initial values
 init_bank_start <- 0
 init_bank_end <- 0
 init_bank_contained_history <- rep(0, 10)
 
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
 process_all_components <- function(kigali_data, uncertainty_data) {
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
     # Replace NAs with 0 (no uncertainty)
     mutate(
       u_kigali = replace_na(u_kigali, 0),
       u_ef_installed_base = replace_na(u_ef_installed_base, 0),
       u_destruction = replace_na(u_destruction, 0)
     )
   
   # Split by component
   component_list <- kigali_data %>%
     arrange(component, year) %>%
     group_by(component) %>%
     group_split()
   
   # Get corresponding uncertainty chunks
   uncertainty_list <- uncertainties %>%
     arrange(component, year) %>%
     group_by(component) %>%
     group_split()
   
   # Process each scenario
   point_estimates <- map2_dfr(
     component_list, 
     uncertainty_list,
     ~process_component(.x, .y, scenario = "point")
   )
   
   lower_estimates <- map2_dfr(
     component_list, 
     uncertainty_list,
     ~process_component(.x, .y, scenario = "lower")
   )
   
   upper_estimates <- map2_dfr(
     component_list, 
     uncertainty_list,
     ~process_component(.x, .y, scenario = "upper")
   )
   
   # Combine into single dataframe with uncertainty bounds
   point_estimates %>%
     mutate(
       total_emissions_lower = lower_estimates$total_emissions,
       total_emissions_upper = upper_estimates$total_emissions,
       in_use_emissions_lower = lower_estimates$in_use_equip_emissions,
       in_use_emissions_upper = upper_estimates$in_use_equip_emissions,
       eol_emissions_lower = lower_estimates$end_of_life_emissions,
       eol_emissions_upper = upper_estimates$end_of_life_emissions
     ) %>%
     mutate(
       ef_filling = 0,
       ef_use = emission_factor_installed_base,
       new_equip_imports = 0,
       new_equip_exports = 0,
       destroyed = 0,
       exported_used_equip = 0
     )
 }
 
 # Emssions-----------------------------------------
 
 
 # Calculate net imports, exports, and production & destruction
 imports <- kigali_data %>% 
   group_by(year, flow, component) %>%
   summarize(imports = new + recovered - feedstock) %>%
   ungroup()
 
 exports <- kigali_data %>% 
   group_by(year, flow, component) %>%
   summarize(exports = new + recovered) %>%
   ungroup()
 
 production <- kigali_data %>% 
   group_by(year, flow, component) %>%
   summarize(prod = produced - feedstock_produced - destroyed) %>%
   ungroup()
 
 # Join all refrigeration data
  emissions <- list(
   imports,
   exports,
   production) %>%
   reduce(full_join, by = c("year", "flow", "component")) %>%
   left_join(hfc_defaults, by = c("component" = "hfc")) 
 
 
 # Run the full processing
 hfc_emissions_complete <- process_all_components(emissions, uncertainty)
 
 # View result
 hfc_emissions_complete %>%
   select(year, flow, component, total_emissions, total_emissions_lower, total_emissions_upper)
 
 