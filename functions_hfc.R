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

# Tabs 3A, 3B, 3C----------------------------------

# need separate dfs for 1) imports, 2) exports, 3) production & destruction
total_hfc <- user_input %>% 
  filter(type == "mixture") %>%
  left_join(mixture_compositions, by = c("hfc" = "mixture")) %>%
  mutate(component_quantity = fraction * quantity) %>%
  group_by(component) %>%
  summarize(total_hfc_mixture = sum(component_quantity, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(user_input %>% 
              filter(type == "component") %>%
              rename(total_hfc_component = quantity), 
            by = c("component" = "hfc")) %>%
  mutate(total_hfc = total_hfc_component + total_hfc_mixture)
    
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
   pivot_wider(names_from = year, values_from = mmt_co2_eq) %>%
   gt()
 # next: make this table pretty; get subtotals by application & annual totals
 
 
 # Figures: Line plot (application~year); pie (current year by application); 
 # pie (current year by subapplication)
 
 # Emission Model Assumptions---------------------------------
 
 # Need to review all of these. Some are divided by the Velders emissions 
 # factors, and some are not. Some applications are aggregated into what seem to be 
 # arbitrary groups; others are unchanged from the scoping statement data. 
 # need to add all missing apps to velders_applications with factor = 1.
 
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
   
  
# Refrigeration------------------------------------------------
 
 # Calculate net imports, exports, and production & destruction
refrigeration_imports <- imports_data %>% 
   group_by(year, component) %>%
   summarize(refrigeration_imports = new + recovered - feedstock)
 
 refrigeration_exports <- exports_data %>% 
   group_by(year, component) %>%
   summarize(refrigeration_exports = new + recovered)
 
 refrigeration_prod <- prod_data %>% 
   group_by(year, component) %>%
   summarize(refrigeration_prod = produced - feedstock_produced - destroyed)
 
 # Join all refrigeration data
 refrigeration <- list(
   refrigeration_imports,
   refrigeration_exports,
   refrigeration_prod) %>%
   reduce(full_join, by = c("year", "component")) %>%
   mutate(total_new_agent = refrigeration_imports + 
            refrigeration_prod - 
          refrigeration_exports)
   left_join(hfc_defaults, by = c("component" = "hfc")) %>%
     mutate(end_of_life_substance = ,
            end_of_life_destruction = ,
            end_of_life_release = ,
            bank = ,
            emissions = ,
            emissions_upper_estimate = ,
            emissions_lower_estimate = ,
            bank_emissions = bank * emissions_factor_installed_base,
            retired_eq_emissions = ,
            # sum???? = bank_emissions + retired_eq_emissions, 
            bank_emissions_ratio = bank / emissions)

  lower_estimate = consumption_share * (1 - uncertainty)
  upper_estimate = consumption_share * (1 + uncertainty)
   
 previous_years_estimated <-
   #
   year = 
   hfc = 
   production = 
   agent_in_imports = 
   agent_in_exports = 
   total_new_agent = production + agent_in_imports + agent_in_exports
   substance_in_eq_eol = 
   destruction_in_eq_eol = substance_in_eq_eol * recovery
   release_of_substabce_in_eq_eol = substance_in_eq_eol - destruction_in_eq_eol
   bank = total_new_agent
   emissions = bank * ef + release_of_substabce_in_eq_eol
   uncertainty_upper_bound = 
     uncertainty_lower_bound = 
  emission_from_bank = bank * ef
   emission_eq_eol = 
     sum = emission_from_bank + emission_eq_eol
   bankemissions = bank / emissions
   
   