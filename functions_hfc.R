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
   
  
# Refrigeration------------------------------------------------
 
 # Calculate net imports, exports, and production & destruction
refrigeration_imports <- imports_data %>% 
   group_by(year, component) %>%
   summarize(imports = new + recovered - feedstock)
 
 refrigeration_exports <- exports_data %>% 
   group_by(year, component) %>%
   summarize(exports = new + recovered)
 
 refrigeration_prod <- prod_data %>% 
   group_by(year, component) %>%
   summarize(prod = produced - feedstock_produced - destroyed)
 
 # Join all refrigeration data
 refrigeration <- list(
   refrigeration_imports,
   refrigeration_exports,
   refrigeration_prod) %>%
   reduce(full_join, by = c("year", "component")) %>%
   left_join(hfc_defaults, by = c("component" = "hfc")) %>%
   mutate(
     ef_filling = emission_factor_filling,
     ef_use = emission_factor_installed_base,
     bank_end_of_year = 0, # default value to prevent error. redefined below.
     bank_start_of_year =  lag(bank_end_of_year, n = 1, order_by = year, default = 0),
     domestic_sales = imports + prod - exports, 
     in_use_equip_emitted = bank_start_of_year * ef_use, 
     servicing = pmax(domestic_sales, in_use_equip_emitted, na.rm = TRUE), 
     new_equip_filling = domestic_sales - servicing,
     filling_emissions = new_equip_filling * ef_filling, 
     new_equip_contained = new_equip_filling - filling_emissions,
     bank_contained = new_equip_contained + new_equip_imports - new_equip_exports,
     retired_equip = lag(bank_contained, n = 10, order_by = year, default = 0),
     reclaimed = retired_equip * destruction_at_end_of_life,
     bank_end_of_year = bank_start_of_year + bank_contained + servicing - in_use_equip_emitted - retired_equip)
 
 abs_hfc_consumption %>%
   left_join(uncertainty) %>%
   mutate(
     # IS THIS RIGHT????????????????????????????
     lower_estimate = consumption_share * (1 - uncertainty),
     upper_estimate = consumption_share * (1 + uncertainty))
 
 

   
   