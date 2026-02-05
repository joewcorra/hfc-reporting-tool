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
   summarize(imports = new + recovered - feedstock) %>%
   ungroup()
 
 refrigeration_exports <- exports_data %>% 
   group_by(year, component) %>%
   summarize(exports = new + recovered) %>%
   ungroup()
 
 refrigeration_prod <- prod_data %>% 
   group_by(year, component) %>%
   summarize(prod = produced - feedstock_produced - destroyed) %>%
   ungroup()
 
 # Join all refrigeration data
 refrigeration <- list(
   refrigeration_imports,
   refrigeration_exports,
   refrigeration_prod) %>%
   reduce(full_join, by = c("year", "component")) %>%
   left_join(hfc_defaults, by = c("component" = "hfc")) %>%
   mutate(bank_start_of_year = 0, 
          bank_end_of_year = 0) %>%
   add_row(year = as_factor(2019), 
           bank_start_of_year = 0, 
           bank_end_of_year = 0) %>%
   
   mutate(
     ef_filling = 0,
     ef_use = emission_factor_installed_base,
     # new_equip_imports and new_equip_exports need to be made user input values
     new_equip_imports = 0, 
     new_equip_exports = 0, 
     domestic_sales = imports + prod - exports, 
     bank_start_of_year = lag(bank_end_of_year, 
                               n = 1,
                               order_by = year),
     in_use_equip_emissions = bank_start_of_year * ef_use, 
     servicing = pmax(domestic_sales, 
                      in_use_equip_emissions, na.rm = TRUE), 
     new_equip_filling = domestic_sales - servicing,
     filling_emissions = new_equip_filling * ef_filling, 
     new_equip_contained = new_equip_filling - filling_emissions,

     bank_contained = new_equip_contained + new_equip_imports - new_equip_exports,
     retired_equip = 0,
       # lag(bank_contained, 
       #                   n = 10, 
       #                   order_by = year, 
       #                   default = 0),
     reclaimed = retired_equip * destruction_at_end_of_life,
     # Destroyed and exported in used equipment need to be made user input values
     destroyed = 0, 
     exported_used_equip = 0, 
     end_of_life_emissions = retired_equip - reclaimed - destroyed - exported_used_equip, 
     bank_end_of_year = bank_start_of_year + bank_contained + servicing - in_use_equip_emissions - retired_equip, 
     total_emissions = filling_emissions + in_use_equip_emissions + end_of_life_emissions) %>%
   left_join(abs_hfc_consumption %>% 
               filter(meta_application == "refrigeration and air conditioning") %>%
               select(-meta_application),
             by = "year") %>%
   left_join(uncertainty %>%
               filter(parameter == "kigali data") %>%
               select(-parameter) %>%
               mutate(hfc = str_remove_all(hfc, "-")), 
             by = c("component" = "hfc")) %>%
   mutate(
     lower_estimate = total_emissions * consumption_share * (1 - uncertainty),
     upper_estimate = total_emissions * consumption_share * (1 + uncertainty))
 
 

   
   