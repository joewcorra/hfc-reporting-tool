library(tidyverse)
library(pins)
library(treemapify)
library(viridis)


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
                            "industrial refrigeration") ~ "commercial and industrial refrigeration",
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
   group_by(year, velders_application) %>%
   # Replace default emission factor NA values with 1 
   replace_na(list(emission_factor_dev_countries = 1)) %>%
   summarize(consumption_by_sector = sum(mmt_co2_eq / 
                                           emission_factor_dev_countries, 
                                         na.rm = TRUE)) %>%
   ungroup()%>%
   mutate(consumption_total = sum(consumption_by_sector, 
                                  na.rm = TRUE), .by = year) %>%
   mutate(consumotion_share = consumption_by_sector / consumption_total)


 
   
  
# Refrigeration------------------------------------------------
 
 # Why is the current year computed separately?
 current_year <- hfc_defaults %>%
   # data from 'refrigeration_data_summary':
   year = 
   hfc = 
   production = 
   imports = 
   exports = 
 
   # This is a mess--need to clarify what all of these variables are for and why
   # they are needed. 
  
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
   
   