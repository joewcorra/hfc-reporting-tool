library(tidyverse)
library(pins)
library(gt)

hfc_board <- board_folder("data/pins")

# We should have a master list of HFCs....maybe from the data dictionary?
edgar <- pin_read(hfc_board, "edgar")
velders <- pin_read(hfc_board, "velders")
uncertainty <- pin_read(hfc_board, "uncertainty")
mixture_compositions <- pin_read(hfc_board, "mixture_compositions")
applications <- pin_read(hfc_board, "applications")

# Tabs 3A, 3B, 3C----------------------------------

# need separate dfs for imports, exports, production & destruction
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

user_input <- "usa"

scoping <- edgar %>%
  left_join(applications, by = "sub_application") %>%
  filter(c_group == user_input) %>%
  mutate(co2_eq = gwp * hfc_emissions) %>%
  group_by(year, application, sub_application) %>%
  summarize(mmt_co2_eq = sum(co2_eq, na.rm = TRUE) / 10^6) %>%
  ungroup() 

 scoping %>%
   pivot_wider(names_from = year, values_from = mmt_co2_eq) %>%
   gt()
 # next: make this table pretty; get subtotals by application & annual totals
 
 
 # Figures: Line plot (application~year); pie (current year by application); 
 # pie (current year by subapplication)
 
 
 # Refrigeration ----------------------------------
 
 
  

