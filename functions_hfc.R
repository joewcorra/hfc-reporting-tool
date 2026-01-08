hfc_board <- board_folder("data/pins")

edgar <- read_excel("HFC Tool v09_05Aug2024.xlsx", 
                    sheet = "EDGAR Applications", 
                    range = "a1:an4180")


edgar <- edgar %>% clean_names()
edgar  <- edgar %>% 
  rename(
    c_group = c_group_im24_sh,
    country_code = country_code_a3,
  )
edgar<-edgar %>%
  pivot_longer(cols = starts_with("y_"), values_to = "hfc_emissions", names_to = "year")

edgar <- edgar %>% mutate(across(where(is.character),  ~ str_to_lower(.) ))

edgar <- edgar %>% mutate(year = parse_number(year) %>% as_factor())

pins::pin_write(hfc_board, uncertainty)

library(tibble)

velders <- tribble(
  ~application, ~emission_factor_dev_countries,
  "Domestic Refrigeration", 2.5,
  "Commercial and Industrial Refrigeration", 11.0,
  "Stationary Air Conditioning", 7.0,
  "Mobile Air Conditioning", 15.0,
  "Closed Cell Foam", 5.0,
  "Open Cell Foam", 100.0,
  "Aerosols", 67.0,
  "Fire Extinguishing", 3.0,
  "Solvents", 67.0
)

library(tibble)

uncertainty <- tribble(
  ~parameter, ~`HFC-23`, ~`HFC-32`, ~`HFC-125`, ~`HFC-134a`, ~`HFC-143a`, ~`HFC-152a`, ~`HFC-227ea`, ~`HFC-236fa`,
  "Kigali Data",                           5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
  "Year of Introduction to Market",   NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,
  "Growth Rate in New Equipment Sales",    0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
  "Equipment Lifetime",                NA,  NA,  NA,  NA,  NA,  NA,  NA,  NA,
  "Emission Factor from Installed Base",   5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0,
  "Destruction at End of Life",            10.0,10.0,10.0,10.0,10.0,10.0,10.0,10.0
)

uncertainty <- uncertainty %>% mutate(uncertainty =  uncertainty / 100)
uncertainty <-  uncertainty %>% pivot_longer(cols = starts_with("H"), names_to = "hfc", values_to = "uncertainty")
