library(tidyverse)

energy <- read.csv("data/owid-energy-data.csv")

nas <- energy |> summarise(across(everything(), ~ sum(is.na(.)))) |> 
  pivot_longer(population:wind_share_energy, names_to="col", values_to="no_na")

energy_clean <- energy |> select(country, year, iso_code, population, oil_production, coal_production)
 
solar_share <- energy |> select(country, year, iso_code, solar_share_elec, solar_share_energy)
