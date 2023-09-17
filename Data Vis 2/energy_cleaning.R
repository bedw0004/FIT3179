library(tidyverse)

energy <- read.csv("data/owid-energy-data.csv")
map_df <- fromJSON("data/ne_110m_admin_0_countries.topojson")

nas <- energy |> summarise(across(everything(), ~ sum(is.na(.)))) |> 
  pivot_longer(population:wind_share_energy, names_to="col", values_to="no_na")

energy_clean <- energy |> select(country, year, iso_code, population, oil_production, coal_production)

solar_share <- energy |> select(country, year, iso_code, solar_share_elec, solar_share_energy)

######## THE ENERGY PROBLEM
co2_gdppc <- read.csv("data/consumption-co2-per-capita-vs-gdppc.csv") |> 
  filter(Year > 1985) |> 
  rename("GDP_pc" = "GDP.per.capita..PPP..constant.2017.international...") |> 
  rename("population" = "Population..historical.estimates.") |> 
  rename("annual_co2_pc" = "Annual.consumption.based.CO..emissions..per.capita.")
write.csv(solar_share, "data/co2_gdppc.csv")

######## SHARE OF SOLAR ELECTRICITY
solar_share <- read.csv("data/share-electricity-solar.csv") |> 
  rename(`solar_perc` = `Solar....electricity.`) |> 
  pivot_wider(names_from=Year, values_from=solar_perc) |> 
  pivot_longer(cols=`2000`:`1999`, names_to="Year", values_to="solar_perc")

write.csv(solar_share, "data/share-elec-solar-raw.csv")

solar_share_filled <- solar_share |> 
  group_by(Entity) |> 
  arrange(Year) |> 
  fill(solar_perc, .direction='down')
write.csv(solar_share_filled, "data/share-elec-solar.csv")

solar_share_imputed <- solar_share |> 
  pivot_wider(names_from=Year, values_from=solar_perc) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |> 
  pivot_longer(cols=`2000`:`1999`, names_to="Year", values_to="solar_perc")
# write.csv(solar_share_imputed, "data/share-elec-solar.csv")

solar_share_2022 <- read.csv("data/share-electricity-solar.csv") |> filter(Year == 2022) |> rename(`solar_perc` = `Solar....electricity.`)
write.csv(solar_share_2022, "data/share-elec-solar-2022.csv")

solar_share_2021 <- read.csv("data/share-electricity-solar.csv") |> filter(Year == 2021) |> rename(`solar_perc` = `Solar....electricity.`)
write.csv(solar_share_2021, "data/share-elec-solar-2021.csv")

