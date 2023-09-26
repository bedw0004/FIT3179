library(tidyverse)

energy <- read.csv("data/owid-energy-data.csv")
map_df <- fromJSON("data/ne_110m_admin_0_countries.topojson")

valid_countries <- append(read.csv("data/country_info.csv")$name, "United States")

nas <- energy |> summarise(across(everything(), ~ sum(is.na(.)))) |> 
  pivot_longer(population:wind_share_energy, names_to="col", values_to="no_na")

energy_clean <- energy |> select(country, year, iso_code, population, oil_production, coal_production)

solar_share <- energy |> select(country, year, iso_code, solar_share_elec, solar_share_energy)

######## THE ENERGY PROBLEM
co2_gdppc <- read.csv("data/consumption-co2-per-capita-vs-gdppc.csv") |> 
  group_by(Entity) |> 
  mutate_at(c("Continent"), ~na_if(., "")) |> 
  fill(Continent, .direction='downup') |> 
  ungroup() |> 
  filter(Year > 1985) |> 
  rename("GDP_pc" = "GDP.per.capita..PPP..constant.2017.international...") |> 
  rename("population" = "Population..historical.estimates.") |> 
  rename("annual_co2_pc" = "Annual.consumption.based.CO..emissions..per.capita.")
not_in <- unique(co2_gdppc[!(co2_gdppc$Entity %in% valid_countries),]$Entity)
co2_gdppc <- co2_gdppc[co2_gdppc$Entity %in% valid_countries,]

write.csv(co2_gdppc, "data/co2_gdppc.csv")

co2_gdppc_multiples <- co2_gdppc |> 
  filter(Entity %in% c("Australia", "China", "India", "Russia", "United States", "Germany")) 
  # mutate(annual_co2_pc = annual_co2_pc * 2000) |> 
  # pivot_longer(c(annual_co2_pc, GDP_pc), names_to="type", values_to="vals")
  # pivot_wider(names_from=Entity, values_from=c(annual_co2_pc, GDP_pc))

write.csv(co2_gdppc_multiples, "data/co2_gdppc_multiples.csv")

######## ENERGY PRODUCTION BY SOURCE
elec_prod_by_source <- read.csv("data/electricity_prod_source_stacked.csv")

elec_prod_by_source_world <- elec_prod_by_source |> filter(Entity == "World")
oldnames <- colnames(elec_prod_by_source_world)[4:12]
newnames <- c("other_renewables", "bioenergy", "solar", "wind", "hydro", "nuclear", "oil", "gas", "coal")
elec_prod_by_source_world <- elec_prod_by_source_world |> rename_at(vars(oldnames), ~ newnames)
write.csv(elec_prod_by_source_world, "data/elec_prod_source_world.csv")

elec_prod_source_rank <- elec_prod_by_source_world |> 
  pivot_longer(cols=other_renewables:coal, names_to="source", values_to = "energy_twh") |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  group_by(Year) |> 
  arrange(desc(energy_twh), .by_group = TRUE) |> 
  mutate(rank = row_number())

write.csv(elec_prod_source_rank, "data/elec_prod_source_rank.csv")
  

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

