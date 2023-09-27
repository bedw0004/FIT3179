library(tidyverse)

energy <- read.csv("data/owid-energy-data.csv")
map_df <- fromJSON("data/ne_110m_admin_0_countries.topojson")

interesting_countries <- c("World", "Australia", "Brazil", "China", "India", "Russia", "United States", 
                           "Germany", "France", "United Kingdom", "Norway", "Canada", "Sweden", "Japan", 
                           "South Africa")

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

######## ELECTRICITY PRODUCTION BY SOURCE
elec_prod_by_source <- read.csv("data/electricity_prod_source_stacked.csv")

oldnames <- colnames(elec_prod_by_source)[4:12]
newnames <- c("other_renewables", "bioenergy", "solar", "wind", "hydro", "nuclear", "oil", "gas", "coal")
elec_prod_by_source <- elec_prod_by_source |> rename_at(vars(oldnames), ~ newnames) |> 
  filter(Entity %in% interesting_countries)

elec_prod_by_source_rank <- elec_prod_by_source |> 
  pivot_longer(cols=other_renewables:coal, names_to="source", values_to = "energy_twh") |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  group_by(Year) |> 
  arrange(desc(energy_twh), .by_group = TRUE) |> 
  mutate(rank = row_number())

elec_prod_source_rank <- elec_prod_by_source_rank |> filter(Entity == "World")

write.csv(elec_prod_source_rank, "data/elec_prod_source_rank.csv")

elec_prod_by_source <- elec_prod_by_source |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  mutate(renewables = other_renewables + bioenergy + solar + wind + hydro,
         fossil_fuels = coal + oil + gas) |> 
  select(-other_renewables, -bioenergy, -solar, -wind, -hydro, -coal, -oil, -gas) |> 
  pivot_longer(cols=nuclear:fossil_fuels, names_to="source", values_to = "energy_twh") |> 
  group_by(Year, Entity) |> 
  mutate(energy_prop = energy_twh / sum(energy_twh))

write.csv(elec_prod_by_source, "data/elec_prod_by_source.csv")

######## ENERGY CONSUMPTION BY SOURCE
energy_consump_by_source <- read.csv("data/energy_consumption_source.csv")

oldnames <- colnames(energy_consump_by_source)[4:13]
newnames <- c("other_renewables", "biofuels", "solar", "wind", "hydro", "nuclear", "gas", "oil", "coal", "traditional_biomass")
energy_consump_by_source <- energy_consump_by_source |> 
  filter(Year > 1899) |> 
  rename_at(vars(oldnames), ~ newnames) |> 
  mutate(bioenergy = biofuels + traditional_biomass) |> 
  select(-biofuels, -traditional_biomass)

write.csv(energy_consump_by_source, "data/energy_consump_source_world.csv")

energy_consump_source_rank <- energy_consump_by_source |> 
  pivot_longer(cols=other_renewables:bioenergy, names_to="source", values_to = "energy_twh") |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  group_by(Year) |> 
  arrange(desc(energy_twh), .by_group = TRUE) |> 
  mutate(rank = row_number())

write.csv(energy_consump_source_rank, "data/energy_consump_source_rank.csv")


######## RENEWABLE ENERGY GENERATION
renewable_generation <- read.csv("data/renewable-energy-gen.csv") |> 
  filter(Entity == "World")

oldnames <- colnames(renewable_generation)[4:7]
newnames <- c("other_renewables", "solar", "wind", "hydro")
renewable_generation <- renewable_generation |> 
  rename_at(vars(oldnames), ~ newnames) |> 
  pivot_longer(cols=other_renewables:hydro, names_to="source", values_to = "energy_twh") |> 
  mutate_all(~replace(., is.na(.), 0)) |> 
  group_by(Year) |> 
  arrange(desc(energy_twh), .by_group = TRUE) |> 
  mutate(rank = row_number())

write.csv(renewable_generation, "data/renewable_gen_world.csv")


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

######## SHARE OF LOW CARBON ENERGY
low_carbon_share <- read.csv("data/share-electricity-low-carbon.csv") |> 
  rename(low_carbon_perc = `Low.carbon.electricity....electricity.`) |> 
  pivot_wider(names_from=Year, values_from=low_carbon_perc) |> 
  pivot_longer(cols=`2000`:`1999`, names_to="Year", values_to="low_carbon_perc") |> 
  group_by(Entity) |> 
  arrange(Year) |> 
  fill(low_carbon_perc, .direction='down')

write.csv(low_carbon_share, "data/share_elec_low_carbon.csv")

low_carbon_share_filtered <- low_carbon_share |> 
  filter(Entity %in% interesting_countries)

write.csv(low_carbon_share_filtered, "data/share_elec_low_carbon_filtered.csv")


# SLOPE CHART - LEVELIZED COST OF ENERGY
slope_lcoe <- data.frame(
  "source" = c("solar_pv", "gas_peaking", "solar_thermal", "wind", "nuclear", "coal", "gas_combined_cylce", "geothermal"),
  "lcoe_2009" = c(359, 275, 168, 135, 123, 111, 83, 76),
  "lcoe_2021" = c(36, 173, 141, 38, 167, 108, 60, 75)
  ) |> 
  rename(`2009` = lcoe_2009, `2021` = lcoe_2021) |> 
  pivot_longer(cols = c(`2009`, `2021`), names_to = "year", values_to = "lcoe")

write.csv(slope_lcoe, "data/lcoe_2009_2021.csv")


# NUCLEAR DOWNFALL
nuclear_primary_energy <- read.csv("data/nuclear-primary-energy.csv") |> 
  pivot_wider(names_from=Year, values_from=nuclear_perc) |> 
  pivot_longer(cols=`1965`:`2022`, names_to="Year", values_to="nuclear_perc")|> 
  group_by(Entity) |> 
  arrange(Year) |> 
  fill(nuclear_perc, .direction='down') |> 
  ungroup() |> 
  drop_na()

write.csv(nuclear_primary_energy, "data/nuclear_primary_energy.csv")

nuclear_primary_energy_filtered <- nuclear_primary_energy |> 
  filter(Entity %in% interesting_countries) 

write.csv(nuclear_primary_energy_filtered, "data/nuclear_primary_energy_filtered.csv")
