library(sf)
library(rmapshaper)
# install.packages("hoardr")
# remotes::install_github("wmgeolab/rgeoboundaries")
library(rgeoboundaries)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)

# spatial metadata --------------------------------------------------------
# http://gapm.io/datageo
# df_datageo = googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1qHalit8sXC0R8oVXibc2wa2gY7bkwGzOybEMTWp-08o", sheet = "list-of-countries-etc")
df_datageo = read_excel("metadata/raw-data/Data Geographies - v1 - by Gapminder.xlsx",
                        sheet = "list-of-countries-etc")

# extracting GEO codes
df_datageo_geo = df_datageo |>
  select(geo, name) |>
  mutate(GEO = str_to_upper(geo)) |>
  select(-geo)

library(giscoR)
wm = gisco_get_countries(resolution = 60) |>
    select(NAME_ENGL, GEO = ISO3_CODE) |>
    mutate(GEO = if_else(GEO == "GRL", "DNK", GEO)) |>
    mutate(GEO = if_else(GEO == "PRI", "USA", GEO)) |> #Puerto Rico -> USA
    # mutate(GEO = if_else(GEO == "GRE", "GRC", GEO)) |>
    left_join(df_datageo_geo, by = "GEO") |>
    select(GEO) |>
    st_cast("MULTIPOLYGON")

# world bank regions ------------------------------------------------------
df_datageo_meta = df_datageo |>
  select(geo, name,
         wb_region = `World bank region`,
         wb_income_region = `World bank, 4 income groups 2017`) |>
  mutate(GEO = str_to_upper(geo)) |>
  select(GEO, name, wb_region, wb_income_region)

# total population --------------------------------------------------------
# https://www.gapminder.org/data
# manual download
tp = read_excel("metadata/raw-data/population_total.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "population")) |>
  filter(time == max(time)) |>
  select(-time)

# CO 2 emissions ----------------------------------------------------------
# https://www.gapminder.org/data/
# manual download
co2 = read_excel("metadata/raw-data/co2_emissions_tonnes_per_person.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "CO2_emissions")) |>
  filter(time == max(time)) |>
  select(-time)

# GDP per capita, constant PPP dollars ------------------------------------
# manual download
gdp = read_xlsx("metadata/raw-data/gdppercapita_us_inflation_adjusted.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "gdp_per_cap")) |>
  filter(time == max(time)) |>
  select(-time)

# Life expectancy ---------------------------------------------------------
# manual download
le = read_xlsx("metadata/raw-data/life_expectancy_years.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "life_expectancy")) |>
  filter(time == max(time)) |>
  select(-time)

# Corruption Perception Index ---------------------------------------------
# manual download
cpi = read_xlsx("metadata/raw-data/corruption_perception_index_cpi.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "corruption_perception_index")) |>
  filter(time == max(time)) |>
  select(-time)

# Democracy score ---------------------------------------------------------
# manual download
ds = read_excel("metadata/raw-data/democracy_score_use_as_color.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "democracy_score")) |>
  filter(time == max(time)) |>
  select(-time)

# HDI ---------------------------------------------------------------------
# manual download
hdi = read_excel("metadata/raw-data/hdi_human_development_index.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "hdi")) |>
  filter(time == max(time)) |>
  select(-time)

# Energy use --------------------------------------------------------------
# manual download
eu = read_excel("metadata/raw-data/energy_use_per_person.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "energy_use_per_cap")) |>
  filter(time == 2007) |> #less NA than 2008
  select(-time)

# Literacy rate -----------------------------------------------------------
# manual download
lr = read_excel("metadata/raw-data/literacy_rate_adult_total_percent_of_people_ages_15_and_above.xlsx") |>
  pivot_longer(-country) |>
  set_names(c("name", "time", "literacy_rate")) |>
  filter(time == max(time)) |> #less NA than 2008
  select(-time)

# joining all datasets ----------------------------------------------------
world_all = wm |>
  left_join(df_datageo_meta, by = "GEO") |>
  left_join(tp, by = "name") |>
  left_join(co2, by = "name") |>
  left_join(gdp, by = "name") |>
  left_join(le, by = "name") |>
  left_join(cpi, by = "name") |>
  left_join(ds, by = "name") |>
  left_join(hdi, by = "name") |>
  left_join(eu, by = "name") |>
  left_join(lr, by = "name")

# adding two more vars ----------------------------------------------------
world_all = world_all |>
  mutate(demo_corr = democracy_score * 2.5 + 25 + corruption_perception_index / 2,
         demo_corr_rank = rank(-demo_corr, ties.method = "min"))

# spatial transformation --------------------------------------------------
world_all8857 = world_all |>
  sf::st_transform(crs = 8857) |>
  sf::st_make_valid() |>
  ms_simplify(keep = 0.5,
              # keep_shapes = TRUE, explode = FALSE,
              method = "vis",
              # weighting = 0,
              sys = TRUE) |>
  st_make_valid() |>
  st_cast("MULTIPOLYGON")

write_sf(world_all8857, "data/worldvector.gpkg")

# agglomerations ----------------------------------------------------------
data(metro, package = "tmap")
metro_large = metro |>
  filter(pop2020 >= 20e6) |>
  st_join(world_all) |>
  select(name = name.x, name_long,
         country = name.y, wb_region,
         contains("pop"), - population) |>
  mutate(country = ifelse(name == "Mumbai", "India", country),
         wb_region = ifelse(name == "Mumbai", "South Asia", wb_region)) |>
  mutate(country = ifelse(name == "New York", "United States", country),
         wb_region = ifelse(name == "New York", "North America", wb_region))

write_sf(metro_large, "data/worldcities.gpkg")

# rasters -----------------------------------------------------------------
library(stars)
land = readRDS("metadata/raw-data/land.rds")
worldraster = land
worldraster = worldraster[c("cover_cls", "elevation")]
names(worldraster) = c("land_cover", "elevation")
st_crs(worldraster) = 4326

write_stars(worldraster[1], "data/worldlandcover.tif")
write_stars(worldraster[2], "data/worldelevation.tif")
