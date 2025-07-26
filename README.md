# Datasets

## 1. World Data ([script](metadata/R/01-prepare_world_data.R))

- **data/worldvector.gpkg**: Simplified world borders with merged attributes (e.g., GDP, population, HDI).
- **data/worldcities.gpkg**: Agglomeration data for large cities, using population and other socioeconomic variables.
- **data/worldlandcover.tif**: A raster representing global land cover.
- **data/worldelevation.tif**: A raster representing global elevation.

## 2. Slovenia Data ([script](metadata/R/02-get_slovenia_data.R))

- **data/slovenia/slo_border.gpkg**: Slovenia's country boundary.
- **data/slovenia/slo_regions_ts.gpkg**: Time-series dataset of Slovenia's regions enriched with population, GDP per capita, tourism, and age demographics.
- **data/slovenia/slo_regions.gpkg**: Filtered dataset of Slovenia's regions for the year 2022.
- **data/slovenia/slo_cities.gpkg**: Locations and attributes of Slovenian cities with a population over 10,000.
- **data/slovenia/slo_railroads.gpkg**: Railroads data for Slovenia with calculated track width.
- **data/slovenia/slo_elev.tif**: Elevation raster for Slovenia.
- **data/slovenia/slo_gm.tif**: Geomorphon-based raster derived from elevation data which describes local landforms.
- **data/slovenia/slo_tavg.tif**: Raster of average temperature data for Slovenia.
- **data/slovenia/slo_mosaic.tif**: A mosaic satellite imagery product for Slovenia. 