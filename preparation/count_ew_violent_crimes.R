# load packages
library("raster")
library("sf")
library("tidyverse")



# bulk police.uk data can only be downloaded manually, so data for England and
# Wales for 2019 were downloaded manually into a zip file

# create temporary directory and unzip files to it
dir.create(str_glue("{tempdir()}/crime-data"))
unzip(here::here("data/police-uk-data-2019.zip"),
      exdir = str_glue("{tempdir()}/crime-data"))

# load data and merge into one object
crimes <- str_glue("{tempdir()}/crime-data") %>%
  dir(pattern = "*.csv$", full.names = TRUE, recursive = TRUE) %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() %>%
  remove_missing(na.rm = TRUE, vars = c("longitude", "latitude")) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  # convert to British National Grid so that units are metres
  st_transform(27700)

# create a 250m grid across England and Wales
# this is done as a raster because it's much faster than st_make_grid()
crime_grid <- raster(crimes, resolution = 250)

# load police force areas
forces <- st_read("https://opendata.arcgis.com/datasets/1fc3e34edece4cd58dabac422c3556c9_0.geojson") %>%
  janitor::clean_names() %>%
  select(force = pfa19nm) %>%
  # convert to British National Grid to match the crimes layer
  st_transform(27700)

# local authority districts
districts <- st_read("https://opendata.arcgis.com/datasets/0e07a8196454415eab18c40a54dfbbef_0.geojson") %>%
  janitor::clean_names() %>%
  # filter out all except England and Wales
  filter(str_detect(lad19cd, "^E") | str_detect(lad19cd, "^W")) %>%
  select(district = lad19nm) %>%
  # convert to British National Grid to match the crimes layer
  st_transform(27700)

# count violent crimes in grid
crimes_in_grid <- crimes %>%
  filter(crime_type == "Violence and sexual offences") %>%
  rasterize(crime_grid, "crime_type", fun = "count") %>%
  as.data.frame(xy = TRUE) %>%
  as_tibble() %>%
  rename(crimes = layer) %>%
  arrange(desc(crimes)) %>%
  mutate(
    crimes = ifelse(is.na(crimes), 0, crimes),
    perc = crimes / sum(crimes),
    cperc = cumsum(perc)
  ) %>%
  st_as_sf(coords = c("x", "y"), remove = FALSE, crs = 27700) %>%
  st_join(forces) %>%
  st_join(districts) %>%
  st_set_geometry(NULL)

# save data
write_rds(crimes_in_grid, here::here("data/ew_violence_in_grid.Rds"))


