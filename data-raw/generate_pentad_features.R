# The script is the source of the pentad features data in data/
# The methodology is simple:
#   Find the midpoint of each pentad
#   Check if the midpoint lies in a polygon (defined by county etc)

library(rgdal)
library(rgeos)
library(here)
library(tidyverse)


# get raw -----------------------------------------------------------------


# get pentad list

# this url was provided by Michael Brooks
url <- 'http://api.adu.org.za/sabap2/v2/pentads/country/kenya?format=geoJSON'
downloader::download(url, destfile = "data-raw/kenya-pentads.geojson")
pentad_json <- readOGR(dsn = "data-raw/kenya-pentads.geojson")


# get counties map
# downloaded from
# http://data.openinstitute.com/dataset/kenya-counties/resource/138d827b-167f-4425-947a-9f241e718d08
counties_json <- readOGR(dsn = "data-raw/kenyan-counties.geojson")





# extract from raw --------------------------------------------------------


# extract pentad data

pentad_table <- map_df(1:dim(pentad_json)[1],
                       function(x) tibble(Lat = pentad_json@polygons[[x]]@Polygons[[1]]@coords[1:4],
                                          Long = pentad_json@polygons[[x]]@Polygons[[1]]@coords[6:9],
                                          Pentad = as.character(pentad_json@data[["pentad"]][x])) ) %>%
  # get mid points of the pentads
  group_by(Pentad) %>%
  summarize(

    MidLat = mean(Lat),
    MidLong = mean(Long)
  )


# extract counties data
counties_table <- map_df(1:dim(counties_json)[1],
                         function(x) tibble(Lat = counties_json@polygons[[x]]@Polygons[[1]]@coords[,1],
                                            Long = counties_json@polygons[[x]]@Polygons[[1]]@coords[,2],
                                            Counties = as.character(counties_json@data[["COUNTY"]][x])) )




# map pentads to features -------------------------------------------------

# get all pentads
all_pentads = pentad_table$Pentad

# get all counties
all_counties = unique(counties_table$Counties)

# all combinations
all_combinations = expand.grid(Pentad = all_pentads, County = all_counties)

# check if each mid-point is in the polygon defined by a county
pentad_polygon_mapping <-  map2(as.character(all_combinations$Pentad),
              as.character(all_combinations$County),
              function(x, y) point.in.polygon(point.x = pentad_table[pentad_table$Pentad == x, ]$MidLat,
                                              point.y = pentad_table[pentad_table$Pentad == x, ]$MidLong,
                                              pol.x = counties_table[counties_table$Counties == y,]$Lat,
                                              pol.y = counties_table[counties_table$Counties == y,]$Long))




pentad_polygon_mapping <- unlist(pentad_polygon_mapping)

# add if back to the main pentad table
all_combinations$Contains = pentad_polygon_mapping

# join with county-province data

county_provinces <- read_csv('data-raw/county_province_list.csv')

pentads_geographical_features <- all_combinations %>%
  mutate(Pentad = as.character(Pentad)) %>%
  mutate(County = as.character(County)) %>%
  filter(Contains == 1) %>%
  left_join(., county_provinces, by = "County") %>%
  select(-Contains) %>%
  mutate(Country = 'Kenya') %>%
  select(Pentad, Country, Province, County)

save(pentads_geographical_features, file = 'data/pentads_geographical_features.RData')







