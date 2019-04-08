# The script is the source of the pentad features data in data/
# The methodology is simple:
#   Find the midpoint of each pentad
#   Check if the midpoint lies in a polygon (defined by county etc)

library(rgdal)
library(rgeos)
library(here)
library(tidyverse)
library(sf)



# get raw -----------------------------------------------------------------


# get pentad list

# this url was provided by Michael Brooks

# run the two lines below only once
# url <- 'http://api.adu.org.za/sabap2/v2/pentads/country/kenya?format=geoJSON'
# downloader::download(url, destfile = "data-raw/kenya-pentads.geojson")

pentad_json <- readOGR(dsn = "data-raw/kenya-pentads.geojson")


# get counties map
# downloaded from
# http://data.openinstitute.com/dataset/kenya-counties/resource/138d827b-167f-4425-947a-9f241e718d08
sf_counties = read_sf('data-raw/kenyan-counties.geojson')


# extract from raw --------------------------------------------------------


# extract pentad data
# 6815 pentads
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



# get county labels -------------------------------------------------------

counties_planar <- st_transform(sf_counties, 2163)

county_names_for_pentads <- apply(pentad_table[,2:3], 1, function(x){

  # transformation to palnar is required, since sf library assumes planar projection
  coords <- as.data.frame(matrix(x, nrow = 1,
                                 dimnames = list("", c("x", "y"))))
  pentads_sf <- st_transform(st_sfc(st_point(x),crs = 4326), 2163)
  # st_intersects with sparse = FALSE returns a logical matrix
  # with rows corresponds to argument 1 (points) and
  # columns to argument 2 (polygons)

  counties_planar[which(st_intersects(pentads_sf, counties_planar, sparse = FALSE)), ]$COUNTY


} )

pentad_table$CountyNames = as.character(county_names_for_pentads)


# map pentads to features -------------------------------------------------


# join with county-province data

county_provinces <- read_csv('data-raw/county_province_list.csv')

pentads_geographical_features <- pentad_table %>%
  rename("County" = "CountyNames") %>%
  mutate(County  = if_else(County == "character(0)", "Unclassified", County)) %>%
  left_join(., county_provinces, by = "County") %>%
  mutate(Country = "Kenya")

save(pentads_geographical_features, file = 'data/pentads_geographical_features.RData')
