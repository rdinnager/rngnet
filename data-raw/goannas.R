## code to prepare `goannas` dataset goes here
library(dplyr)

# dryad_package_dois('10.5061/dryad.83s7k')
# dryad_files('10.5061/dryad.83s7k')

reps <- sf::read_sf("D:/Projects/deepSDFSDM/data/modeled_reptiles.shp")
Oz <- rnaturalearth::ne_countries(scale = 50, country = "Australia", returnclass = "sf") %>%
  rmapshaper::ms_filter_islands(100000000) %>%
  sf::st_union()

# Oz_reps <- reps %>%
#   sf::st_transform(7856) %>%
#   sf::st_make_valid() %>%
#   sf::st_intersection(Oz %>%
#                         sf::st_transform(7856) %>%
#                         sf::st_make_valid())

Oz_reps <- reps %>%
  sf::st_intersection(Oz)

Oz_goannas <- Oz_reps[grep("Varanus ", Oz_reps$Binomial), ]

usethis::use_data(Oz_goannas)
