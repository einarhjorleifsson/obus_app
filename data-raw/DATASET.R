# # Tables for shiny
# # Objectives: Preprocessing to speed up "interactiveness"
# library(duckdb)
# library(duckdbfs)
# library(tidyverse)
# source("R/dr_tidy.R")
# # The datras tables ----------------------------------------------------------
# hh <-
#   duckdbfs::open_dataset("https://heima.hafro.is/~einarhj/datras_latin/HH.parquet") |>
#   dr_tidy() |>
#   select(.id, survey, year, quarter, lon = shootlong, lat = shootlat)
# hl <-
#   duckdbfs::open_dataset("https://heima.hafro.is/~einarhj/datras_latin/HL.parquet") |>
#   dr_tidy() |>
#   mutate(length = floor(length),
#          length = as.integer(length),
#          b = hlnoatlngt * 1e-5 * length^3) |>
#   select(.id, latin = scientificname_worms, length, n = hlnoatlngt, b) |>
#   # length has been floored
#   group_by(.id, latin, length) |>
#   summarise(n = sum(n, na.rm = TRUE),
#             b = sum(b, na.rm = TRUE),
#             .groups = "drop") |>
#   left_join(hh |> select(.id, survey, year, quarter))
# by.length <-
#   hl |>
#   group_by(survey, year, quarter, latin, length) |>
#   summarise(n = sum(n, na.rm = TRUE),
#             b = sum(b, na.rm = TRUE),
#             .groups = "drop")
# by.station <-
#   hl |>
#   group_by(.id, latin) |>
#   summarise(n = sum(n, na.rm = TRUE),
#             b = sum(b, na.rm = TRUE),
#             .groups = "drop")

# hh |>
#   mutate(survey = paste0(survey, "-", quarter)) |>
#   select(-quarter) |>
#   write_dataset("/net/hafri.hafro.is/export/home/hafri/einarhj/public_html/datras/haul.parquet")
# by.length |>
#   mutate(survey = paste0(survey, "-", quarter)) |>
#   select(-quarter) |>
#   write_dataset("/net/hafri.hafro.is/export/home/hafri/einarhj/public_html/datras/by_length.parquet")
# by.station |> write_dataset("/net/hafri.hafro.is/export/home/hafri/einarhj/public_html/datras/by_station.parquet")
# system("chmod -R a+r /net/hafri.hafro.is/export/home/hafri/einarhj/public_html/datras")

# # test -------------------------------------------------------------------------
# hh <- open_dataset("https://heima.hafro.is/~einarhj/datras/haul.parquet")
# by.length <- open_dataset("https://heima.hafro.is/~einarhj/datras/by_length.parquet")
# by.station <- open_dataset("https://heima.hafro.is/~einarhj/datras/by_station.parquet")
# input <- list()
# input$survey <- S <- "NS-IBTS-1"
# input$latin <- L <- "Gadus morhua"
# source("R/dr_plot.R")
# hh |>
#   filter(year == 2024) |>
#   drp_bubble(by.station)
# drp_length(by.length)
# drp_boot(hh, by.station)

# Auxllary stuff ---------------------------------------------------------------
## Coastlines ------------------------------------------------------------------
# if(FALSE) {
#   library(sf)
#   bb <-
#     hh |>
#     collect() |>
#     drop_na() |>
#     st_as_sf(coords = c("lon", "lat"),
#              crs = 4326) |>
#     st_bbox()
#   sf::sf_use_s2(FALSE)
#   ne <-
#     rnaturalearth::ne_coastline(scale = 50) |>
#     st_cast("POLYGON") |>
#     st_make_valid() |>
#     st_crop(bb) |>
#     select(geometry)
#   sf::sf_use_s2(TRUE)
#   ne |> write_sf("data/ne.gpkg")
# }

