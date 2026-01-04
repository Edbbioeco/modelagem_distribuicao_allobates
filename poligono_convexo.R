# Pacotes ----

library(tidyverse)

library(sf)

# Dados ----

## Carrengando ----

load("salvo.RData")

## Importando ----

pontos <- readr::read_csv("thinned_data_thin1.csv") |>
  dplyr::select(where(is.numeric)) |>
  dplyr::mutate(lon = LONG,
                lat = LAT) |>
  dplyr::select(c(3:4))

pontos

# Polígono ----

## Criando o polígono convexo ----

poligono_convexo <- pontos |>
  sf::st_as_sf(coords = c("lon", "lat"), crs = brasil |> sf::st_crs()) |>
  sf::st_union() |>
  sf::st_convex_hull()

poligono_convexo

poligono_convexo |>
  ggplot() +
  geom_sf()

## Recortando para a Mata atlântica do Nordeste ----

poligono_convexo_ma <- poligono_convexo |>
  sf::st_intersection(ma_tratado)

poligono_convexo_ma

poligono_convexo_ma |>
  ggplot() +
  geom_sf()

## Mapa com os pontos ----

ggplot() +
  geom_sf(data = poligono_convexo, color = "red", fill = "transparent") +
  geom_sf(data = poligono_convexo_ma, color = "green", fill = "transparent") +
  geom_point(data = pontos, ggplot2::aes(lon, lat))

