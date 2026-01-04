# Packages ----


library(tidyverse)

library(ggimage)

library(dismo)

library(spThin)

library(sdm)

library(usdm)

library(rJava)

library(mapview)

library(terra)

library(raster)

library(sf)

library(sp)

library(geobr)

library(ggspatial)

library(geodata)

library(DT)

library(readxl)

library(patchwork)

library(cowplot)

library(ggiraph)

library(flextable)


set.seed(123)


# Dados ----

## Dados de ocorrência ----

### Allobates olfersioides ----


olfersioides <- dismo::gbif(genus= "Allobates", species= "olfersioides", download = TRUE, geo= TRUE, removeZeros= TRUE)

olfersioides %>% datatable()


### Allobates alagoanus ----


alagoanus <- dismo::gbif(genus= "Allobates", species= "alagoanus", download = TRUE, geo= TRUE, removeZeros= TRUE)

alagoanus %>% datatable()


### Dados de literatura e externos ----


dados_externos <- readxl::read_xlsx("Dados de ocorrencia.xlsx") %>% as.data.frame()

dados_externos

externos <- dados_externos %>%
  dplyr::select(lon, lat)

externos %>% dplyr::glimpse()

externos


### tratando os dados ----

#### Filtrando os dados ----


olfersioides <- olfersioides %>%
  dplyr::select(lon, lat) %>%
  dplyr::filter(lat > -18.33) %>%
  unique()

olfersioides



alagoanus <- alagoanus %>%
  dplyr::select(lon, lat) %>%
  dplyr::filter(lat > -18.33) %>%
  unique()

alagoanus


#### Unificando os dados ----


allobates <- rbind(olfersioides, alagoanus, externos) %>%
  unique()

allobates <- allobates[-c(4),]

allobates


#### Removendo autocorrelação espacial ----


ne_shp <- geobr::read_state() %>% dplyr::filter(name_region == "Nordeste")

allobates_tratado <- allobates %>%
  dplyr::mutate(SPEC = "Allobates olfersioides",
         LAT = lat, LONG = lon) %>%
  dplyr::select(3:5)

allobates_tratado

allobates_tratado %>% thin(thin.par = 50, out.dir = here::here(), reps = 34)

allobates_thin <- read_csv("thinned_data_thin1.csv") %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::mutate(lon = LONG,
                lat = LAT) %>%
  dplyr::select(c(3:4))

allobates_thin

#### Transformando os dados ----


allobates_thin$allobates_thin <- 1

coordinates(allobates_thin) <- c("lon", "lat")

proj4string(allobates_thin) <- raster::raster() %>% raster::projection()

allobates_thin |> class()

allobates_thin %>% plot()


## Dados Ambientais ----

### Variáveis climáticas ----


bioclim <- raster::getData("worldclim", var = "bio", res = 0.5, lat= -10.9488, lon= -37.6891)

bioclim %>% plot()


### Shapefiles ----

#### Unidades de conservação ----


uc <- geobr::read_conservation_units() %>%
  sf::st_make_valid()

uc %>% plot()


#### Mata Atlântica ----


ma <- geobr::read_biomes() %>%
  dplyr::filter(name_biome == "Mata Atlântica")

ma %>% plot()


#### Estados ----


estados_brasil <- geobr::read_state()

estados_nordeste <- geobr::read_state() %>% dplyr::filter(name_region == "Nordeste")

estados_brasil %>% plot()

estados_nordeste %>% plot()


#### Região Nordeste ----


nordeste <- raster::shapefile("Região Nordeste.shp")

nordeste <- spTransform(nordeste, crs(bioclim[[2]]))

nordeste %>% plot

nordeste_sf <- geobr::read_region(year = 2019) %>%
  dplyr::filter(name_region == "Nordeste")

nordeste_sf %>% plot()


#### Território brasileiro ----


brasil <- sf::read_sf("BR_Pais_2021.shp")

brasil %>% plot()


#### Continentes ----


continentes <- sf::st_read("World_Continents.shp")

continentes %>% plot()


### Inclinação do terreno ----

#### Altitude do terreno ----


alt <- geodata::elevation_30s(country = "BRA", path = here::here()) %>%
  raster::raster()

alt %>% plot()


#### Inclinação ----


inclinacao <- alt %>% terra::terrain(v = "slope", unit = "degrees")

inclinacao %>% plot()


### Unificando os dados ----

#### Recortando os dados para o Nordeste ----


bioclim <- raster::crop(bioclim, nordeste)

bioclim <- raster::mask(bioclim, nordeste)

plot(bioclim[[1]])
points(allobates_thin)


inclinacao <- terra::crop(inclinacao, nordeste)

inclinacao <- raster::mask(inclinacao, nordeste)

plot(inclinacao)
points(allobates_thin)


### Dados de índice de vegetação (EVI) ----

#### Importando os dados ----


evi <- raster::raster("evi_indice.tif")

evi %>% plot()


#### Recortando a malha ----


evi <- raster::crop(evi, nordeste)

evi <- raster::mask(evi, nordeste)

evi %>% plot()
points(allobates_thin)


#### Alterando a resolução da malha ----


evi <- resample(evi, bioclim, method = "ngb")

evi %>% plot()

bioclim %>% extent()

evi %>% extent()

# Definir a nova extensão do raster ----

limite_lon <- c(-48.75833, -32.39167)
limite_lat <- c(-18.35, -1.041667)

nova_extensao <- extent(limite_lon, limite_lat)

# Reorganizar a extensão do raster ----
evi <- crop(evi, nova_extensao)

evi %>% plot()

evi %>% extent()


### Unificando as malhas ambientais ----


bioclim[[20]] <- evi

bioclim[[21]] <- inclinacao

names(bioclim) <- c(paste0("Bio", 1:19), "EVI", "Inclinação")

bioclim %>% plot()

bioclim[[17:21]] %>% plot()


# Modelagem ----

## Extraindo os valores ----


extract_data <- terra::extract(bioclim, allobates_thin)

extract_data


## Testando a colinearidade ----

### Colinearidade ----


nomes_colunas <- extract_data %>% colnames()

nomes_colunas[nomes_colunas == "Inclinação"] <- "Slope"

nomes_colunas

extract_data_2 <- extract_data

colnames(extract_data_2) <- nomes_colunas

matrix_cor <- extract_data_2 %>%
  cor(method = "spearman")

matrix_cor[upper.tri(matrix_cor)] <- NA

matrix_cor %>%
  reshape2::melt(na.rm = TRUE) %>%
  dplyr::mutate(`Spearman's Correlation Index` = value %>% round(2)) %>%
  dplyr::select(-3) %>%
  ggplot(aes(Var1, Var2, fill = `Spearman's Correlation Index`, label = `Spearman's Correlation Index`)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(color = "black") +
  scale_fill_viridis_c(breaks = seq(-1, 1, 0.25), limits = c(-1, 1)) +
  guides(fill = guide_colorbar(barwidth = 30, title.position = "top")) +
  theme(axis.text = element_text(size = 15, color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.title = element_blank(),
        panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "bottom",
        legend.title = element_text(color = "black", size = 15, hjust = 0.5),
        legend.text = element_text(color = "black", size = 15, hjust = 0.5),
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.45))


ggsave(filename = "autocorrelacao.png", height = 8, width = 10)


### Excluindo ----


bioclim_tratado <- bioclim[[c(9, 14, 16, 18, 19, 20, 21)]]

bioclim_tratado %>% plot()


## Criando o modelo ----

### Criando o objeto `sdmData` ----


sdmdata <- sdm::sdmData(allobates_thin ~ .,
                        allobates_thin,
                        predictors = bioclim_tratado,
                        bg = list(method = "gRandom", n = 1000))

sdmdata


### Modelagem para o `MaxEnt` ----


allobates_maxent <- sdm(allobates_thin~.,
                        sdmdata,
                        methods = "maxent",
                        replication = "sub",
                        test.p = 25,
                        n = 5)

allobates_maxent


## Criando o raster ----

### Gerando o raster para as 5 repetições ----


maxent_raster <- predict(allobates_maxent, bioclim_tratado, overwrite = TRUE)

maxent_raster %>% plot()


### Ensemble ----


ensemble <- ensemble(allobates_maxent, newdata = bioclim_tratado, setting = list(method = 'weighted' ,stat ='AUC'), overwrite = TRUE)

ensemble %>% plot()


### ggplot ----

#### Criando o dataframe ----


gg_maxent <- ensemble %>%
  as.data.frame(xy = TRUE) %>%
  na.omit()

names(gg_maxent) <- c("Lon", "Lat", "Environmental suitability")

gg_maxent %>% glimpse()

gg_maxent


#### Inset map ----


inset_map <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_raster(data = gg_maxent, aes(Lon, Lat, fill = `Environmental suitability`)) +
  scale_fill_viridis_c() +
  geom_sf(data = estados_brasil %>% dplyr::filter(code_region == 2), color = "black", fill = NA) +
  geom_rect(aes(xmin = -34, xmax = -49, ymin = -18.5, ymax = -1), color = "red", fill = "red", alpha = 0.4, size = 0.85) +
  coord_sf(xlim = c(-80, -35), ylim = c(-55, 12)) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

inset_map


#### Gerando o ggplot ----


gg_modelo <- ggplot() +
  geom_sf(data = estados_brasil, color = "black") +
  geom_raster(data = gg_maxent, aes(Lon, Lat, fill = `Environmental suitability`)) +
  geom_sf(data = estados_nordeste, color = "black", fill = NA, linewidth = 0.5) +
  geom_sf(data = ma_tratado, aes(color = "Atlantic Forest"), fill = NA, linewidth = 0.5) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  scale_color_manual(values = "orange") +
  coord_sf(xlim = c(-48.75, -32.375), ylim = c(-18.33333, -1.041667)) +
  guides(fill = guide_colorbar(barwidth = 15, title.position = "top")) +
  labs(x = NULL,
       y = NULL,
       colour = NULL) +
  ggspatial::annotation_scale(location = "br", height = unit(0.3,"cm"), bar_cols = c("black", "white")) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.ontop = T,
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.45),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

gg_modelo %>%
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map,
                     x = 0.631,
                     y = 0.7255,
                     width = 0.274,
                     height = 0.274)


ggsave(filename = "modelo_allobates.png", height = 8, width = 10)


# Avaliações do modelo ----

## Histograma de predição ----

### Avaliação dos quartos ----


cont <- gg_maxent %>%
  dplyr::select(`Environmental suitability`) %>%
  dplyr::count() %>%
  dplyr::pull()

cont

# 1º quarto ----

q_1 <- gg_maxent %>%
  dplyr::filter(`Environmental suitability` < 0.25) %>%
  dplyr::count() %>%
  dplyr::pull()

q_1

q1 <- (q_1 * 100) / cont

# 2º quarto ----

q_2 <- gg_maxent %>%
  dplyr::filter(`Environmental suitability` >= 0.25 & `Environmental suitability` < 0.5) %>%
  dplyr::count() %>%
  dplyr::pull()

q_2

q2 <- (q_2 * 100) / cont

# 3º quarto ----

q_3 <- gg_maxent %>%
  dplyr::filter(`Environmental suitability` >= 0.5 & `Environmental suitability` < 0.75) %>%
  dplyr::count() %>%
  dplyr::pull()

q_3

q3 <- (q_3 * 100) / cont

# 4º quarto ----

q_4 <- gg_maxent %>%
  dplyr::filter(`Environmental suitability` >= 0.75) %>%
  dplyr::count() %>%
  dplyr::pull()

q_4

q4 <- (q_4 * 100) / cont

3.235754 %>% round(2)


#### Graficando ----


distri_prob <- tibble::tibble(`Area representatiom` = c("<25%", ">=25% & <50%", ">=50% & <75%", ">=75%"),
                              `Background Area Representation (%)` = c(q1 %>% round(3), q2 %>% round(3), q3 %>% round(3), q4 %>% round(3)))

distri_prob

distri_prob %>%
  ggplot(aes(`Area representatiom`, `Background Area Representation (%)`, group = 1)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  geom_line(color = "blue", size = 1) +
  geom_label(aes(label = `Background Area Representation (%)`), size = 4) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))


### Histograma de Predição ----


gg_maxent %>%
  ggplot(aes(`Environmental suitability`)) +
  geom_histogram(color = "black") +
  labs(y = "Pixels count") +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

ggsave(filename = "histograma_predição.png", height = 8, width = 10)

### Unificando os Gráficos ----


g_1 <- gg_maxent %>%
  ggplot(aes(`Environmental suitability`)) +
  geom_histogram(color = "black") +
  labs(y = "Pixels count") +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

g_2 <- distri_prob %>%
  ggplot(aes(`Area representatiom`, `Background Area Representation (%)`, group = 1)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  geom_line(color = "blue", size = 1) +
  geom_label(aes(label = `Background Area Representation (%)`), size = 4) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

g_2 + g_1 + patchwork::plot_layout(ncol = 1) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 30, face = "bold"))

ggsave(filename = "histograma_unidos.png", height = 10, width = 8)


## Curva ROC ----


sdm::roc(allobates_maxent)


## Importância das variáveis ----


var_importancia <- sdm::getVarImp(allobates_maxent) %>% plot()

gg_var_imp <- var_importancia$data

gg_var_imp <- gg_var_imp %>%
  arrange(corTest %>% desc())

levels <- gg_var_imp$variables

gg_var_imp <- gg_var_imp %>%
  mutate(variables = factor(variables, levels = levels %>% rev()))

gg_var_imp %>%
  ggplot(aes(variables, corTest)) +
  geom_col(color = "black") +
  geom_label(aes(variables, corTest, label = corTest), fill = "grey35") +
  labs(x = "Predictors",
       y = "Model response") +
  coord_flip() +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

ggsave(filename = "variaveis_importância.png", height = 8, width = 10)


## Curva de resposta ----


curv_resp <- sdm::rcurve(allobates_maxent)

curv_resp_data <- curv_resp$data

curv_resp_data

curv_resp_data %>%
  ggplot(aes(Value, Response)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(x = "Predictor value",
       y = "Model response")+
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

ggsave(filename = "curva_resposta.png", height = 8, width = 10)


## Unificando Importância das variáveis e curva de resposta ----


g_var_1 <- gg_var_imp %>%
  dplyr::mutate(variables = dplyr::case_when(variables == "Inclinação" ~ "Slope",
                                             .default = variables),
                variables = variables %>% forcats::fct_relevel(levels %>% rev)) %>%
  ggplot(aes(variables, corTest)) +
  geom_col(color = "black") +
  geom_label(aes(variables, corTest, label = corTest), fill = "grey35") +
  labs(x = "Predictors",
       y = "Model Response") +
  coord_flip() +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

g_var_2 <- curv_resp_data %>%
  dplyr::mutate(variable = dplyr::case_when(variable == "Inclinação" ~ "Slope",
                                             .default = variable)) %>%
  ggplot(aes(Value, Response)) +
  geom_line(linewidth = 1, color = "blue") +
  labs(x = "Predictor Value",
       y = "Model Response")+
  facet_wrap(~variable, nrow = 2, scales = "free") +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 10),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_line(color = "grey75", linetype = "dashed", linewidth = 0.7),
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

g_var_1 + g_var_2 + patchwork::plot_layout(ncol = 1) + plot_annotation(tag_levels = 'a') & theme(plot.tag = element_text(size = 30, face = "bold"))

ggsave(filename = "vars_unidos.png", height = 10, width = 10)


## Comparação com o poligono da IUCN ----

### Criando o poligono do modelo ----

#### Criando diferentes modelos ----


for(i in c(0.7, 0.75, 0.8, 0.85, 0.9)){

  a <- ensemble %>%
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  dplyr::filter(layer >= i) %>%
  rasterFromXYZ(crs = estados_nordeste %>% sf::st_crs()) %>%
  rasterToPolygons() %>%
  sf::st_as_sf(crs = estados_nordeste %>% sf::st_crs()) %>%
  sf::st_union()


  b <- sf::st_sf(geometry = a, crs = estados_nordeste %>% sf::st_crs()) %>%
    dplyr::mutate(DN = 1)

  stringr::str_glue("poligono: {i}") %>% message()

  b %>% print()

  c <- b %>%
  ggplot() +
  geom_sf(color = "black", fill = "yellow") +
  labs(title = stringr::str_glue("modelo com {i}")) +
  theme_minimal()

  c %>% print()

  assign(paste0("modelo_", i), b)

}


#### Testando qual modelo abriga mais registros ----


registros <- read_csv("thinned_data_thin1.csv") %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::mutate(lon = LONG,
                lat = LAT) %>%
  dplyr::select(c(3:4))

registros

registros_sf <- registros %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = estados_nordeste %>% sf::st_crs())

registros_sf %>% plot()

lista <- list(`0.70` = modelo_0.7, `0.75` = modelo_0.75, `0.80` = modelo_0.8, `0.85` = modelo_0.85, `0.90` = modelo_0.9)

for(i in lista %>% seq_along()){

  a <- sf::st_join(registros_sf, lista[[i]], join = st_within)

  b <- registros[is.na(a$DN), ]

  stringr::str_glue("registros para o modelo {i}")

  b %>% print()

  nomes <- lista[i] %>% names()

  nomes %>% print()

  assign(paste0(nomes, "%"), b)

}



#### Tratando e unidicando os dados dos registros para todos os thresholds ----


lista2 <- list(`70` = `0.70%`, `75` = `0.75%`, `80` = `0.80%`, `85` = `0.85%`, `90` = `0.90%`)

for(i in lista2 %>% seq_along()){

  nomes <- lista2[i] %>% names()

  a <- registros %>%
  mutate(`Inside the model polygon` = dplyr::case_when(lon %in% lista2[[i]]$lon & lat %in% lista2[[i]]$lat ~ "No",
                                   .default = "Yes"),
         modelo = paste0("≥ ", nomes, "%"))

  a %>% print()

  assign(paste0("registro_", nomes), a)

}

pontos <- dplyr::bind_rows(registro_70, registro_75, registro_80, registro_85, registro_90)

pontos


#### Tratando e unificando os modelos para os 5 threshold ----


modelos <- list(modelo_0.7, modelo_0.75, modelo_0.8, modelo_0.85, modelo_0.9) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(modelo = paste0("≥ ", seq(70, 90, 5), "%"))

modelos

ma_tratado <- sf::st_intersection(ma, nordeste_sf)


#### Visualizando ----


modelos %>%
  ggplot() +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = estados_nordeste, fill = "white", color = "black", linewidth = 0.5) +
  geom_sf(aes(color = "Model Suitability", fill = "Model Suitability"), alpha = 0.3) +
  geom_sf(data = ma_tratado, aes(color = "Atlantic Forest", fill = "Atlantic Forest"), alpha = 0, linewidth = 0.75) +
  scale_color_manual(values = c("darkgreen", "orangered")) +
  scale_fill_manual(values = c("transparent", "orangered")) +
  coord_sf(xlim = c(-48.5, -34.9), ylim = c(-18, -1)) +
  labs(x = NULL,
       y = NULL,
       colour = NULL,
       fill = NULL) +
  scale_x_continuous(breaks = seq(-48, -35, 6)) +
  ggnewscale::new_scale_fill() +
  geom_point(data = pontos, aes(lon, lat, fill = `Inside the model polygon`), shape = 21, color = "black", size = 2) +
  facet_wrap(~modelo) +
  scale_fill_manual(values = c("blue", "yellow")) +
  guides(color = guide_legend(title.position = "top", title.vjust = 0.5),
         fill = guide_legend(title.position = "top", title.vjust = 0.5)) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.ontop = T,
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text = element_text(size = 13, color = "black"))

ggsave(filename = "modelos_adequabilidade.png", height = 10, width = 12)


### poligono da IUCN ----

#### Importando ----


iucn <- sf::read_sf("iucn.shp")

iucn

iucn %>%
  ggplot() +
  geom_sf(color = "black")


#### Recortando ----


st_crs(iucn) <- sf::st_crs(nordeste_sf)

poligono_iucn <- sf::st_intersection(iucn, nordeste_sf)

sf::st_crs(poligono_iucn) <- sf::st_crs(nordeste_sf)

poligono_iucn

poligono_iucn %>%
  ggplot() +
  geom_sf(color = "black")


### Calculando a área de poligono ----

#### poligono do modelo ----


area_modelo <- sf::st_area(poligono_modelo) / 1000000 # em Km2

area_modelo


#### poligono da IUCN ----


area_iucn <- sf::st_area(poligono_iucn) / 1000000 # em Km²

area_iucn


#### Inset map ----


inset_map_iucn <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = estados_nordeste, aes(fill = "Northeastern region", color = "Northeastern region")) +
  geom_sf(data = poligono_iucn, aes(fill = "IUCN polygon", color = "IUCN polygon")) +
  geom_sf(data = poligono_modelo, aes(fill = "Model polygon", color = "Model polygon"), alpha = 0.4) +
  geom_sf(data = poligono_iucn, color = "darkviolet", fill = NA) +
  scale_fill_manual(values = c("Northeastern region" = "white",
                               "IUCN polygon" = "transparent",
                               "Model polygon" = "yellow")) +
  scale_color_manual(values = c("Northeastern region" = "black",
                                "IUCN polygon" = "darkviolet",
                                "Model polygon" = "orangered")) +
  labs(fill = NULL,
       color = NULL) +
  geom_rect(aes(xmin = -34, xmax = -49, ymin = -18.5, ymax = -1), color = "red", fill = "red", alpha = 0.4, size = 0.85) +
  coord_sf(xlim = c(-80, -35), ylim = c(-55, 12)) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

inset_map_iucn


#### Analisando se há contenção ----


sf::st_crs(poligono_iucn) <- sf::st_crs(poligono_modelo)

sf::st_within(poligono_modelo, poligono_iucn)

gg_iucn <- ggplot() +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = estados_nordeste, aes(fill = "Northeastern region", color = "Northeastern region"), linewidth = 0.5) +
  geom_sf(data = poligono_iucn, aes(fill = "IUCN polygon", color = "IUCN polygon"), linewidth = 1) +
  geom_sf(data = poligono_modelo, aes(fill = "Model polygon", color = "Model polygon"), alpha = 0.4) +
  geom_sf(data = poligono_iucn, color = "darkviolet", fill = NA, linewidth = 1) +
  scale_fill_manual(values = c("Northeastern region" = "white",
                               "IUCN polygon" = "transparent",
                               "Model polygon" = "yellow")) +
  scale_color_manual(values = c("Northeastern region" = "black",
                                "IUCN polygon" = "darkviolet",
                                "Model polygon" = "orangered")) +
  labs(fill = NULL,
       color = NULL) +
  coord_sf(xlim = c(-48.75, -32.375), ylim = c(-18.33333, -1.041667)) +
  annotation_scale(location = "br", height = unit(0.3,"cm"), bar_cols = c("black", "white")) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

gg_iucn %>%
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map_iucn,
                     x = 0.642,
                     y = 0.715,
                     width = 0.285,
                     height = 0.285)

ggsave(filename = "diferencas_poligonos.png", height = 8, width = 10)



#### Diferença entre essas áreas ----


area_modelo > area_iucn

area_diferença <- 100 - ((area_modelo * 100) / area_iucn) %>% as.numeric() # Subtrair por 100 para calcular a redução

area_diferença


#### Calculando o interseção ----


poligono_intersec <- sf::st_intersection(poligono_iucn, poligono_modelo)

poligono_intersec

poligono_intersec %>%
  ggplot() +
  geom_sf(color = "black")

area_intersec <- poligono_intersec %>% sf::st_area() / 1000000 # em Km²

area_intersec

(area_intersec * 100) / area_modelo

(area_intersec * 100) / area_iucn


## Comparação com as únidades de conservação ----

### Tratamento ----

#### Tratamento do CRS: uc x ma ----


sf::st_crs(uc) <- sf::st_crs(ma)


#### Mata Atlântica ----


ma_tratado <- sf::st_intersection(ma, nordeste_sf)

ma_tratado

ma_tratado %>%
  ggplot() +
  geom_sf(color = "black")

ma_tratado %>% sf::st_area() / 1000000


#### Unidades de conservação ----


uc_tratado <- sf::st_intersection(uc, ma_tratado)

uc_tratado <- uc_tratado %>% sf::st_union() %>% sf::st_make_valid() %>% sf::st_as_sf()

uc_tratado

uc_tratado %>% sf::st_area() / 1000000

uc_tratado %>%
  ggplot() +
  geom_sf(color = "black")


#### Tratando os CRS: uc_tratado x poligono_modelo ----


sf::st_crs(uc_tratado) <- sf::st_crs(poligono_modelo)


### Comparando a % de intersecção ----

#### Criando a intersecção ----


uc_int_modelo <- sf::st_intersection(uc_tratado, poligono_modelo)

uc_int_modelo <- uc_int_modelo %>% sf::st_union() %>% sf::st_make_valid() %>% sf::st_as_sf()

uc_int_modelo

uc_int_modelo %>%
  ggplot() +
  geom_sf(color = "black")


#### Calculando a área da intersecção ----


uc_int_modelo_area <- uc_int_modelo %>% sf::st_area() / 1000000 # para estar em Km²

uc_int_modelo_area


#### Calculando a % de área que o poligono do modelo está nas unidades de conservação ----


(uc_int_modelo_area * 100) / area_modelo


#### Calculando a % de área que as unidades de conservação estão no modelo ----


(uc_int_modelo_area * 100) / (uc_tratado %>% sf::st_area() / 1000000)


#### Inset map ----


inset_map_uc <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = estados_nordeste, aes(fill = "Northeastern region", color = "Northeastern region")) +
  geom_sf(data = poligono_modelo, aes(fill = "Model polygon", color = "Model polygon"), alpha = 0.4) +
  geom_sf(data = uc_tratado, aes(fill = "Conservation units", color = "Conservation units"), alpha = 0.3) +
  scale_fill_manual(values = c("Northeastern region" = "white",
                               "Conservation units" = "transparent",
                               "Model polygon" = "yellow")) +
  scale_color_manual(values = c("Northeastern region" = "black",
                                "Conservation units" = "darkviolet",
                                "Model polygon" = "orangered")) +
  labs(fill = NULL,
       color = NULL) +
  geom_rect(aes(xmin = -34, xmax = -49, ymin = -18.5, ymax = -1), color = "red", fill = "red", alpha = 0.4, size = 0.85) +
  coord_sf(xlim = c(-80, -35), ylim = c(-55, 12)) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

inset_map_uc


### Graficando ----


gg_uc <- ggplot() +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = estados_nordeste, aes(fill = "Northeastern region", color = "Northeastern region"), linewidth = 0.5) +
  geom_sf(data = poligono_modelo, aes(fill = "Model polygon", color = "Model polygon"), alpha = 0.4) +
  geom_sf(data = uc_tratado, aes(fill = "Conservation units", color = "Conservation units"), alpha = 0.4) +
  scale_fill_manual(values = c("Northeastern region" = "white",
                               "Conservation units" = "lightblue",
                               "Model polygon" = "yellow")) +
  scale_color_manual(values = c("Northeastern region" = "black",
                                "Conservation units" = "darkviolet",
                                "Model polygon" = "orangered")) +
  labs(fill = NULL,
       color = NULL) +
  coord_sf(xlim = c(-48.75, -32.375), ylim = c(-18.33333, -1.041667)) +
  annotation_scale(location = "br", height = unit(0.3,"cm"), bar_cols = c("black", "white")) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_blank(),
          panel.grid = element_blank(),
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

gg_uc %>%
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map_uc,
                     x = 0.642,
                     y = 0.715,
                     width = 0.285,
                     height = 0.285)

ggsave(filename = "diferencas_poligonos_uc.png", height = 8, width = 10)


# Mapa de EVI ----

## Inset Map ----


inset_map_evi <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_raster(data = evi %>% as.data.frame(xy = TRUE) %>% na.omit() %>% mutate(EVI = evi_indice / 10000), aes(x, y, fill = EVI)) +
  scale_fill_gradientn(colours = c("#6e3500", "#9a4a00", "#b07100", "#c69700", "#d7b500", "#e7d100", "#f9de00", "#fff500", "#f0f600", "#cde200", "#9dc800", "#77b400", "#4f9f00", "#1d8500", "#156200"), breaks = seq(-1, 1, 0.2)) +
  geom_sf(data = estados_nordeste, color = "black", fill = NA) +
  geom_sf(data = ma_tratado, color = "blue", fill = NA, size = 1.5) +
  geom_rect(aes(xmin = -34, xmax = -49, ymin = -18.5, ymax = -1), color = "red", fill = "red", alpha = 0.4, size = 0.85) +
  coord_sf(xlim = c(-80, -35), ylim = c(-55, 12)) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

inset_map_evi


## Mapa composto ----


gg_evi <- evi %>%
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  mutate(EVI = evi_indice / 10000) %>%
  ggplot() +
  geom_sf(data = estados_brasil, color = "black") +
  geom_raster(aes(x, y, fill = EVI)) +
  geom_sf(data = estados_nordeste, color = "black", fill = NA, linewidth = 0.5) +
  geom_sf(data = ma_tratado, aes(color = " "), fill = NA, linewidth = 0.75) +
  scale_fill_gradientn(colours = c("#6e3500", "#9a4a00", "#b07100", "#c69700", "#d7b500", "#e7d100", "#f9de00", "#fff500", "#f0f600", "#cde200", "#9dc800", "#77b400", "#4f9f00", "#1d8500", "#156200"), breaks = seq(-0.2, 1, 0.2), limits = c(-0.2, 1)) +
  scale_color_manual(values = c(" " = "blue")) +
  coord_sf(xlim = c(-48.75, -32.375), ylim = c(-18.33333, -1.041667)) +
  labs(x = NULL,
       y = NULL,
       color = "Atlantic Forest") +
  guides(fill = guide_colorbar(barwidth = 15, title.position = "top")) +
  guides(color = guide_legend(title.position = "top", label.hjust = 0.5)) +
  ggspatial::annotation_scale(location = "br", height = unit(0.3,"cm"), bar_cols = c("black", "white")) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.ontop = T,
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.45),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom")

gg_evi %>%
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map_evi,
                     x = 0.631,
                     y = 0.7255,
                     width = 0.274,
                     height = 0.274)

ggsave("evi.png", height = 8, width = 10)



evi %>%
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  mutate(EVI = evi_indice / 10000) %>%
  dplyr::slice_min(EVI)

evi %>%
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  mutate(EVI = evi_indice / 10000) %>%
  dplyr::slice_max(EVI)


# Mapa de Inclinação ----

## Inset map ----


inset_map_alt <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_raster(data = inclinacao %>% as.data.frame(xy = TRUE) %>% na.omit() %>% mutate(Inclinação = slope), aes(x, y, fill = Inclinação)) +
  scale_fill_gradientn(colours = c("#6e3500", "#9a4a00", "#b07100", "#c69700", "#d7b500", "#e7d100", "#f9de00", "#fff500", "#f0f600", "#cde200", "#9dc800", "#77b400", "#4f9f00", "#1d8500", "#156200"), breaks = seq(0, 20, 5), limits = c(0, 20)) +
  geom_sf(data = estados_nordeste, color = "black", fill = NA) +
  geom_sf(data = ma_tratado, color = "blue", fill = NA) +
  geom_rect(aes(xmin = -34, xmax = -49, ymin = -18.5, ymax = -1), color = "red", fill = "red", alpha = 0.4, size = 0.85) +
  coord_sf(xlim = c(-80, -35), ylim = c(-55, 12)) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

inset_map_alt


## Mapa Composto ----


gg_inclinação <- inclinacao %>%
  as.data.frame(xy = TRUE) %>%
  na.omit() %>%
  mutate(Inclinação = slope) %>%
  ggplot() +
  geom_sf(data = estados_brasil, color = "black") +
  geom_raster(aes(x, y, fill = Inclinação)) +
  geom_sf(data = estados_nordeste, color = "black", fill = NA, linewidth = 0.5) +
  geom_sf(data = ma_tratado, aes(color = " "), fill = NA, linewidth = 0.75) +
  scale_fill_gradientn(colours = c("#6e3500", "#9a4a00", "#b07100", "#c69700", "#d7b500", "#e7d100", "#f9de00", "#fff500", "#f0f600", "#cde200", "#9dc800", "#77b400", "#4f9f00", "#1d8500", "#156200"), breaks = seq(0, 22, 2), limits = c(0, 22)) +
  scale_color_manual(values = c(" " = "blue")) +
  coord_sf(xlim = c(-48.75, -32.375), ylim = c(-18.33333, -1.041667)) +
  labs(x = NULL,
       y = NULL,
       fill = "Terrain slope",
       color = "Atlantic Forest") +
  guides(fill = guide_colorbar(barwidth = 15, title.position = "top")) +
  guides(color = guide_legend(title.position = "top", title.vjust = 0.5)) +
  ggspatial::annotation_scale(location = "br", height = unit(0.3,"cm"), bar_cols = c("black", "white")) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.ontop = T,
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.45),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

gg_inclinação %>%
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map_alt,
                     x = 0.631,
                     y = 0.7255,
                     width = 0.274,
                     height = 0.274)

ggsave("inclinação.png", height = 8, width = 10)


# Mapa de registros ----


allobates_thinned <- read_csv("thinned_data_thin1.csv") %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::mutate(lon = LONG,
                lat = LAT) %>%
  dplyr::select(c(3:4))

allobates_thinned

inset_map_registros <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = ne_shp, color = "black", fill = "white") +
  geom_sf(data = ma_tratado, color = "darkgreen", fill = NA) +
  scale_fill_viridis_c() +
  geom_sf(data = estados_brasil %>% dplyr::filter(code_region == 2), color = "black", fill = NA) +
  geom_rect(aes(xmin = -34, xmax = -49, ymin = -18.5, ymax = -1), color = "red", fill = "red", alpha = 0.4, size = 0.85) +
  coord_sf(xlim = c(-80, -35), ylim = c(-55, 12)) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        plot.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.position = "none")

mapa_registros <- ggplot() +
  geom_sf(data = continentes, color = "black", fill = "gray40") +
  geom_sf(data = estados_brasil, color = "black") +
  geom_sf(data = estados_nordeste, color = "black", fill = "white", linewidth = 0.5) +
  geom_sf(data = ma_tratado, aes(color = "Atlantic Forest"), fill = "darkgreen", linewidth = 1, alpha = 0.3) +
  geom_point(data = allobates, aes(lon, lat, fill = "Not thinned", size = "Not thinned"), shape = 21, color = "black") +
  geom_point(data = allobates_thinned, aes(lon, lat, fill = "Thinned", size = "Thinned"), shape = 21, color = "black") +
  ggsflabel::geom_sf_label_repel(data = estados_brasil %>%
                                   dplyr::filter(code_region == 2) %>%
                                   dplyr::mutate(name_state = name_state %>% stringr::str_replace("Do", "do")),
                                 aes(label = name_state),
                                 nudge_x = -2,
                                 color = "black",
                                 size = 3) +
  scale_fill_manual(values = c("Not thinned" = "yellow",
                               "Thinned" = "blue")) +
  scale_size_manual(values = c("Not thinned" = 5,
                               "Thinned" = 3)) +
  scale_color_manual(values = "darkgreen") +
  labs(x = NULL,
       y = NULL,
       fill = NULL,
       size = NULL,
       colour = NULL) +
  coord_sf(xlim = c(-48.75, -32.375), ylim = c(-18.33333, -1.041667)) +
  theme(plot.title = ggtext::element_markdown(color = "black", size = 15, hjust = 0.5),
          axis.text = element_text(color = "black", size = 12),
          axis.title = element_text(color = "black", size = 10),
          panel.grid = element_blank(),
          panel.grid.minor = element_blank(),
          panel.ontop = T,
          axis.line = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.45),
          legend.key = element_rect(color = "white", fill = "white"),
          legend.title = element_text(hjust = 0.5),
          legend.position = "bottom",
          strip.background = element_rect(colour = "black", fill = "gray90"),
          strip.text.x = element_text(size = 13))

mapa_registros %>%
  cowplot::ggdraw() +
  cowplot::draw_plot(inset_map_registros,
                     x = 0.6443,
                     y = 0.718,
                     width = 0.282,
                     height = 0.282)

ggsave(filename = "registros.png", height = 8, width = 10)



ggplot() +
  geom_sf(data = nordeste_sf) +
  geom_label(data = allobates %>%
               mutate(point = 1:35) %>%
               dplyr::filter(point %in% c(1:5)),
             aes(lon, lat, label = point %>% as.character(), fill = point), alpha = 0.3) +
  scale_fill_viridis_c()


# Tabela com todos os registros ----

## GBif ----

### olfersioides ----

#### Dados iniciais ----


olfersioides_tab <- dismo::gbif(genus= "Allobates", species= "olfersioides", download = TRUE, geo= TRUE, removeZeros= TRUE)

olfersioides_tab <- olfersioides_tab %>%
  dplyr::filter(lat > -18.33) %>%
  dplyr::select(4, c(lat, lon, municipality)) %>%
  dplyr::mutate(Source = "GBIF",
                State = adm1,
                Municipality = municipality) %>%
  dplyr::select(-c(1, 4)) %>%
  dplyr::relocate(Source, .after = State) %>%
  dplyr::relocate(Municipality, .before = Source) %>%
  unique()

olfersioides_tab

olfersioides_tab[c(3, 6), 3] <- "Alagoas"

olfersioides_tab[9, 3] <- "Bahia"

olfersioides_tab


#### Municipios ----


muni <- geobr::read_municipality() %>%
  sf::st_make_valid()

olfersioides_tab_coord <- olfersioides_tab %>%
  dplyr::select(lon, lat) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = muni %>% sf::st_crs())

olfersioides_tab_coord %>% plot()

muni_olfersioides <- sf::st_join(olfersioides_tab_coord, muni, join = st_within) %>%
  dplyr::pull(name_muni)

muni_olfersioides


#### Adicionando os municípios ----


olfersioides_tab <- olfersioides_tab %>%
  dplyr::mutate(Municipality = muni_olfersioides)

olfersioides_tab


### alagoanus ----


alagoanus_tab <- dismo::gbif(genus= "Allobates", species= "alagoanus", download = TRUE, geo= TRUE, removeZeros= TRUE)

alagoanus_tab <- alagoanus_tab %>%
  dplyr::filter(lat > -18.33) %>%
  dplyr::select(3, c(lat, lon, municipality)) %>%
  dplyr::mutate(Source = "GBIF",
                State = adm1,
                Municipality = municipality) %>%
  dplyr::select(-c(1, 4)) %>%
  dplyr::relocate(Source, .after = State) %>%
  dplyr::relocate(Municipality, .before = Source) %>%
  unique()

alagoanus_tab


## Externos ----


dados_externos_tab <- dados_externos %>%
  dplyr::mutate(State = UF,
                Municipality = Município,
                Source = Fonte,
                Source =  dplyr::case_when(Source == "Registro de campo" ~ "Field Record",
                                           Source == "CHUPE" ~ "UFPE",
                                           Source == "Dubeux, M. J. M., et al (2020)" ~ "Dubeux et al. 2020",
                                           Source == "DIAS, I. B., et al (2014)" ~ "Dias et al. 2014",
                                           Source == "OLIVEIRA, P. M. A., et al (2021)" ~ "Oliveira et al. 2021",
                                           Source == "FREITAS, M. A., et al (2019)" ~ "Freitas et al. 2019",
                                           .default = Source)) %>%
  dplyr::select(-c(3:7))

dados_externos_tab


## Dados unidos ----

### Unificando ----


uni_tab <- rbind(olfersioides_tab, alagoanus_tab, dados_externos_tab)

uni_tab


### Removendo os dados repetidos para um dataframe de "fila" ----


uni_tab_unique <- uni_tab %>%
  dplyr::select(c(lat, lon)) %>%
  dplyr::mutate(ordem = 1:40) %>%
  dplyr::distinct(lat, lon, .keep_all = TRUE)

uni_tab_unique <- uni_tab_unique[-4, ]

uni_tab_unique


### Removendo os dados repetidos no dataframe principal ----


uni_tab_filt <- uni_tab %>%
  dplyr::mutate(ordem = 1:40) %>%
  dplyr::filter(ordem %in% uni_tab_unique$ordem) %>%
  dplyr::select(-6)

uni_tab_filt


### Adicionando uma variável para se foi filtrado ----


allobates_thinned <- read_csv("thinned_data_thin1.csv") %>%
  dplyr::select(where(is.numeric)) %>%
  dplyr::mutate(lon = LONG,
                lat = LAT) %>%
  dplyr::select(c(3:4))

uni_tab_filt <- uni_tab_filt %>%
  dplyr::mutate(Thinned = dplyr::case_when(lat %in% allobates_thinned$lat & lon %in% allobates_thinned$lon ~ "Yes",
                                    .default = "No"))

uni_tab_filt


### Adicionando uma variável para se está em alguma unidade de conservação ----


uc_teste <- sf::st_intersection(uc, ma_tratado)

uc_teste %>% plot()

uni_tab_filt_coord <- uni_tab_filt %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = uc_teste %>% sf::st_crs())

conservation_units_list <- sf::st_join(uni_tab_filt_coord, uc_teste, join = st_within) %>%
  as.data.frame() %>%
  dplyr::select(c(1:4), 6) %>%
  dplyr::mutate(`Conservation Unity` = name_conservation_unit %>% stringr::str_to_title() %>% stringr::str_replace_all(c("De" = "de", "Do" = "do", "E" = "e", "Da" = "da"))) %>%
  dplyr::select(-5)

conservation_units_list

uni_tab_filt <- uni_tab_filt %>%
  dplyr::mutate(`Into a Conservation Unity` = conservation_units_list$`Conservation Unity`,
                `Into a Conservation Unity` = case_when(`Into a Conservation Unity` %>% is.na() == TRUE ~ "No",
                                                        .default = `Into a Conservation Unity`),
                Municipality = Municipality %>% stringr::str_replace_all(c("De" = "de", "Do" = "do", "E" = "e", "Da" = "da")),
                State = dplyr::case_when(State == "BA"  ~ "Bahia",
                                         State == "SE" ~ "Sergipe",
                                         State == "AL" ~ "Alagoas",
                                         State == "PE" ~ "Pernambuco",
                                         .default = State))

uni_tab_filt


## Tabela flextable ----

### Criando ----


uni_tab_filt_flex <- uni_tab_filt %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.05) %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 12, part = "all")

uni_tab_filt_flex


### Salvando ----


uni_tab_filt_flex %>%
  flextable::save_as_docx(path = "tabela_geral_apendice.docx")


# Tabela de Unididades de Conservação com algum registro ----

## Criando a tabela flextable ----


conservation_units_list_flex <- conservation_units_list %>%
  na.omit() %>%
  dplyr::mutate(State = State %>% dplyr::case_when(. == "BA" ~ "Bahia",
                                                   . == "AL" ~ "Alagoas",
                                                   . == "SE" ~ "Sergipe",
                                                   . == "PE" ~ "Pernambuco",
                                                   .default = .)) %>%
  flextable::flextable() %>%
  flextable::align(align = "center", part = "all") %>%
  flextable::width(width = 1.1) %>%
  flextable::font(fontname = "Times New Roman", part = "all") %>%
  flextable::fontsize(size = 12, part = "all")

conservation_units_list_flex


## Salvando a tabela ----


conservation_units_list_flex %>%
  flextable::save_as_docx(path = "tabela_unidades_conservação_registro.docx")


