### script frequency ensemble ###

# Victor Leandro
# adaptado de Maurício Vancine
# 13-02-2020

###----------------------------------------------------------------------------###

# memory
rm(list = ls())

# packages
library(colorRamps)
library(raster)
library(tidyverse)

# list packages
search()

###----------------------------------------------------------------------------###

# import data
# directory
setwd(choose.dir())

# enms
tif <- dir(patt = ".tif$")
tif

enm <- raster(tif[[1]])
enm

plot(enm, main = names(enm), col = colorRamps::matlab.like2(100))

# import evaluates
csv <- purrr::map_dfr(dir(patt = ".csv"), readr::read_csv)
csv

###----------------------------------------------------------------------------###

## frequency ensemble 
# species
sp <- csv %>% 
  dplyr::select(species) %>% 
  dplyr::distinct() %>% 
  dplyr::pull()
sp

# algorithms
al <- csv %>% 
  dplyr::select(algorithm) %>% 
  dplyr::distinct() %>%
  dplyr::pull() %>% 
  stringr::str_replace("_", "")
al

# ensembles
ens <- enm
ens[] <- 0
ens
plot(ens)

# directory
dir.create("00_ensemble_freq")

# for
for(i in sp){
  
  # select model by species
  tif.sp <- grep(i, tif, value = TRUE)
  eva.sp <- csv %>% dplyr::filter(species == i)
  
  # information
  print(paste0("The ensemble for ", i, " started, relax, take a coffee, it may take awhile..."))
  
  
  for(j in al){
    
    # select model by algorithms
    tif.al <- grep(j, tif.sp, value = TRUE)
    eva.al <- eva.sp %>% dplyr::filter(algorithm == j)
    
    # information
    print(paste0("The ensemble for '", i, "', algorithm '", j, "' are going!"))
    
    # import raster
    enm.al <- stack(tif.al)
    
    
    for(k in seq(length(tif.al))){
      
      # sum
      ens <- sum(ens, enm.al[[k]] >= eva.al[k, 5] %>% dplyr::pull())
      
    }
    
  }
  
  # export
  setwd("00_ensemble_freq")
  writeRaster(ens / (length(tif.sp)), paste0("ensemble_freq_", i, ".tif"), 
              options = c("COMPRESS=DEFLATE"), format = "GTiff", overwrite = TRUE)
  setwd("..")
  
  # information
  print(paste0("Nice! The ensemble of ", i, " it's done!"))
  
  
  ens[] <- 0
  
  print("Yeh! It's over!!!")
  
}

###----------------------------------------------------------------------------###

# directory
setwd("00_ensemble_freq")

# import
mo <- stack(dir(patt = ".tif"))
mo

# map
# occurrences
setwd(choose.dir())

occ <- readr::read_csv("Automolus_clean_lim.csv") %>% 
  dplyr::filter(name == "Automulus_lammi")
occ

mapa <- ggplot() +
  geom_raster(data = raster::rasterToPoints(mo) %>% tibble::as_tibble(), 
              aes(x, y, fill = ensemble_freq_Automolus_lammi)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like2(100)) +
 geom_point(data = occ, aes(long, lat), size = 2, alpha = .7) +
  theme_minimal() +
  theme(legend.title = element_blank())

plot(mapa)
###----------------------------------------------------------------------------###
