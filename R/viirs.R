# Recursos ----------------------------------------------------------------
source("R/fnc_gee.R")
source("R/fnc_grals.R")


# Librerias ---------------------------------------------------------------
library(dplyr)
library(sf)
library(raster)
library(mapview)
library(ggplot2)
library(viridis)


# Parámetros --------------------------------------------------------------

year = 2021
n_region = "08"
image_name = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG'
description_d = "VIIRS"
folder_name_d = "ILN"
path_out = "data/tif"
scale_r = 500
scale_r_out = 100
out_name = paste0(path_out, "/R", n_region,"_", 
                  description_d, "_", year, "_",
                  scale_r_out, ".tif")
layer = "avg_rad"
legend_name = "Indicador de Luces Nocturnas (ILN)"



### Definición de Área de Estudio

# Región de Estudio -------------------------------------------------------
region_sf <- readRDS("../../insumos/regiones/Regiones_Chile.rds") %>% 
  filter(REGION == n_region) %>% 
  st_transform(4326)



# La región de estudio es geometría única y se le aplicará un buffer (`st_buffer()`) de 1000 metros, a fin de asegurarnos que no queden partes de polígonos sin cubrir con el raster.


ae <- region_sf  %>%
  dplyr::select(geometry) %>% 
  st_buffer(1000)
mapview(ae, col.region = "darkgreen")



### Producto Satelital


library(rgee)
email_gee = 'denis.berroeta@gmail.com'
ee_Initialize(email_gee, drive = TRUE)



ae_ee <- ae %>% sf_as_ee()

## Noise Correction

# noise_corr <- function(x, umbral=0.25) {
#   x[x<umbral] <- 0
#   return(x)
# }
# 
replacement = ee$Image(0);

conditional = function(image) {
  return(image$where(image$lt(0.25), replacement))
}


dataset <-  ee$ImageCollection(image_name)$
  filterDate('2021-10-01', '2021-12-01')$
  select("avg_rad")$
  map(conditional)$
  median()

imagen <- dataset$clip(ae_ee)




palette_viirs = c('0602ff', '235cb1', '307ef3', '269db1', '30c8e2',
                        '32d3ef', '3ae237','b5e22e', 'd6e21f', 'fff705',
                        'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
                        'ff0000', 'de0101', 'c21301') %>% 
  paste0("#", .)

vis <- list(min = 0, max = 60,
            palette = palette_viirs)



region <- ae_ee$geometry()$bounds()

Map$centerObject(region, zoom = 10)
Map$addLayer(imagen, vis, name = "Nighttime")




## Descarga de Raster

img <- dl_image(image_ee = imagen, roi = region_sf,
                year = year, n_region = n_region,
                description_d = description_d, 
                folder_name_d = folder_name_d,
                path_out = path_out, 
                scale_r = scale_r)




raster_viirs <-  raster(img$dsn)



# Ajuste Resolución



# reproject
cp_r <- raster_viirs
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
if(crs(raster_viirs)@projargs != crs_utm){
  cp_r <- projectRaster(raster_viirs, crs = crs_utm)
}


# copia raster
r <- cp_r
res(r) <- scale_r_out

s <- raster::resample(cp_r, r, method = "bilinear")
s <-  round(s)
s[s<0] = 0

writeRaster(s,  filename = out_name, overwrite=TRUE)
mapview(s, col.regions =palette_viirs)





## Visualización de resultados 




s <-  raster(out_name)

# rastero to df (na omit)
viirs_raw <- raster::as.data.frame(s, xy = TRUE) %>% na.omit() 
names(viirs_raw)[3] <- "layer"  
viirs_raw$layer <- as.numeric(viirs_raw$layer)

map_lc <- ggplot() +
  geom_tile(data = viirs_raw , 
            aes(x = x, y = y, 
                fill = layer)) + 
  scale_fill_viridis_c(name = "VIIRS",option = "B")+
  coord_equal()+
  ggtitle(paste0("R08: VIIRS avg") ) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))

path_out_plot = "images"
out_name_plot = paste0(path_out_plot, "/R", n_region,"_", 
                       description_d, "_", year, "_",
                       scale_r_out, ".png")



ggsave(plot = map_lc,filename =out_name_plot)






