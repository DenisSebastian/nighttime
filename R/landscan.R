# objetivo:  Accesder a Landscan Population
# Población Global 2016
# autor: denis.berroeta@uai.cl
# fecha: 08.06.2023



# Recursos ----------------------------------------------------------------
source("R/fnc_gee.R")
source("R/fnc_grals.R")


# Librerias ---------------------------------------------------------------


library(dplyr)
library(sf)
library(mapview)
library(ggplot2)
library(viridis)
library(rgee)
email_gee = 'denis.berroeta@gmail.com'
ee_Initialize(email_gee, drive = TRUE)


# Parámetros --------------------------------------------------------------

year = 2021
n_region = "08"
description_d = "LandScanGlobal"
folder_name_d = "landscan"
path_out = "data/tif"
scale_r = 500
scale_r_out = 100
out_name = paste0(path_out, "/R", n_region,"_", 
                  description_d, "_", year, "_",
                  scale_r_out, ".tif")

# Región de Estudio -------------------------------------------------------


region_sf <- readRDS("../../insumos/regiones/Regiones_Chile.rds") %>% 
  filter(REGION == n_region) %>% 
  st_transform(4326)


ae <- region_sf  %>%
  dplyr::select(geometry) %>% 
  st_buffer(1000)
mapview(ae)

ae_ee <- ae %>% sf_as_ee()



# Lectura producto --------------------------------------------------------

# dataset = ee$Image('users/giacomofalchetta/LandScanGlobal2016')$
#   clip(ae_ee)

dataset = ee$ImageCollection('projects/sat-io/open-datasets/ORNL/LANDSCAN_GLOBAL')$
  # filterDate('2019')$
  sort('system:time_start', FALSE)$
  first()$
  clip(ae_ee)

dataset$getInfo()



land_scan = dataset$
  clip(ae_ee)

land_scan$getInfo()


# Visaulización -----------------------------------------------------------

colors_pop <- c("#1c0dff", "#05450a", "#086a10", "#54a708",
               "#78d203", "#009900", "#c6b044", "#dcd159",
               "#dade48", "#fbff13")




vis <- list(min = 0, max = 20000, palette = colors_pop) 

Map$centerObject(ae_ee, zoom = 8)
Map$addLayer(land_scan, vis, name = description_d) 


# Download local ----------------------------------------------------------


img <- dl_image(image_ee = land_scan, roi = region_sf,
                year = year, n_region = n_region,
                description_d = description_d, 
                folder_name_d = folder_name_d,
                path_out = path_out, 
                scale_r = scale_r)

library(raster)
raster_result<-  raster(img$dsn)
plot(raster_result, legend= T)




# Ajuste resolución -------------------------------------------------------

# reproject
cp_r <- raster_result
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"

if(crs(raster_result)@projargs != crs_utm){
  cp_r <- projectRaster(raster_result, crs = crs_utm)
}

# copia raster
r <- cp_r
res(r) <- scale_r_out

s <- raster::resample(cp_r, r, method = "bilinear")
s <-  round(s)



s <- raster::mask(s, region_sf %>% st_transform(32719))

writeRaster(s,  filename = out_name, overwrite=TRUE)
mapview(s, col.regions =colors_pop)

# s2 <- s
# s2[s2<1] = NA
# mapview(s2, col.regions =colors_pop)



# Generar Carta -----------------------------------------------------------


# rastero to df (na omit)
raster_df_raw <- raster::as.data.frame(s, xy = TRUE) %>% na.omit()
library(viridis)
#paleta
# pal_ndvi<- colorRampPalette(c("gray20", "yellow", "green","springgreen4"))( 200 )

map_plot <- ggplot() +
  geom_tile(data = raster_df_raw , 
            aes(x = x, y = y, 
                fill = layer)) + 
  scale_fill_viridis_c(name = "LandScan", option = "D")+
  coord_equal()+
  ggtitle(paste0("R",n_region, ": ", description_d, " ", year) ) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))
map_plot


path_out_plot = "images"
out_name_plot = paste0(path_out_plot, "/R", n_region,"_", 
                       description_d, "_", year, "_",
                       scale_r_out, ".png")



ggsave(plot = map_plot,filename =out_name_plot)

