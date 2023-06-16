# objetivo:  Accesder a world Population
# Población Global 2016
# autor: denis.berroeta@uai.cl
# fecha: 13.06.2023



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

year = 2016
n_region = "08"
description_d = "WorldPop"
folder_name_d = "worldpop"
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
  select(geometry) %>% 
  st_buffer(1000)
mapview(ae)

ae_ee <- ae %>% sf_as_ee()





clipToCol = function(image){
  return (image$clip(ae_ee))
}


# Lectura producto --------------------------------------------------------

dataset = ee$ImageCollection('WorldPop/GP/100m/pop')$
  map(clipToCol)$
  filterDate('2016')
  # select("population")$
  # first()$
  # clip(ae_ee)

dataset$getInfo()

mosaic = dataset$select('population')$mosaic()




# Visaulización -----------------------------------------------------------

colors_pop <- c('#24126c', '#1fff4f', '#d4ff50')

vis <- list(min = 0, max = 50, 
            palette = colors_pop) 

Map$centerObject(ae_ee, zoom = 8)
Map$addLayer(mosaic, vis, name = description_d) 


# Download local ----------------------------------------------------------


img <- dl_image(image_ee = mosaic, roi = region_sf,
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
  scale_fill_viridis_c(name = description_d, option = "D")+
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



ggsave(plot = map_plot,filename = out_name_plot)

