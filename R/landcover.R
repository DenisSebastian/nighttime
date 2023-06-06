# objetivo:  Accesder a Landcover Modis



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
description_d = "Landcover_T2"
folder_name_d = "LANDCOVER_T2"
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


dataset <-  ee$ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG')$
  filterDate('2020-01-01', '2020-12-01')$
  select("avg_rad")$
  median()

imagen <- dataset$clip(ae_ee)



# Lectura producto --------------------------------------------------------

dataset = ee$ImageCollection('MODIS/061/MCD12Q1')
land_cove_t2 = dataset$select('LC_Type2')$
  filter(ee$Filter$calendarRange(year,year,'year'))$
  first()$
  clip(ae_ee)

# tab_lc_t2 <- openxlsx::read.xlsx("data/excel/tab_LC_type2.xlsx")

colors_t2 <- c("#1c0dff", "#05450a", "#086a10", "#54a708",
               "#78d203", "#009900", "#c6b044", "#dcd159",
               "#dade48", "#fbff13", "#b6ff05", "#27ff87",
               "#c24f44", "#a5a5a5", "#ff6d4c", "#f9ffa4")
  
  
  
categories <- tab_lc_t2$Description

# Visaulización -----------------------------------------------------------

vis <- list(min = 1, max = 16, palette = colors_t2) 
  
Map$centerObject(ae_ee, zoom = 8)
Map$addLayer(land_cove_t2, vis, name = "LC_Type2") 
  


# Download local ----------------------------------------------------------


img <- dl_image(image_ee = land_cove_t2, roi = region_sf,
                year = year, n_region = n_region,
                description_d = description_d, 
                folder_name_d = folder_name_d,
                path_out = path_out, 
                scale_r = scale_r)

library(raster)
raster_lc_t2 <-  raster(img$dsn)
plot(raster_lc_t2, legend= T)
# mapview(raster_lc_t2, col.regions =colors_t2 )


# Ajuste resolución -------------------------------------------------------

# reproject
cp_r <- raster_lc_t2
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
if(crs(raster_lc_t2)@projargs != crs_utm){
  cp_r <- projectRaster(raster_lc_t2, crs = crs_utm)
}


# copia raster
r <- cp_r
res(r) <- scale_r_out

s <- raster::resample(cp_r, r, method = "bilinear")
s <-  round(s)
s[s<0] = 0

writeRaster(s,  filename = out_name, overwrite=TRUE)
mapview(s, col.regions =colors_t2)



# Generar Carta -----------------------------------------------------------


# rastero to df (na omit)
lc_raw <- raster::as.data.frame(s, xy = TRUE) %>% na.omit()

#paleta
# pal_ndvi<- colorRampPalette(c("gray20", "yellow", "green","springgreen4"))( 200 )

map_lc <- ggplot() +
  geom_tile(data = lc_raw , 
            aes(x = x, y = y, 
                fill = as.character(layer))) + 
  scale_fill_manual(name = "LC_t2", values = colors_t2)+
  coord_equal()+
  ggtitle(paste0("R08: Land Cover T2") ) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))

ggsave(plot = map_lc,filename = "images/R08_landcover_t2.png")


