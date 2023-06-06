library(sf)
library(dplyr)
library(purrr)
library(mapview)
library(raster)
library(exactextractr)

source("R/fnc_spatial.R")

### - Lectura de Insumos

vr <- st_read("../../insumos/vivienda_rural/Vivienda_Rural_Publica.shp") %>% 
  st_transform(32719)
mz <- list.files(path = "../../insumos/manzanas/", 
                 pattern = ".rds$", full.names = T) %>%  
  map_df(readRDS)

# - Imputar Información Administrativa
vr_info <- add_info_intersects(mz, vr)
# saveRDS(vr_info, "../../insumos/vivienda_rural/viv_rur_info.rds")
# vr_info <-  readRDS("../../insumos/vivienda_rural/viv_rur_info.rds")


# - Filtrar por la región de estudio
vr_r08 <-  vr_info %>% filter(REGION == "08")
saveRDS(vr_r08, "data/rds/R08_viv_rural.rds")
vr_r08 <-  readRDS("data/rds/R08_viv_rural.rds")


# - Visualización
mapview(vr_r08)




# Count Viviendas por pixel -----------------------------------------------


ln_tif <- raster("data/tif/R08_VIIRS_2016_res_100.tif")

ln_tif_copy <- ln_tif
values(ln_tif_copy)  <- NA

pts_rasterized <- rasterize(vr_r08, ln_tif_copy, field = 1, fun = "count")


mapview(pts_rasterized)

# Obtener la cantidad de puntos en cada píxel
values_df <- as.data.frame(pts_rasterized, xy = TRUE)
values_sf  <- st_as_sf(values_df, coords = c("x", "y"), crs = 32719) %>% 
  mutate(id_raster = 1:n()) 

dim(values_sf)

mz_r08 <- readRDS("../../insumos/manzanas/MZ_REGION_08.rds")

# imputar ID
vr_info_viv <- add_info_intersects(mz_r08, values_sf)

mapview(vr_info_viv, zcol= "layer" )


# 
# por_urb <- vr_info_viv %>% 
#   dplyr::select( id_raster,  id, layer, PERSONAS, MANZ_EN) %>% 
#   left_join(tab_pond, by ="id") %>% 
#   mutate(pob_rural = (PERSONAS/viviendas_mz)*layer) %>% 
#   mutate(pob_rural = ifelse(pob_rural<1, 1, round(pob_rural)))






# RURAL -------------------------------------------------------------------

# lectura
mz_r08 <- readRDS("../../insumos/manzanas/MZ_REGION_08.rds")
vr_r08 <-  readRDS("data/rds/R08_viv_rural.rds")




# distribución de población por viviendas equitativamente

tab_pond <- vr_r08 %>% 
  st_drop_geometry() %>% 
  group_by(ID_MANZCIT, TOTAL_V, PERSONAS) %>% 
  summarise(viv_puntos = n(), .groups = "keep") %>% 
  mutate(diff_viv = TOTAL_V-viv_puntos) %>% 
  select(ID_MANZCIT, viv_censo = TOTAL_V, viv_puntos, diff_viv, personas = PERSONAS) %>% 
  arrange(desc(diff_viv))

tab_pond


pob_rural <- vr_r08 %>% 
  left_join(tab_pond, by ="ID_MANZCIT") %>% 
  mutate(pob_rural = (personas/viv_censo)) %>% 
  mutate(pob_rural = ifelse(pob_rural<1, 1, round(pob_rural)))

# saveRDS(pob_rural, "data/rds/R08_viv_rural.rds")

# URBANO ------------------------------------------------------------------

# generar viviendas urbanas dentro de cada manzana random

# urb_viv <- mz_r08 %>% 
#   filter(MANZ_EN == "URBANO") %>% 
#   filter(TOTAL_V > 0) %>% 
#   split(.$ID_MANZCIT) %>% 
#   map(~st_sample(x = ., size = .$TOTAL_V, progress = T)) %>% 
#   map(~ st_sf(.)) %>% 
#   bind_rows(.id = "ID_MANZCIT")


# mz_r08 <- readRDS("../../insumos/manzanas/MZ_REGION_08.rds")
# urb_viv <- vu_r08 %>% 
  # left_join(st_drop_geometry(mz_r08), by ="ID_MANZCIT")




# pob_urb <- urb_viv %>% 
#   mutate(pob_urb = (PERSONAS/TOTAL_V)) %>% 
#   mutate(pob_urb = ifelse(pob_urb<1, 1, round(pob_urb)))
# 
# saveRDS(pob_urb, "data/rds/R08_viv_urb.rds")


# Kernel Density ----------------------------------------------------------

library(dplyr)
library(rgdal)

library(rgeos)
library(raster)
library(spatstat)
library(spdep)
library(tmap)
library(exactextractr)

library(SpatialKDE)

# Merge urbano rural

vu_r08 <-  readRDS("data/rds/R08_viv_urb.rds") %>% 
  dplyr::select(ID_MANZCIT, poblacion = pob_urb)

vr_r08 <-  readRDS("data/rds/R08_viv_rural.rds") %>% 
  dplyr::select(ID_MANZCIT, poblacion = pob_rural)


all_viv <- bind_rows(vu_r08, vr_r08)



#Definirán Parámemetros de Estudio
# cell_size <- 100 # Tamaño de Celda
# band_width <- 500 #  Parámetro de Suavisado (denominada ventana o h)

# raster_pob <- pts_pob %>% 
#   create_raster(cell_size = cell_size, side_offset = band_width)
# 
# kde_raster <- pts_pob %>% 
#   kde(band_width = band_width, kernel = "quartic", grid = raster_pob)




## Definir proyeccion
crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
crs_ll <- "+proj=longlat +datum=WGS84 +no_defs"
sp_all_viv = as(all_viv, "Spatial")
l_reg <-   readRDS("../../insumos/regiones/Regiones_Chile.rds") %>% 
    filter(REGION == "08")



grid_spacing = 100 # aplicacion comunal, para aplicacion regional usar 500 metros
ext <- extent(l_reg)
x_min <- ext[1] - grid_spacing
x_max <- ext[2] + grid_spacing
y_min <- ext[3] - grid_spacing
y_max <- ext[4] + grid_spacing
w <- as.owin(c(x_min,x_max, y_min, y_max)) # ventana que define espacio de trabajo
r <- fasterize::raster(l_reg,res=grid_spacing)
# base de puntos
pts <- coordinates(sp_all_viv)
p <- ppp(pts[,1], pts[,2], window = w)

# Calculo de Hotspots con radios mas y menos extensos de agregacion (adjust*bandwidth)
pob_hotspots <- stats::density(p, adjust=.05, weights=sp_all_viv$poblacion) # parametro de radio de kernel
# pob_hotspots <- stats::density(p, adjust=.05) # parametro de radio de kernel
plot(pob_hotspots, main='Densidad de poblacion 0.25')


# Transformar a Raster ----------------------------------------------------
kde_pob <- raster(pob_hotspots)
proj4string(kde_pob)=proj4string(sp_all_viv) # Asignar CRS # corregido
kde_pob <- raster::mask(kde_pob, l_reg) # Cortar por contorno URBANO
# kde_del <- kde_del * factor_mult
kde_pob[kde_pob < 0] <- 0 
# Reasamplear ------------------------------------------------
kde_pob <-  resample(kde_pob, r)

normalize_minmax <- function(x) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T)-min(x, na.rm = T))
}

# values(kde_pob) <- normalize_minmax(values(kde_pob))

mapview(kde_pob)

writeRaster(kde_pob, filenime = "data/tif/R08_kde_raw.tif", overwrite = T)





# mz_r08 %>%  st_drop_geometry()%>% 
#   group_by(NOM_COMUNA) %>%
#   summarise(pob = sum(PERSONAS),
#             viv = sum(TOTAL_V)) %>% arrange(desc(viv))

# imputar población -------------------------------------------------------
kde_pob <- raster("data/tif/R08_kde.tif")
comuna <-  readRDS("../../insumos/comunas/Comunas_Chile.rds") %>% 
  filter(REGION == "08")

mz_r08 <- readRDS("../../insumos/manzanas/MZ_REGION_08.rds")

pob_com_tab <-  mz_r08 %>% 
  st_drop_geometry() %>% 
  group_by(COMUNA) %>% 
  summarise(personas = sum(PERSONAS, na.rm = T)) %>% 
  arrange(desc(personas))


comuna <- comuna %>% left_join(pob_com_tab, by= "COMUNA")

factores_com <- comuna %>% 
  mutate(raster_sum = exact_extract(kde_pob, y = comuna, fun = "sum")) %>% 
  mutate(scale_factor = personas/ raster_sum) %>% 
  dplyr::select(COMUNA, NOM_COMUNA, personas, raster_sum, scale_factor)


r_pob_estimada <-  kde_pob
values(r_pob_estimada) <- NA
  
pond_com_rasterized <- rasterize(factores_com, r_pob_estimada, 
                            field = "scale_factor")

r_est_pob <- kde_pob *pond_com_rasterized
mapview(r_est_pob)

writeRaster(r_est_pob, filename = "data/tif/R08_kde_pob.tif", overwrite = T)

# 3. Homologar con raster LN ----------------------------------------------


# 4. Guardar Resultados ---------------------------------------------------





# Visualizar --------------------------------------------------------------

library(ggplot2)
library(viridis)

# rastero to df (na omit)
kde_pob_df <- raster::as.data.frame(r_est_pob, xy = TRUE) %>% na.omit()

#paleta
# pal_ndvi<- colorRampPalette(c("gray20", "yellow", "green","springgreen4"))( 200 )

map_kde <- ggplot() +
  geom_tile(data = kde_pob_df , 
              aes(x = x, y = y, 
                  fill = layer)) + 
  scale_fill_viridis_c(name = "KDE Pob", option = "E")+
  coord_equal()+
  ggtitle(paste0("R08: Estimación de Población por KDE de Vivienda") ) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))

ggsave(plot = map_kde,filename = "images/R08_kde_pob.png")


# rastero to df (na omit)
kde_pob_df_raw <- raster::as.data.frame(kde_pob, xy = TRUE) %>% na.omit()

#paleta
# pal_ndvi<- colorRampPalette(c("gray20", "yellow", "green","springgreen4"))( 200 )

map_kde <- ggplot() +
  geom_tile(data = kde_pob_df_raw , 
            aes(x = x, y = y, 
                fill = layer)) + 
  scale_fill_viridis_c(name = "KDE Viv", option = "E")+
  coord_equal()+
  ggtitle(paste0("R08: KDE Vivienda Raw") ) +
  theme_bw() +
  theme(panel.grid.major = element_line(colour = "gray80"), 
        panel.grid.minor = element_line(colour = "gray80"))

ggsave(plot = map_kde,filename = "images/R08_kde_raw.png")







  
