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




# Lectura de Insumos ------------------------------------------------------

pb <-  raster("data/tif/R08_kde_pob.tif")
pb_ls <-  raster("data/tif/R08_LandScanGlobal_2016_100.tif")
pb_ls_2021 <-  raster("data/tif/R08_LandScanGlobal_2021_100.tif")
pb_wp <- raster("data/tif/R08_WorldPop_2016_100.tif")
ln <-  raster("data/tif/R08_VIIRS_2016_res_100.tif")
ln_2021 <-  raster("data/tif/R08_VIIRS_2021_100.tif")
lc <-  raster("data/tif/R08_Landcover_T2_2016_100.tif")

mapview(pb_wp)

# Resample ----------------------------------------------------------------
pb <- resample_res(r_ori = pb, r_template = pb_ls, round_out = T)



# no Access function ------------------------------------------------------
# library(terra)
# x <- ifel(rast(pb) < 1, NA, 
#           ifel(rast(ln) < 0.1, 1, NA))
# rx <-  raster(x)

noaccess <- function(pob, u_pob = 1, ln, u_ln= 0.1) {
  mask <- pob >= u_pob & ln <  u_ln
  # mask[mask == 0] <- NA
  pop_masked <- mask(pob, mask, maskvalue=0)
  return(pop_masked)
}

nacc  <-  noaccess(pob = pb, ln = ln)
mapview(nacc)
# Test --------------------------------------------------------------------

# imputación viviendas KDE
nacc_kde  <-  noaccess(pob = pb, ln = ln, u_ln= 0.5)
mapview(nacc_kde) + mapview(pb,  hide = TRUE) + mapview(ln, hide = TRUE)


# landscan
nacc_ls  <-  noaccess(pob = pb_ls, u_pob = 100, ln = ln , u_ln = 0.1)
mapview(nacc_ls,   alpha.regions = 1) + mapview(pb_ls,  hide = TRUE) + mapview(ln, hide = TRUE)

nacc_wp_2021 <-  noaccess(pob = pb_ls_2021,u_pob = 100, ln = ln_2021, u_ln = 1)
mapview(nacc_wp_2021) + mapview(pb_ls_2021,  hide = TRUE) + mapview(ln, hide = TRUE)



 # world pop
nacc_wp <-  noaccess(pob = pb_wp, ln = ln)
mapview(nacc_wp) + mapview(pb_wp,  hide = TRUE) + mapview(ln, hide = TRUE)



mapview(nacc_kde) + mapview(nacc_ls) + mapview(nacc_wp)+
  mapview(pb_ls,  hide = TRUE) + mapview(ln, hide = TRUE)
