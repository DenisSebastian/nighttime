


read_raster_file <- function(file_path) {
  file <- raster::raster(file_path)
  return(file)
}

# 
# read_brick_file <- function(file_path) {
#   file <- raster::brick(file_path)
#   return(file)
# }
# 


read_image <-  function(name, region, year, path = "data/raster/origin/",
                        img_ready){
  if(!exists("img_ready")) stop("Imagen no ha sido descargada")
  path_name <- paste0(path, "R", region, "_", name, "_", year, ".tif")
  raster_object <-  read_raster_file(path_name)
  return(raster_object)
}




# ajustar R
resample_res <- function(image, resolution = 100, method = 'bilinear',
                         path_out = "data/raster/upsampled/", 
                         results_copy = T){
  
  make_dir(path_out)

  crs_utm <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
  cp_r <- image
  if(crs(cp_r)@projargs != crs_utm){
    cp_r <- projectRaster(cp_r, crs = crs_utm)
    
  }
  r <- cp_r
  res(r) <- resolution
  
  name_r <-  get_source_raster(image) %>% gsub(".tif", "", .)
  out <- paste0(path_out, name_r,"_res_", resolution, ".tif")
  out_results <- paste0(path_out, name_r,"_res_", resolution, ".tif")
  
  s <- raster::resample(cp_r, r, method = method, filename = out, 
                        overwrite=TRUE)
  
  if(isTRUE(results_copy)){
    path_results <- "results/insumos/"
    make_dir(path_results)
    out_results <- paste0(path_results, name_r,"_res_", resolution, ".tif")
    writeRaster(s, filename =out_results, overwrite=TRUE)
  }
  
  return(s)
}



get_source_raster <- function(raster){
  src_raster <-  raster@file@name
  name_base <-  basename(src_raster)
  return(name_base)
}



make_mosaic <-  function(path_in, roi,
                         region_num, path_out, description_d, year){
  out_name  = paste0(path_out, "/R", region_num,"_",description_d, "_", year, ".tif")
  
  raster_files <-  path_in %>% 
    list.files(., pattern = ".tif$", full.names = TRUE, recursive = F)
  
  rasters <- lapply(raster_files, terra::rast)
  raster_sprc <- terra::sprc(rasters)
  raster_mosaic <- terra::mosaic(raster_sprc, fun = "max")
  raster_mosaic_crop <- terra::crop(raster_mosaic, terra::vect(roi), mask=TRUE,
                                    filename=out_name, overwrite=TRUE)

  img <- raster::raster(raster_mosaic_crop)
  
  return(img)
}



# reproyectar raster
reproject_utm <- function(raster, crs_string=NULL){
  if(is.null(crs_string)){
    crs_string ="+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs"
  }
  if(crs(raster)@projargs != crs_string){
    raster <- projectRaster(raster, crs = crs_string)
  }
  return(raster)
}

# resamplear con template

resample_res <-  function(r_ori, r_template, method="bilinear", 
                          round_out = F){
  
  # Trasnformar crs UTM 19 S
  r_ori <- reproject_utm(r_ori)
  r_template <- reproject_utm(r_template)
  
  r_adj <- raster::resample(r_ori, r_template, method = method)
  if(isTRUE(round_out)){
    r_adj <- round(r_adj)
  }
  return(r_adj) 
  
}
