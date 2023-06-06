


dl_imgSat <- function(roi,  n_region,  year = "2022", comunas,
                      imageSat = 'NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG',
                      band = "avg_rad", scale_r = 500, 
                      folder_name_d = "ILN", description_d = "VIIRS",
                      path_out = "data/raster/origin", sleep_time = 10,
                      email_gee = 'denis.berroeta@gmail.com'){
  rgee::ee_Initialize(email_gee, drive = TRUE)
  
  
  
  reg_g <- c("10", "11", "12")
  
  # Condición
  
  if(n_region %in% reg_g){
    print(paste0("La región R", n_region, " Se procesarà por comuna por su extensión"))
    path_out_com = paste0("data/raster/temp/R", n_region)
    
    for(i in 1:nrow(comunas)){
      
      roi_com = comunas[i, ]
      print(paste0("Descargando raster de la comuna de ", unique(roi_com$NOM_COMUNA)))
      name_cod = unique(roi_com$COMUNA) 
      dataset_com = get_image(roi = roi_com, year = year,
                              simplify_roi = T, 
                              imageSat = imageSat,
                              band = band)
      # vis_map(image = dataset, roi = dataset %>% rgee::sf_as_ee(),
      #         name = "LUCES",max=300, zoom = 11)
      
      #descarga temporal
      dl_image(
        image_ee = dataset_com,
        roi =  roi_com,
        year = year,
        simplify_roi = T, 
        n_region = n_region,
        description_d = paste0(description_d, "_", name_cod),
        folder_name_d = folder_name_d,
        path_out = path_out_com,
        scale_r = scale_r,
        sleep_time = sleep_time)
    }
    # crear Mosaico
    img <- make_mosaic(path_in = path_out_com, region_num = n_region, roi = roi, 
                       path_out = path_out, description_d = description_d, 
                       year = year)
    
  }else{
    dataset = get_image(roi = roi, year = year, imageSat = imageSat, band = band)
    img <- dl_image(image_ee = dataset, roi = roi,
                    year = year, n_region = n_region,
                    description_d = description_d, 
                    folder_name_d = folder_name_d,
                    path_out = path_out, 
                    scale_r = scale_r,
                    sleep_time = sleep_time)
    
  }
  
  return(img)
}




get_image <- function(roi, year, imageSat, band,
                      simplify_roi = F, 
                      md_init = '-10-01',
                      md_end = '-12-31'){
  print("vars")
  
  if(isTRUE(simplify_roi)){
    print("Modify ROI")
    roi_ee <- roi %>% make_box() %>% rgee::sf_as_ee()
    region <- roi_ee
    
  }else{
    roi_ee <- roi %>% rgee::sf_as_ee()
    region <- roi_ee$geometry()$bounds()
  }
  
  
  # date
  date_start <- paste0(year, md_init)
  date_end <- paste0(year, md_end)
  
  #image
  print("image")
  dataset <-  ee$ImageCollection(imageSat)$
    filterDate(date_start, date_end)$
    select(band)$
    median()$
    clip(roi_ee)
  
  
  return(dataset)
}


dl_image <- function(image_ee, roi, year, n_region,  description_d, 
                     simplify_roi = F, folder_name_d, path_out, 
                     scale_r, sleep_time = 10){
  
  make_dir(path_out)
  
  # roi ee
  if(isTRUE(simplify_roi)){
    print("Modify ROI")
    roi_ee <- roi %>% make_box() %>% rgee::sf_as_ee()
    region <- roi_ee
    
  }else{
    roi_ee <- roi %>% rgee::sf_as_ee()
    region <- roi_ee$geometry()$bounds()
  }
  
  print("up drive")
  img_dl <- ee_image_to_drive(
    image = image_ee,
    description = paste("R",n_region, description_d, year, sep = "_"),
    folder = folder_name_d,
    fileFormat = "GEO_TIFF",
    crs='EPSG:4326', 
    region = region,
    scale = scale_r
  )
  
  print("status")
  img_dl$start()
  ee_monitoring(img_dl)
  img_dl$status()$state
  
  seg <- 0
  while(img_dl$status()$state!="COMPLETED"){
    print(paste0("Sleep ", sleep_time, " seg"))
    Sys.sleep(sleep_time)
    seg = seg+sleep_time
  }
  print(paste0("Total  ", seg, " seg"))
  
  
  print("download")
  img <- ee_drive_to_local(
    task = img_dl,
    metadata = TRUE,
    dsn =paste0(path_out, "/R", n_region,"_",description_d, "_", year, ".tif")
  )
  return(img)
}


vis_map <-  function(image, roi, name, min =0, max =100, zoom = 11, palette = NULL){
  if(is.null(palette)){
    palette = c('0602ff', '235cb1', '307ef3', '269db1', '30c8e2',
                '32d3ef', '3ae237','b5e22e', 'd6e21f', 'fff705',
                'ffd611', 'ffb613', 'ff8b13', 'ff6e08', 'ff500d',
                'ff0000', 'de0101', 'c21301')
  }
  vis <- list(min = min, max = max, palette = palette)
  Map$centerObject(roi, zoom = zoom)
  Map$addLayer(image, vis, name = name)
}
