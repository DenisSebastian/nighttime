
# funciones ---------------------------------------------------------------


# Dataframe to SF Object --------------------------------------------------


# dataframe a sf
df2sf <- function(df, lon ="lon", lat ="lat", crs_base = 4326) {
  sf_object <- df %>%
    dplyr::filter(!is.na(lon)|!is.na(lat)) %>%
    sf::st_as_sf(coords = c(lon, lat),
                 crs = crs_base, agr = "constant")
  return(sf_object)
}


# sf to dataframe
sf2df <- function(sf_points){
  df <- sfheaders::sf_to_df( sf_points, fill = TRUE ) %>% 
    rename(lon = x, lat = y)
  return(df)
}
# points_sf %>% 
# sf2df() %>% 
# head()


# imputar datos por intersect ---------------------------------------------

add_info_intersects <-  function(pol_info, sf_data){
  # versión eficiente de intersects
  id_int <- sapply(st_intersects(sf_data, pol_info), 
                   function(z) if (length(z)==0) NA_integer_ else z[1])
  
  pol_info <- pol_info %>% 
    mutate(id = 1:nrow(.)) %>% 
    st_drop_geometry()
  
  resultados <- sf_data%>%
    mutate(id = id_int) %>%
    left_join(pol_info, by ="id")
  return(resultados)
  
}


# Tablas Resumen ----------------------------------------------------------

resumen_base <- function(base, name_base, n_samples = 5){ 
  smp <- sample(nrow(base), size = n_samples, replace = F)
  res <- NULL
  for(i in 1:ncol(base)){
    r <- data.frame(
      col_name = as.character(names(base)[i]),
      # col_name_cor =  names(janitor::clean_names(base))[i],
      n_rows = nrow(base),
      n_unique = length(unique(base[,i])),
      n_NA = sum(is.na(base[,i])),
      min = round(min(base[,i],  na.rm = T), 3),
      max = round(max(base[,i],  na.rm = T), 3),
      mean = round(mean(base[,i],  na.rm = T), 3),
      sample1 = as.character(base[smp[1],i]),
      sample2 = as.character(base[smp[2],i]),
      sample3 = as.character(base[smp[3],i]),
      sample4 = as.character(base[smp[4],i]),
      sample5 = as.character(base[smp[5],i])
    )
    r <- r %>%
      mutate(base_name = name_base, 
             porc_NA = round((n_NA / nrow(base)) * 100, 2),
             col_name = gsub(pattern = "\\.", replacement = " ", x = col_name),# puntos -> espacios
      )%>%
      dplyr::select(base_name, col_name,  n_rows, n_unique, n_NA, porc_NA, everything())
    
    res <- rbind(res, r)
  }
  return(res)
  
}



# Cuensta duplicados por columna ------------------------------------------

duplicados <- function(file,columna){
  if(columna == "geometry"){
    col <- file %>% dplyr::select(all_of(columna))
  }else{
    col <- file %>% st_drop_geometry() %>% dplyr::select(all_of(columna))
  }
  
  d <- col %>% duplicated() %>% sum()
  return(d)
}

sf_toupper <-  function(sf_object){
  names(sf_object)[1:(ncol(sf_object)-1)] <- toupper(names(sf_object)[1:(ncol(sf_object)-1)])
  return(sf_object)
}

