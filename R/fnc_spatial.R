
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

# tegs <- read_rds("data/TEGS.rds")
# resumen_tegs <- resumen_base(tegs, name_base = "TEGS_BASE")
# resumen_tegs




## funciones de uso general

# Si no existe directorio lo crea
make_dir <- function(path){
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
}

# transformar -999 a NA
fix_missing <- function(x) {
  if(is.numeric(x)){
    x[x == -999] <- NA
    
  }
  return(x)
}


# transformar NA a 0
fix_NA_0 <- function(x) {
  x[is.na(x)] <- 0
  return(x)
}



# Reeplaza puntos pomas
pto_por_coma <- function(x){
  format(x, big.mark = ".", decimal.mark = "," ,
         scientific = FALSE)
  
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


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

sf_toupper <-  function(sf_object){
  names(sf_object)[1:(ncol(sf_object)-1)] <- toupper(names(sf_object)[1:(ncol(sf_object)-1)])
  return(sf_object)
}

# read delitos rds

read_delitos <- function(path_data = "data/delitos/delitos_base/", year){
  delitos <- readRDS(paste0(path_data, "delitos_", year, ".rds"))
  return(delitos)
}


# función paara saber catidad de acuerdo un porcentaje
porc_df <- function(dataframe, porcentaje){
  if(porcentaje>100){
    stop("Porcentaje NO puede ser mayor que 100")
  }else{
    num <- round(nrow(dataframe)*(porcentaje/100))
  }
  return(num)
}
