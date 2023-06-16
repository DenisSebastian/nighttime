
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


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
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
