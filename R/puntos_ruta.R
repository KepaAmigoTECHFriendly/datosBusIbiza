#' @title Devuelve los puntos de una ruta para una ruta y destino
#'
#' @description Devuelve los puntos de una ruta para una ruta y destino
#'
#' @param ruta,destino
#'
#' @return json
#'
#' @examples  puntos_ruta("13 Santa Eulària > Eivissa","Eivissa/CETIS")
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

puntos_ruta <- function(ruta,destino){

  RUTA <- as.character(ruta)
  DESTINO <- as.character(destino)

  ficheros_en_ruta <- list.files(system.file('extdata', package = 'datosBusIbiza'), full.names = TRUE)

  df_agencia <- read.csv(as.character(ficheros_en_ruta[grep("agency.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_calendario <- read.csv(as.character(ficheros_en_ruta[grep("calendar.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_calendario_fechas <- read.csv(as.character(ficheros_en_ruta[grep("calendar_dates.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_rutas <- read.csv(as.character(ficheros_en_ruta[grep("routes.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_sombras <- read.csv(as.character(ficheros_en_ruta[grep("shapes.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_paradas <- read.csv(as.character(ficheros_en_ruta[grep("stops.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_tiempo_paradas <- read.csv(as.character(ficheros_en_ruta[grep("stop_times.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)
  df_viajes <- read.csv(as.character(ficheros_en_ruta[grep("trips.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)


  df_rutas$NOMBRE_RUTAS <- paste(df_rutas$route_short_name,df_rutas$route_long_name, sep = " ")


  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------
  # RECOGIDA DE TIEMPOS PARA LISTADO
  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------

  id_ruta <- df_rutas$route_id[df_rutas$NOMBRE_RUTAS == RUTA]
  viaje <- df_viajes[df_viajes$route_id == id_ruta & df_viajes$trip_headsign == DESTINO,]


  # Filtro por día de la semana ---
  fecha <- Sys.Date()
  dia_semana <- as.POSIXlt(fecha)$wday
  dias_semana_ref <- c(colnames(df_calendario)[2:8])
  num_semana <- 0:6
  num_semana <- num_semana[c(2:7,1)]
  nombre_semana <- num_semana
  names(dias_semana_ref) <- nombre_semana

  dia_semana_actual <- dias_semana_ref[dia_semana]

  calendario <- df_calendario[which(df_calendario$service_id %in% viaje$service_id),]
  pos_dia <- match(dia_semana_actual,colnames(calendario))
  id_servicio <- calendario$service_id[calendario[,pos_dia] == 1]
  # ---
  viaje <- viaje[which(viaje$service_id %in% id_servicio),]


  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------
  # RECOGIDA DE TIEMPOS PARA MAPA
  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------
  shape_id <- unique(viaje$shape_id)
  DF_RUTA <- df_sombras[df_sombras$shape_id == shape_id,]
  DF_RUTA <- DF_RUTA[,c(2,3,4)]


  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------
  # GENERACIÓN JSON
  #------------------------------------------------------------------------------------
  #------------------------------------------------------------------------------------
  json_rutas <- toJSON(DF_RUTA)

  return(json_rutas)
}
