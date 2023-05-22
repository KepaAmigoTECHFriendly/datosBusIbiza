#' @title Devuelve el listado de las paradas de una ruta y destino con los tiempos restantes y lat, long de las paradas
#'
#' @description Devuelve el listado de las paradas de una ruta y destino con los tiempos restantes
#'
#' @param ruta,destino
#'
#' @return json
#'
#' @examples  tiempos_paradas("13 Santa Eulària > Eivissa","Eivissa/CETIS")
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

tiempos_paradas <- function(ruta,destino){

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

  tiempos <- df_tiempo_paradas[which(df_tiempo_paradas$trip_id %in% viaje$trip_id),]
  tiempos$arrival_time <- as.POSIXct(tiempos$arrival_time, format = '%H:%M', tz = 'CET')
  tiempos$departure_time <- as.POSIXct(tiempos$departure_time, format = '%H:%M', tz = 'CET')
  #tiempos$DESTINO <- viaje$trip_headsign[match(tiempos$trip_id,viaje$trip_id)]
  tiempo_actual <- Sys.time()
  hour(tiempo_actual) <- hour(tiempo_actual) + 1
  tiempos$diferencia <- round(as.numeric(difftime(tiempos$arrival_time,tiempo_actual, units = "mins")))
  tiempos <- tiempos[complete.cases(tiempos[, 2]), ]

  if(!any(tiempos$diferencia > 0)){ # No hay proximo bus en esta ruta hasta el día siguiente, devuelvo el horario.
    tiempos <- tiempos[order(tiempos$arrival_time),]
    pos_max_secuencia <- match(max(tiempos$stop_sequence),tiempos$stop_sequence)[1]
    tiempos <- tiempos[1:pos_max_secuencia,]
    tiempos$diferencia <- substr(tiempos$arrival_time,12,16)
  }else{
    tiempos <- tiempos[tiempos$diferencia > 0,]
    pos_min_tiempo <- match(min(tiempos$diferencia),tiempos$diferencia)
    tiempos <- tiempos[pos_min_tiempo:nrow(tiempos),]
    pos_min_tiempo <- match(min(tiempos$diferencia),tiempos$diferencia)
    pos_max_secuencia <- match(max(tiempos$stop_sequence),tiempos$stop_sequence)[1]
    if(tiempos$stop_sequence[pos_min_tiempo] == 0){  # Si está la primera parada por donde pasa
      tiempos <- tiempos[1:pos_max_secuencia,]
    }else{
      posiciones_a_recoger <- which(tiempos$stop_sequence %in% tiempos$stop_sequence[1])[2] - 1
      tiempos <- tiempos[1:posiciones_a_recoger,]
      tiempos <- tiempos[order(tiempos$stop_sequence),]
    }
  }


  # Generación DF con Nombre paradas y tiempos restantes de línea seleccionada
  paradas <- df_paradas[which(df_paradas$stop_id %in% tiempos$stop_id),]
  paradas <- paradas[order(match(paradas$stop_id,tiempos$stop_id)),]
  paradas$tiempo_restante <- tiempos$diferencia
  DF_JSON_LISTADO <- paradas[,c("stop_name","tiempo_restante","stop_lat","stop_lon")]
  DF_JSON_LISTADO <- toJSON(DF_JSON_LISTADO)
  return(DF_JSON_LISTADO)
}
