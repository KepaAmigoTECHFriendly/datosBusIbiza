#' @title Devuelve el tiempo restante de la parada seleccionada
#'
#' @description Devuelve el tiempo restante de la parada seleccionada
#'
#' @param parada
#'
#' @return json
#'
#' @examples  tiempo_por_parada()
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

tiempo_por_parada <- function(parada){

  PARADA <- as.character(parada)

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


  id_parada <- df_paradas$stop_id[match(PARADA,df_paradas$stop_name)]
  tiempos_parada <- df_tiempo_paradas[which(df_tiempo_paradas$stop_id %in% id_parada),]
  viajes_parada <- df_viajes[which(df_viajes$trip_id %in% tiempos_parada$trip_id),]

  # Filtro por día de la semana ---
  fecha <- Sys.Date()
  dia_semana <- as.POSIXlt(fecha)$wday
  dias_semana_ref <- c(colnames(df_calendario)[2:8])
  num_semana <- 0:6
  num_semana <- num_semana[c(2:7,1)]
  nombre_semana <- num_semana
  names(dias_semana_ref) <- nombre_semana

  dia_semana_actual <- dias_semana_ref[dia_semana]

  calendario <- df_calendario[which(df_calendario$service_id %in% viajes_parada$service_id),]
  pos_dia <- match(dia_semana_actual,colnames(calendario))
  id_servicio <- calendario$service_id[calendario[,pos_dia] == 1]

  # ---
  viajes_parada <- viajes_parada[which(viajes_parada$service_id %in% id_servicio),]
  rutas_parada <- df_rutas[which(df_rutas$route_id %in% viajes_parada$route_id),]


  # Generación de DF
  SENTIDO <- viajes_parada$trip_headsign
  RUTA <- c()
  HORARIO <- c()
  for(i in 1:length(SENTIDO)){
    RUTA <- c(RUTA, df_rutas$NOMBRE_RUTAS[match(viajes_parada$route_id[i],df_rutas$route_id)])
    HORARIO <- c(HORARIO, tiempos_parada$arrival_time[match(viajes_parada$trip_id[i],tiempos_parada$trip_id)])
  }
  PARADA <- rep(PARADA, length(SENTIDO))

  df_tiempos_parada <- data.frame(PARADA, RUTA, SENTIDO, HORARIO, stringsAsFactors = FALSE)
  df_tiempos_parada <- df_tiempos_parada[order(df_tiempos_parada$HORARIO),]

  tiempos <- df_tiempos_parada
  tiempos$HORARIO <- as.POSIXct(tiempos$HORARIO, format = '%H:%M', tz = 'CET')
  tiempos$diferencia <- round(as.numeric(difftime(tiempos$HORARIO,Sys.time(), units = "mins")))

  if(!any(tiempos$diferencia > 0)){ # No hay proximo bus en esta ruta hasta el día siguiente, devuelvo el horario.
    tiempos <- tiempos[order(tiempos$HORARIO),]
    tiempos$HORARIO <- substr(tiempos$HORARIO,12,16)
    tiempos$diferencia <- tiempos$HORARIO
  }else{
    tiempos <- tiempos[tiempos$diferencia > 0,]
    pos_min_tiempo <- match(min(tiempos$diferencia),tiempos$diferencia)
    tiempos <- tiempos[pos_min_tiempo:nrow(tiempos),]
  }

  colnames(tiempos)[ncol(tiempos)] <- "tiempo_restante"

  tiempos <- tiempos[,c(2,3,5)]
  json <- toJSON(tiempos)

  return(json)

}
