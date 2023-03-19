#' @title Devuelve los destinos de una ruta particular
#'
#' @description Devuelve los destinos de una ruta particular
#'
#' @param ruta
#'
#' @return json
#'
#' @examples  destinos_ruta("13 Santa Eulària > Eivissa")
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

destinos_ruta <- function(ruta){

  RUTA <- as.character(ruta)

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

  id_ruta <- df_rutas$route_id[df_rutas$NOMBRE_RUTAS == RUTA]

  df_destinos <- df_viajes[df_viajes$route_id == id_ruta,]
  destinos <- unique(df_destinos$trip_headsign)

  json_destinos <- toJSON(destinos)
  return(json_destinos)
}
