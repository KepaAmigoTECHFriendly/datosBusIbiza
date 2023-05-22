#' @title Devuelve las paradas del servicio de autobús de la isla de Ibiza
#'
#' @description Devuelve las paradas del servicio de autobús de la isla de Ibiza
#'
#' @return json
#'
#' @examples  paradas()
#'
#' @import httr
#' jsonlite
#' dplyr
#' lubridate
#'
#' @export

paradas <- function(){

  ficheros_en_ruta <- list.files(system.file('extdata', package = 'datosBusIbiza'), full.names = TRUE)

  df_paradas <- read.csv(as.character(ficheros_en_ruta[grep("stops.txt",ficheros_en_ruta)]), sep = ",", stringsAsFactors = FALSE, check.names = FALSE)

  df_paradas <- df_paradas[,c(3,5,6)]
  json <- toJSON(df_paradas)

  return(json)
}
