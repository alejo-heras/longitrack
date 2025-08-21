

#' Poner nombres en Title Case respetando partículas
#'
#' Convierte un nombre a "Title Case" (primera letra en mayúscula) y
#' luego pasa a minúscula las partículas comunes (con [ajustar_particulas()]).
#' @keywords internal
#'
#' @param nombre Vector de caracteres con nombres.
#' @return Vector de caracteres con formato Title Case + partículas en minúscula.
nombre_title_case <- function(nombre) {
  nombre <- stringr::str_squish(as.character(nombre))
  nombre <- stringi::stri_trans_totitle(nombre, locale = "es_ES")
  ajustar_particulas(nombre)
}

#' Ajustar partículas en nombres
#' @keywords internal
#'
#' Convierte a minúscula partículas frecuentes en nombres y apellidos
#' (ej. "de", "del", "la", "y", "van", "von").
#'
#' @param nombre Vector de caracteres con nombres.
#' @return Vector de caracteres con partículas en minúscula.
ajustar_particulas <- function(nombre) {

  stopifnot(is.character(nombre) || is.factor(nombre))
  nombre <- as.character(nombre)

  particulas <- c(" De ", " Del ", " La ", " Las ", " Los ", " Y ", " Von ", " Van ", " Der ",
                  " DE ", " DEL ", " LA ", " LAS ", " LOS ", " Y ", " VON ", " VAN ", " DER "
                  )

  reemplazos <- tolower(particulas)
  stringr::str_replace_all(nombre, stats::setNames(reemplazos, particulas))
}


#' Normalizar nombres (Primera letra en mayúscula, 
#' excepto las partículas más comunes de algunos apellidos)
#'
#' @param nombre Vector de caracteres con nombres.
#' @return Vector normalizado.
#' @examples
#' # Normaliza mayúsculas y espacios extra:
#' normalizar_nombre("  MARÍA  DEL   PILAR ")
#' #> [1] "María del Pilar"
#'
#' # Normaliza partículas comunes:
#' normalizar_nombre(c("antonio de la vega", "JUAN DEL RÍO"))
#' #> [1] "Antonio de la Vega" "Juan del Río"
#' @export
normalizar_nombre <- function(nombre) {
  ajustar_particulas(nombre_title_case(nombre))
}




