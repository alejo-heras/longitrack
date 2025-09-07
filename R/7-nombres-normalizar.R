# ------------------------------------------------------------------
# Este script define funciones para normalizar nombres propios:
# - nombre_title_case(): pone en "Title Case" y luego ajusta partículas.
# - ajustar_particulas(): convierte a minúscula partículas comunes.
# - normalizar_nombre(): combina ambas para normalizar nombres completos.
# ------------------------------------------------------------------

#' Poner nombres en Title Case respetando partículas
#'
#' Convierte un nombre a "Title Case" (primera letra en mayúscula) y
#' luego pasa a minúscula las partículas comunes (con [ajustar_particulas()]).
#' @keywords internal
#'
#' @param texto Vector de caracteres con nombres.
#' @return Vector de caracteres con formato Title Case + partículas en minúscula.
nombre_title_case <- function(texto) {
  texto <- stringr::str_squish(as.character(texto))
  texto <- stringi::stri_trans_totitle(texto, locale = "es_ES")
  ajustar_particulas(texto)
}

#' Ajustar partículas en nombres
#' @keywords internal
#'
#' Convierte a minúscula partículas frecuentes en nombres y apellidos
#' (ej. "de", "del", "la", "y", "van", "von").
#'
#' @param texto Vector de caracteres con nombres.
#' @return Vector de caracteres con partículas en minúscula.
ajustar_particulas <- function(texto) {
  stopifnot(is.character(texto) || is.factor(texto))
  texto <- as.character(texto)
  
  particulas <- c(" De ", " Del ", " La ", " Las ", " Los ", " Y ", " Von ", " Van ", " Der ",
                  " DE ", " DEL ", " LA ", " LAS ", " LOS ", " Y ", " VON ", " VAN ", " DER ")
  
  reemplazos <- tolower(particulas)
  stringr::str_replace_all(texto, stats::setNames(reemplazos, particulas))
}

#' Normalizar nombres (Primera letra en mayúscula, 
#' excepto las partículas más comunes de algunos apellidos)
#'
#' @param texto Vector de caracteres con nombres.
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
normalizar_nombre <- function(texto) {
  ajustar_particulas(nombre_title_case(texto))
}
