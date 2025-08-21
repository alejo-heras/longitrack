

#' Poner nombres en Title Case respetando partículas
#'
#' Convierte un nombre a "Title Case" (primera letra en mayúscula) y
#' luego pasa a minúscula las partículas comunes (con [ajustar_particulas()]).
#'
#' @param nombre Vector de caracteres con nombres.
#' @return Vector de caracteres con formato Title Case + partículas en minúscula.
#' @examples
#' nombre_title_case("MARÍA DEL PILAR")
#' nombre_title_case("antonio de la vega")
#' @export
nombre_title_case <- function(nombre) {
  nombre <- stringr::str_squish(as.character(nombre))
  nombre <- stringi::stri_trans_totitle(nombre, locale = "es_ES")
  ajustar_particulas(nombre)
}

#' Ajustar partículas en nombres
#'
#' Convierte a minúscula partículas frecuentes en nombres y apellidos
#' (ej. "de", "del", "la", "y", "van", "von").
#'
#' @param nombre Vector de caracteres con nombres.
#' @return Vector de caracteres con partículas en minúscula.
#' @examples
#' ajustar_particulas("Antonio De La Vega")
#' ajustar_particulas(c("JUAN DEL RÍO", "María VAN DER Meer"))
#' @export
ajustar_particulas <- function(nombre) {

  stopifnot(is.character(nombre) || is.factor(nombre))
  nombre <- as.character(nombre)

  particulas <- c(" De ", " Del ", " La ", " Las ", " Los ", " Y ", " Von ", " Van ", " Der ",
                  " DE ", " DEL ", " LA ", " LAS ", " LOS ", " Y ", " VON ", " VAN ", " DER "
                  )

  reemplazos <- tolower(particulas)
  stringr::str_replace_all(nombre, setNames(reemplazos, particulas))
}


#' Normalizar nombres (Title Case + partículas)
#'
#' Atajo: aplica `nombre_title_case()` (Title Case con locale es_ES)
#' y `ajustar_particulas()` (partículas a minúscula).
#'
#' @param nombre Vector de caracteres con nombres.
#' @return Vector normalizado.
#' @examples
#' normalizar_nombre("  MARÍA  DEL   PILAR ")
#' normalizar_nombre(c("antonio de la vega", "JUAN DEL RÍO"))
#' @export
normalizar_nombre <- function(nombre) {
  ajustar_particulas(nombre_title_case(nombre))
}




