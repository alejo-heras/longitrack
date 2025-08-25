# ------------------------------------------------------------------
# Este script define funciones para normalizar emails:
# - quitar_acentos(): elimina acentos y caracteres no imprimibles.
# - normalizar_email(): pone en minúsculas, quita acentos y espacios.
# - emails_check(): reporta errores comunes en emails originales.
# ------------------------------------------------------------------

#' Quita tildes/acentos y normaliza caracteres a ASCII
#'
#' Convierte caracteres acentuados a su forma base y elimina
#' caracteres no imprimibles. Usamos `stringi::stri_trans_general("Latin-ASCII")`
#' por su mayor consistencia frente a `iconv` entre plataformas.
#'
#' @param texto Vector de caracteres.
#' @return Un vector de caracteres sin acentos ni caracteres no imprimibles.
#' @examples
#' quitar_acentos(c("Camión", "AÑA", "José  "))
#' @importFrom stringi stri_trans_general
quitar_acentos <- function(texto) {
  stopifnot(is.character(texto))
  y <- stringi::stri_trans_general(texto, "Latin-ASCII")
  # elimina caracteres no imprimibles (incl. restos de transliteración)
  y <- gsub("[^[:print:]]", "", y, perl = TRUE)
  y
}

#' Normaliza emails (minúsculas, sin acentos, sin espacios)
#'
#' @param texto Vector de emails
#' @return Vector con emails normalizados
#' @export
normalizar_email <- function(texto) {
  stopifnot(is.character(texto) | is.factor(texto))
  texto <- trimws(as.character(texto))
  texto <- quitar_acentos(tolower(texto))
  gsub("\\s+", "", texto, perl = TRUE)
}

#' Chequea y reporta errores comunes en emails
#'
#' @param texto Vector de emails
#' @return Tibble con columnas original, normalizado, tipo_error
#' @export
emails_check <- function(texto) {
  original <- trimws(as.character(texto))
  normalizado <- normalizar_email(original)
  
  tipo_error <- ifelse(grepl("[áéíóúÁÉÍÓÚ]", original), "tildes",
                       ifelse(grepl("[ñÑ]", original), "ñ",
                              ifelse(grepl("\\s", original), "espacios",
                                     ifelse(grepl("[A-Z]", original), "mayúsculas", NA_character_))))
  
  tibble::tibble(
    original,
    normalizado,
    tipo_error
  )
}
