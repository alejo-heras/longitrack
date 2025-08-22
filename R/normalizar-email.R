#' Quita tildes/acentos y normaliza caracteres a ASCII
#'
#' Convierte caracteres acentuados a su forma base y elimina
#' caracteres no imprimibles. Usamos `stringi::stri_trans_general("Latin-ASCII")`
#' por su mayor consistencia frente a `iconv` entre plataformas.
#'
#' @param x Vector de caracteres.
#' @return Un vector de caracteres sin acentos ni caracteres no imprimibles.
#' @examples
#' quitar_acentos(c("Camión", "AÑA", "José  "))
#' @export
#' @importFrom stringi stri_trans_general
quitar_acentos <- function(x) {
  stopifnot(is.character(x))
  y <- stringi::stri_trans_general(x, "Latin-ASCII")
  # elimina caracteres no imprimibles (incl. restos de transliteración)
  y <- gsub("[^[:print:]]", "", y, perl = TRUE)
  y
}

#' Normaliza emails (minúsculas, sin acentos, sin espacios)
#'
#' @param x Vector de emails
#' @return Vector con emails normalizados
#' @export
normalizar_email <- function(x) {
  stopifnot(is.character(x) | is.factor(x))
  x <- trimws(as.character(x))
  x <- quitar_acentos(tolower(x))
  gsub("\\s+", "", x, perl = TRUE)
}


#' Chequea y reporta errores comunes en emails
#'
#' @param x Vector de emails
#' @return Tibble con columnas original, normalizado, tipo_error
#' @export
emails_check <- function(x) {
  original <- trimws(as.character(x))
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

