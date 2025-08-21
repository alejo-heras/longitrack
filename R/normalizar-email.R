
#' Quitar acentos (transliterar a ASCII)
#' @keywords internal
#'
#' @param x Vector de caracteres.
#' @return Vector sin tildes ni diacríticos.
quitar_acentos <- function(x) {
  x <- as.character(x)
  stringi::stri_trans_general(x, "Latin-ASCII")
}

#' Normalizar emails y detectar errores comunes
#'
#' Limpia espacios, pasa a minúsculas, elimina tildes y espacios internos.
#' Además etiqueta si el original tenía tildes, ñ, espacios o mayúsculas.
#'
#' @param email_vector Vector de caracteres con emails.
#' @return data.frame con columnas: `original`, `normalizado`, `tipo_error`.
#' @examples
#' # Corrige mayúsculas, espacios, tildes y ñ en direcciones de email:
#' normalizar_email(c("  JOSÉ.PeRez @GmAil.com ", "ana_ñ@example.es"))
#' #>             original          normalizado  tipo_error
#' #> 1   JOSÉ.PeRez @GmAil.com  jose.perez@gmail.com     tildes
#' #> 2         ana_ñ@example.es     ana_n@example.es        ñ
#' @export
normalizar_email <- function(email_vector) {
  stopifnot(is.character(email_vector) || is.factor(email_vector))
  original <- trimws(as.character(email_vector))

  # normalización mínima
  normalizado <- quitar_acentos(tolower(original))
  normalizado <- gsub("\\s+", "", normalizado)

  # tipado de error simple (primera coincidencia relevante)
  tipo_error <- ifelse(grepl("[áéíóúÁÉÍÓÚ]", original), "tildes",
                       ifelse(grepl("[ñÑ]", original), "ñ",
                              ifelse(grepl("\\s", original), "espacios",
                                     ifelse(grepl("[A-Z]", original), "mayúsculas",
                                            NA_character_))))

  data.frame(
    original = original,
    normalizado = normalizado,
    tipo_error = tipo_error,
    stringsAsFactors = FALSE
  )
}
