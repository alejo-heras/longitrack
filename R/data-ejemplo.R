#' Datos de ejemplo: pretest
#'
#' Conjunto ficticio de respuestas de pretest con emails y nombres con errores comunes.
#' Incluye casos con acentos, mayúsculas, espacios extra y dominios mal escritos.
#'
#' @format Un tibble con n filas y 3 variables:
#' \describe{
#'   \item{email}{Dirección de correo con errores intencionales}
#'   \item{name}{Nombre original}
#'   \item{date}{Fecha de respuesta}
#'   \item{email_google}{Email alternativo capturado en Google Forms (opcional)}
#' }
#' @examples
#' data(ejemplo_pre)
#' head(ejemplo_pre)
"ejemplo_pre"


#' Datos de ejemplo: longitudinal
#'
#' Conjunto ficticio de mediciones longitudinales con múltiples observaciones por persona,
#' fechas distribuidas en varias semanas, abandonos y participantes externos sin pretest.
#'
#' @format Un tibble con m filas y 2 variables:
#' \describe{
#'   \item{email}{Dirección de correo (con posibles errores)}
#'   \item{date}{Fecha de la medición}
#' }
#' @examples
#' data(ejemplo_long)
#' dplyr::count(ejemplo_long, email)
"ejemplo_long"


#' Datos de ejemplo: postest
#'
#' Conjunto ficticio de postest aplicado a un subconjunto de participantes,
#' incluyendo casos con y sin pretest/longitudinal.
#'
#' @format Un tibble con p filas y 2 variables:
#' \describe{
#'   \item{email}{Dirección de correo}
#'   \item{date}{Fecha de respuesta}
#' }
#' @examples
#' data(ejemplo_post)
#' nrow(ejemplo_post)
"ejemplo_post"
