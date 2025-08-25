#' Perfil: Abandono
#'
#' Devuelve las personas cuya última medición en LONG es anterior o igual a
#' un límite de inactividad definido por \code{cutoff_days}.
#'
#' @param long data.frame con columnas `email` y `date`.
#' @param cutoff_days Número de días sin mediciones para considerar abandono. 
#'   Por defecto 7.
#' @param ref_date Fecha de referencia respecto a la cual se calcula el abandono.
#'   Por defecto se usa la fecha actual con \code{Sys.Date()}, por lo que 
#'   normalmente no necesitas especificarla. Úsala solo si quieres fijar
#'   manualmente la "foto" de un día concreto (p.ej. para informes reproducibles).
#'
#' @return data.frame con columnas `email` y `ultima_fecha`.
#' @export
#'
#' @examples
#' long <- data.frame(
#'   email = c("a@x.com","a@x.com","b@x.com"),
#'   date  = as.Date(c("2025-01-01","2025-01-10","2025-01-05"))
#' )
#'
#' # Usando la fecha de hoy (Sys.Date())
#' perfil_abandono(long, cutoff_days = 7)
#'
#' # Congelar en una fecha fija para reproducibilidad
#' perfil_abandono(long, cutoff_days = 7, ref_date = as.Date("2025-01-20"))
#' Perfil: Abandono
#' ...
perfil_abandono <- function(long, cutoff_days = 7, ref_date = Sys.Date()) {
  stopifnot("email" %in% names(long), "date" %in% names(long))
  # Normaliza tipos
  long$date <- as.Date(long$date)
  ref_date  <- as.Date(ref_date)
  
  ultima <- stats::aggregate(date ~ email, data = long, FUN = max, na.rm = TRUE)
  names(ultima)[2] <- "ultima_fecha"
  ultima[ultima$ultima_fecha <= (ref_date - cutoff_days), , drop = FALSE]
}

