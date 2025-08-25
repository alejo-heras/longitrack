#' Métricas básicas de evolución temporal (serie)
#'
#' @description
#' Calcula, en una serie temporal entre dos fechas, tres métricas:
#' - Número de personas distintas en PRE.
#' - Número de registros en LONG.
#' - Promedio de seguimientos por persona en LONG.
#'
#' @param pre  Data frame con columnas `email` y `date` (fase PRE).
#' @param long Data frame con columnas `email` y `date` (fase LONG).
#' @param start Fecha mínima (Date o coercible). Por defecto, el mínimo entre pre y long.
#' @param end   Fecha máxima (Date o coercible). Por defecto, la fecha de hoy (`Sys.Date()`).
#' @param by    Incremento de secuencia temporal. Valores típicos: `"day"`, `"week"`, `"month"`.
#'
#' @return Un tibble con columnas:
#' \itemize{
#'   \item \code{fecha} fecha de corte
#'   \item \code{pre_respondientes} número de emails distintos en PRE hasta esa fecha
#'   \item \code{seguimientos_totales} número de registros en LONG hasta esa fecha
#'   \item \code{promedio_seguimientos} seguimientos_totales / personas_long
#' }
#'
#' @examples
#' # evolucion_temporal_df(pre, long, start = "2025-01-01", end = "2025-02-01")
#'
#' @export
evolucion_temporal_df <- function(pre, long,
                             start = NULL,
                             end   = NULL,
                             by    = "day") {
  
  stopifnot("email" %in% names(pre),
            "date"  %in% names(pre),
            "email" %in% names(long),
            "date"  %in% names(long))
  
  # coerción de fechas
  pre$date  <- as.Date(pre$date)
  long$date <- as.Date(long$date)
  
  start <- if (is.null(start)) min(c(pre$date, long$date), na.rm = TRUE) else as.Date(start)
  end   <- if (is.null(end))   Sys.Date() else as.Date(end)
  
  fechas <- seq(start, end, by = by)
  
  purrr::map_dfr(fechas, function(fecha_corte) {
    pre_corte  <- pre  %>% dplyr::filter(.data$date <= fecha_corte)
    long_corte <- long %>% dplyr::filter(.data$date <= fecha_corte)
    
    n_pre_respondientes <- dplyr::n_distinct(pre_corte$email)
    n_long_registros    <- nrow(long_corte)
    n_long_personas     <- dplyr::n_distinct(long_corte$email)
    
    promedio_long <- ifelse(n_long_personas > 0,
                            n_long_registros / n_long_personas,
                            NA_real_)
    
    tibble::tibble(
      fecha = fecha_corte,
      pre_respondientes   = n_pre_respondientes,
      seguimientos_totales = n_long_registros,
      promedio_seguimientos = promedio_long
    )
  })
}
