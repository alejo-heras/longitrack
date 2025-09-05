#' Retención por semanas
#'
#' Estima la fracción de participantes aún "activos" en la semana *k*,
#' a partir de mediciones LONG en formato largo (varias filas por `email`).
#'
#' @details
#' - Se censuran mediciones futuras: `date <= ref_date`.
#' - Para cada `email` se calcula `primera = min(date)` y `ultima = max(date)`.
#' - Definición de retenido en semana `k`: `ultima >= (primera + k*7 - cutoff_days)`.
#' - El eje `0..max_semanas`:
#'   - Si `max_semanas` es `NULL`, se usa el máximo permitido por `ref_date`.
#'   - Si `cap_por_ref = TRUE`, se recorta a lo permitido por `ref_date`.
#'
#' @param long data.frame con columnas `email`, `date` (fechas LONG)
#' @param max_semanas entero opcional.
#' @param cutoff_days días sin medir para considerar abandono (default 7)
#' @param ref_date fecha de referencia (default = Sys.Date())
#' @param cap_por_ref lógico; si TRUE, recorta el eje X a lo permitido por `ref_date`.
#' 
#' @return tibble con columnas `semana` y `retencion` (proporción 0..1)
#' 
#' @examples
#' # Datos mínimos reproducibles
#' long <- data.frame(
#'   email = c("a@x.com","a@x.com","b@x.com","b@x.com","c@x.com"),
#'   date  = as.Date(c("2025-01-01","2025-01-10","2025-01-03","2025-01-17","2025-01-05"))
#' )
#' retencion_semanas(long, cutoff_days = 7, ref_date = as.Date("2025-01-31"))
#' 
#' @export
#' @seealso [metricas_padron()], [metricas_longitudinales()]
#' @family metricas
#' 
#' @importFrom rlang .data
retencion_semanas <- function(long,
                              max_semanas = NULL,
                              cutoff_days = 7,
                              ref_date = Sys.Date(),
                              cap_por_ref = TRUE) {
  long <- dplyr::mutate(long, date = as.Date(.data$date))
  long <- dplyr::filter(long, .data$date <= ref_date)  # censurar futuro
  
  first <- dplyr::summarise(
    dplyr::group_by(long, .data$email),
    primera = min(.data$date), .groups = "drop"
  )
  last  <- dplyr::summarise(
    dplyr::group_by(long, .data$email),
    ultima  = max(.data$date), .groups = "drop"
  )
  df <- merge(first, last, by = "email")
  
  if (nrow(df) == 0) {
    return(tibble::tibble(semana = integer(), retencion = numeric()))
  }
  
  max_por_ref <- max(as.integer((ref_date - df$primera) / 7), na.rm = TRUE)
  if (!is.finite(max_por_ref)) max_por_ref <- 0L
  
  if (is.null(max_semanas)) {
    max_final <- max_por_ref
  } else if (cap_por_ref) {
    max_final <- min(as.integer(max_semanas), max_por_ref)
  } else {
    max_final <- as.integer(max_semanas)
  }
  
  semanas <- 0:max_final
  tibble::tibble(
    semana = semanas,
    retencion = sapply(semanas, function(k) {
      mean(df$ultima >= (df$primera + k * 7 - cutoff_days), na.rm = TRUE)
    })
  )
}
