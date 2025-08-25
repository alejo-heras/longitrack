# ------------------------------------------------------------------------------
# PSEUDOCÓDIGO
# 1) Normaliza fechas de long.
# 2) Calcula primera y última fecha por persona.
# 3) Define rango de semanas 0..máximo.
# 4) retenido si última >= (primera + k*7 - cutoff_days)
# ------------------------------------------------------------------------------

#' Retención por semanas 
#' @param long data.frame con columnas `email`, `date` (fechas LONG)
#' @param max_semanas entero opcional. Si se pasa y `cap_por_ref=TRUE`,
#'   se recorta a lo que permita `ref_date`.
#' @param cutoff_days días sin medir para considerar abandono (default 7)
#' @param ref_date fecha de referencia (default = Sys.Date())
#' @param cap_por_ref lógico; si TRUE, recorta el eje X a lo permitido por `ref_date`.
#' @return tibble con columnas `semana` y `retencion` (proporción 0..1)
#' @export
retencion_semanas <- function(long,
                              max_semanas = NULL,
                              cutoff_days = 7,
                              ref_date = Sys.Date(),
                              cap_por_ref = TRUE) {
  long <- dplyr::mutate(long, date = as.Date(date))
  long <- dplyr::filter(long, date <= ref_date)  # censurar futuro
  
  first <- dplyr::summarise(dplyr::group_by(long, email),
                            primera = min(date), .groups = "drop")
  last  <- dplyr::summarise(dplyr::group_by(long, email),
                            ultima  = max(date), .groups = "drop")
  df <- merge(first, last, by = "email")
  
  # Si no hay datos (p.ej., ref_date anterior a cualquier medición)
  if (nrow(df) == 0) {
    return(tibble::tibble(semana = integer(), retencion = numeric()))
  }
  
  # Semanas máximas permitidas por la fecha de referencia
  max_por_ref <- max(as.integer((ref_date - df$primera) / 7), na.rm = TRUE)
  if (!is.finite(max_por_ref)) max_por_ref <- 0L
  
  # Elegir max_semanas final
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
