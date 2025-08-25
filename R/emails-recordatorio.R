#' Emails por perfil de recordatorio
#'
#' Devuelve un data.frame con los emails de cada perfil:
#' PRE sin LONG, LONG sin PRE, LONG sin POST (si hay post) y Abandono.
#'
#' @param pre data.frame con flags (salida de add_flags()$pre).
#' @param long data.frame con flags y fechas (salida de add_flags()$long).
#' @param post data.frame con flags (salida de add_flags()$post). Opcional.
#' @param cutoff_days Número de días de inactividad para considerar abandono. Por defecto 7.
#' @param ref_date Fecha de referencia (por defecto hoy con Sys.Date()).
#'
#' @return data.frame con columnas `email` y `tipo`.
#' @export
emails_recordatorio <- function(pre, long, post = NULL,
                                cutoff_days = 7, ref_date = Sys.Date()) {
  stopifnot("email" %in% names(pre), "email" %in% names(long))
  
  # PRE sin LONG
  pre_sin_long <- perfil_incompleto(pre, "flag_long", FALSE)
  pre_sin_long <- data.frame(email = pre_sin_long$email, tipo = "PRE sin LONG")
  
  # LONG sin PRE
  long_sin_pre <- perfil_incompleto(long, "flag_pre", FALSE)
  long_sin_pre <- data.frame(email = long_sin_pre$email, tipo = "LONG sin PRE")
  
  # LONG sin POST (solo si hay post)
  if (!is.null(post) && "flag_post" %in% names(long)) {
    long_sin_post <- perfil_incompleto(long, "flag_post", FALSE)
    long_sin_post <- data.frame(email = long_sin_post$email, tipo = "LONG sin POST")
  } else {
    long_sin_post <- NULL
  }
  
  # Abandono
  abandono <- perfil_abandono(long, cutoff_days = cutoff_days, ref_date = ref_date)
  abandono <- data.frame(email = abandono$email, tipo = "Abandono")
  
  # Unir todo
  out <- rbind(pre_sin_long, long_sin_pre, long_sin_post, abandono)
  rownames(out) <- NULL
  out
}
