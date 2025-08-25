#' Resumen de perfiles
#'
#' Devuelve un tibble con el número de personas en cada perfil
#' (PRE sin LONG, LONG sin PRE, POST sin PRE, POST sin LONG, Abandono).
#'
#' @param pre data.frame con flags (salida de add_flags()$pre).
#' @param long data.frame con flags y fechas (salida de add_flags()$long).
#' @param post data.frame con flags (salida de add_flags()$post).
#' @param cutoff_days Número de días de inactividad para considerar abandono.
#' @param ref_date Fecha de referencia (por defecto hoy).
#'
#' @return Tibble con columnas `perfil` y `n`.
#' @export
resumen_perfiles <- function(pre, long, post = NULL, cutoff_days = 7, ref_date = Sys.Date()) {
  af <- add_flags(pre, long, post)
  
  pre2  <- af$pre
  long2 <- af$long
  post2 <- af$post
  
  pre_sin_long  <- perfil_incompleto(pre2,  flag = "flag_long", value = FALSE)
  long_sin_pre  <- long2[!duplicated(long2$email) & !long2$flag_pre, c("email"), drop = FALSE]
  post_sin_pre  <- if (!is.null(post2)) perfil_incompleto(post2, flag = "flag_pre", value = FALSE) else post2
  abandono      <- perfil_abandono(long2, cutoff_days = cutoff_days, ref_date = ref_date)
  
  tibble::tibble(
    grupo = c("PRE sin seguimiento", "Seguimiento sin PRE", "POST sin PRE", paste0("Abandono (+", cutoff_days, " días)")),
    n     = c(nrow(pre_sin_long), nrow(long_sin_pre), if (!is.null(post2)) nrow(post_sin_pre) else 0L, nrow(abandono))
  )
}




