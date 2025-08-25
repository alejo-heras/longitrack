#' Padrón único de participantes
#'
#' Devuelve una tabla con todos los emails únicos que aparezcan en PRE, LONG o POST,
#' junto con flags y métricas básicas.
#'
#' @param pre data.frame con columna `email` (fase PRE).
#' @param long data.frame con columnas `email` y `date` (fase LONG).
#' @param post data.frame con columna `email` (fase POST). Opcional.
#'
#' @return data.frame con columnas:
#'   - email
#'   - flag_pre, flag_long, flag_post
#'   - n_long, first_long, last_long
#' @export
#'
#' @examples
#' base_unica <- padron_participantes(pre, long, post)
padron_participantes <- function(pre, long, post = NULL) {
  stopifnot("email" %in% names(pre),
            "email" %in% names(long),
            "date"  %in% names(long))
  
  # todos los emails únicos
  todos <- tibble::tibble(
    email = unique(c(pre$email, long$email, if (!is.null(post)) post$email))
  )
  
  # pasar como "pseudo-pre" a add_flags
  res <- add_flags(pre = todos, long = long, post = post)
  base <- res$pre
  
  # añadir flag_pre real
  base$flag_pre <- base$email %in% pre$email
  
  base
}
