
#' Perfil según flag
#'
#' Devuelve las personas que cumplen cierta condición lógica sobre un flag
#' (ej. PRE sin LONG, LONG sin PRE, POST sin PRE, etc.).
#'
#' @param data data.frame con columna `email` y el flag lógico correspondiente
#'   (ej. `flag_long`, `flag_pre`, `flag_post`).
#' @param flag nombre del flag (string).
#' @param value valor lógico que define el perfil (por defecto `FALSE`).
#'
#' @return Subconjunto de `data` con las personas que cumplen la condición.
#' @export
#'
#' @examples
#' # PRE sin LONG
#' pre <- add_flags(pre, long, post)$pre
#' perfil_incompleto(pre, flag = "flag_long", value = FALSE)
#'
#' # LONG sin PRE
#' long <- add_flags(pre, long, post)$long
#' perfil_incompleto(long, flag = "flag_pre", value = FALSE)
#'
#' # POST sin PRE
#' post <- add_flags(pre, long, post)$post
#' perfil_incompleto(post, flag = "flag_pre", value = FALSE)
perfil_incompleto <- function(data, flag, value = FALSE) {
  stopifnot("email" %in% names(data))
  if (!flag %in% names(data)) {
    stop("No existe la columna flag: ", flag)
  }
  # mantener una fila por persona
  uniq <- data[!duplicated(data$email), , drop = FALSE]
  uniq[uniq[[flag]] == value, , drop = FALSE]
}


