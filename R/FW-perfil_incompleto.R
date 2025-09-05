
#' Perfil según flag
#'
#' Devuelve las personas que cumplen cierta condición lógica sobre un flag
#' (ej. PRE sin LONG, LONG sin PRE, POST sin PRE, etc.).
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' Esta función está deprecada, usa en su lugar `dplyr::filter()`.
#' 
#' Ejemplo:
#' 
#' perfil_no_long <- padron %>% filter(flag_pre == T & flag_long == F)
#' perfil_no_pre <- padron %>% filter(flag_pre == F & flag_long == T)
#' perfil_abandono     <- padron %>% filter(!is.na(last_long) & last_long <= Sys.Date() - 7)
#'
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
#' # Datos de ejemplo (auto-contenidos para R CMD check)
#' df <- data.frame(
#'   email     = c("a@x.com","b@x.com","c@x.com","d@x.com"),
#'   flag_pre  = c(TRUE,  TRUE,  FALSE, TRUE),
#'   flag_long = c(FALSE, TRUE,  FALSE, NA),
#'   flag_post = c(TRUE,  FALSE, TRUE,  TRUE),
#'   stringsAsFactors = FALSE
#' )
#'
#' # PRE sin LONG: partimos de quienes tienen PRE y buscamos los que NO tienen LONG
#' pre  <- subset(df, flag_pre %in% TRUE)
#' perfil_incompleto(pre, flag = "flag_long", value = FALSE)
#'
#' # LONG sin PRE: partimos de quienes tienen LONG y buscamos los que NO tienen PRE
#' long <- subset(df, flag_long %in% TRUE)
#' perfil_incompleto(long, flag = "flag_pre", value = FALSE)
#'
#' # POST sin PRE: partimos de quienes tienen POST y buscamos los que NO tienen PRE
#' post <- subset(df, flag_post %in% TRUE)
#' perfil_incompleto(post, flag = "flag_pre", value = FALSE)
perfil_incompleto <- function(data, flag, value = FALSE) {
  stopifnot(is.data.frame(data))
  stopifnot("email" %in% names(data))
  stopifnot(is.character(flag), length(flag) == 1L)
  
  if (!flag %in% names(data)) {
    stop("No existe la columna de flag: ", flag, call. = FALSE)
  }
  if (!is.logical(data[[flag]])) {
    stop("La columna '", flag, "' debe ser lógica (TRUE/FALSE/NA).", call. = FALSE)
  }
  
  # mantener una fila por persona
  uniq <- data[!duplicated(data$email), , drop = FALSE]
  
  # Manejo explícito de NA: solo comparamos cuando no es NA
  idx <- !is.na(uniq[[flag]]) & uniq[[flag]] == value
  uniq[idx, , drop = FALSE]
}


