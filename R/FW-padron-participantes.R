# ------------------------------------------------------------------------------
# FW-padron-participantes.R
# ------------------------------------------------------------------------------
# PSEUDOCÓDIGO (breve)
# 1) Validar columnas de entrada.
# 2) Unificar emails únicos de PRE, LONG y (opcional) POST.
# 3) Resumir LONG por email: n_long, first_long, last_long.
# 4) Añadir flags: flag_pre, flag_long, flag_post.
# 5) Devolver tabla armonizada con columnas finales.
# ------------------------------------------------------------------------------

#' Padrón único de participantes (armonizado)
#'
#' Devuelve una tabla con todos los emails únicos que aparezcan en PRE, LONG o POST,
#' junto con flags y métricas básicas (sin dependencias auxiliares).
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
#' # base_unica <- padron_participantes(pre, long, post)
padron_participantes <- function(pre, long, post = NULL) {
  # -- 1) Validación mínima de columnas requeridas -----------------------------
  stopifnot(
    "email" %in% names(pre),
    all(c("email", "date") %in% names(long))
  )
  if (!is.null(post)) stopifnot("email" %in% names(post))
  
  # -- Normalización ligera (opcional pero útil): emails en minúsculas ---------
  tolower_col <- function(x) tolower(trimws(as.character(x)))
  pre_email  <- tolower_col(pre$email)
  long_email <- tolower_col(long$email)
  post_email <- if (!is.null(post)) tolower_col(post$email) else character(0)
  
  # -- 2) Todos los emails únicos ---------------------------------------------
  todos <- tibble::tibble(
    email = unique(c(pre_email, long_email, post_email))
  )
  
  # -- 3) Resumen LONG por email ----------------------------------------------
  # Convertimos date a Date si viene como texto/posix; si ya es Date no cambia.
  long_dates <- suppressWarnings(as.Date(long$date))
  # Si no se pudo convertir, mantenemos tal cual (p.ej. POSIXct) para min/max.
  if (all(is.na(long_dates)) && !inherits(long$date, "Date")) {
    long_dates <- long$date
  }
  
  long_summ <- tibble::tibble(email = long_email, date = long_dates) |>
    dplyr::filter(!is.na(email)) |>
    dplyr::group_by(email) |>
    dplyr::summarise(
      n_long     = dplyr::n(),
      first_long = suppressWarnings(suppressMessages(min(date, na.rm = TRUE))),
      last_long  = suppressWarnings(suppressMessages(max(date, na.rm = TRUE))),
      .groups = "drop"
    )
  
  # -- 4) Flags por presencia --------------------------------------------------
  out <- todos |>
    # unir métricas LONG
    dplyr::left_join(long_summ, by = "email") |>
    # flags: PRE (presencia en pre), LONG (n_long>0), POST (presencia en post)
    dplyr::mutate(
      flag_pre  = email %in% pre_email,
      flag_long = !is.na(n_long) & n_long > 0L,
      flag_post = if (length(post_email)) email %in% post_email else FALSE
    )
  
  # -- 5) Orden y tipos finales ------------------------------------------------
  out |>
    dplyr::mutate(
      n_long = dplyr::coalesce(as.integer(n_long), 0L),
      # Asegurar clases Date si pudieron calcularse; si todo NA, se quedan NA
      first_long = if (inherits(long_dates, "Date")) as.Date(first_long) else first_long,
      last_long  = if (inherits(long_dates, "Date")) as.Date(last_long)  else last_long
    ) |>
    dplyr::select(
      email,
      flag_pre, flag_long, flag_post,
      n_long, first_long, last_long
    )
}
