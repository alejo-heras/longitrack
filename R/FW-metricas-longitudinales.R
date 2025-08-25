#' Métricas longitudinales (día o semana)
#'
#' Calcula:
#' - `n_pre` / `n_pre_acum`: PRE por día / acumulado
#' - `n_long_dia` / `n_long_acum`: LONG (eventos) por día / acumulado
#' - `n_long_personas`: personas únicas con >= LONG hasta la fecha
#' - `n_post` / `n_post_acum`: POST por día / acumulado
#' - `n_pre_no_long`: personas con PRE hasta la fecha y sin LONG aún
#' - `n_long_no_pre`: personas con LONG hasta la fecha y sin PRE aún
#' - `n_long_post_acum`: personas con LONG y POST hasta la fecha
#' - `n_completos`: personas con PRE+LONG+POST hasta la fecha
#' - `activos`: stock con última LONG en los últimos `cutoff_days`
#' - `abandono`: stock acumulado (salidas en `last + cutoff_days`)
#'
#' @param pre  data.frame con `email`, `date` (fecha PRE, una por persona).
#' @param long data.frame con `email`, `date` (fechas LONG, varias por persona).
#' @param post data.frame con `email`, `date` (fecha POST, una por persona).
#' @param cutoff_days Días sin medir para considerar abandono (por defecto 7).
#' @param by "dia" (default) o "semana".
#' @return tibble con columnas:
#'   `fecha`, `n_pre`, `n_pre_acum`, `n_long_dia`, `n_long_acum`,
#'   `n_long_personas`, `n_post`, `n_post_acum`,
#'   `n_pre_no_long`, `n_long_no_pre`,
#'   `n_long_post_acum`, `n_completos`,
#'   `activos`, `abandono`.
#' @export
metricas_longitudinales <- function(pre, long, post,
                                    cutoff_days = 7,
                                    by = c("dia","semana")) {
  by <- match.arg(by)
  stopifnot(all(c("email","date") %in% names(pre)),
            all(c("email","date") %in% names(long)),
            all(c("email","date") %in% names(post)))
  
  # Fechas a Date y limpieza mínima
  pre  <- dplyr::mutate(pre,  date = as.Date(date))  |> dplyr::filter(!is.na(email), !is.na(date))
  long <- dplyr::mutate(long, date = as.Date(date)) |> dplyr::filter(!is.na(email), !is.na(date))
  post <- dplyr::mutate(post, date = as.Date(date)) |> dplyr::filter(!is.na(email), !is.na(date))
  
  # Rango temporal (unión de fechas)
  min_d <- min(c(pre$date, long$date, post$date), na.rm = TRUE)
  max_d <- max(c(pre$date, long$date, post$date), na.rm = TRUE)
  rango <- seq(min_d, max_d, by = "day")
  eje   <- tibble::tibble(fecha = rango)
  
  # Primeras y últimas por persona
  pre_first  <- pre  |> dplyr::group_by(email) |> dplyr::summarise(pre_first  = min(date), .groups = "drop")
  long_bounds<- long |> dplyr::group_by(email) |> dplyr::summarise(long_first = min(date),
                                                                   long_last  = max(date), .groups = "drop")
  post_first <- post |> dplyr::group_by(email) |> dplyr::summarise(post_first = min(date), .groups = "drop")
  
  # Flujos por día
  pre_day   <- dplyr::count(pre,  date, name = "n_pre")
  long_day  <- dplyr::count(long, date, name = "n_long_dia")
  post_day  <- dplyr::count(post, date, name = "n_post")
  
  # Nuevos por día (primeras ocurrencias por persona)
  pre_new_day   <- dplyr::count(pre_first,  pre_first,  name = "pre_new")
  long_new_day  <- dplyr::count(long_bounds, long_first, name = "long_new")
  post_new_day  <- dplyr::count(post_first, post_first, name = "post_new")
  
  # Intersecciones: día en que "cumplen ambos" = max(fecha_primera_x, fecha_primera_y)
  pre_long_both <- dplyr::inner_join(pre_first, long_bounds, by = "email") |>
    dplyr::mutate(day = pmax(pre_first, long_first)) |>
    dplyr::count(day, name = "pre_long_new")
  long_post_both<- dplyr::inner_join(long_bounds, post_first, by = "email") |>
    dplyr::mutate(day = pmax(long_first, post_first)) |>
    dplyr::count(day, name = "long_post_new")
  completos_new <- pre_first |>
    dplyr::inner_join(long_bounds, by = "email") |>
    dplyr::inner_join(post_first,  by = "email") |>
    dplyr::mutate(day = pmax(pre_first, pmax(long_first, post_first))) |>
    dplyr::count(day, name = "completos_new")
  
  # Salidas (abandono) en (last + cutoff_days)
  exits_day <- long_bounds |>
    dplyr::mutate(exit = long_last + cutoff_days) |>
    dplyr::count(exit, name = "aband_exits")
  
  # Ensamblado diario (joins + NA→0 + cumsum)
  daily <- eje |>
    dplyr::left_join(pre_day,      by = c("fecha" = "date")) |>
    dplyr::left_join(long_day,     by = c("fecha" = "date")) |>
    dplyr::left_join(post_day,     by = c("fecha" = "date")) |>
    dplyr::left_join(pre_new_day,  by = c("fecha" = "pre_first")) |>
    dplyr::left_join(long_new_day, by = c("fecha" = "long_first")) |>
    dplyr::left_join(post_new_day, by = c("fecha" = "post_first")) |>
    dplyr::left_join(pre_long_both,by = c("fecha" = "day")) |>
    dplyr::left_join(long_post_both,by= c("fecha" = "day")) |>
    dplyr::left_join(completos_new,by = c("fecha" = "day")) |>
    dplyr::left_join(exits_day,    by = c("fecha" = "exit")) |>
    tidyr::replace_na(list(
      n_pre = 0L, n_long_dia = 0L, n_post = 0L,
      pre_new = 0L, long_new = 0L, post_new = 0L,
      pre_long_new = 0L, long_post_new = 0L, completos_new = 0L,
      aband_exits = 0L
    )) |>
    dplyr::mutate(
      # Acumulados de flujos
      n_pre_acum      = cumsum(n_pre),
      n_long_acum     = cumsum(n_long_dia),
      n_post_acum     = cumsum(n_post),
      # Personas con primera LONG
      n_long_personas = cumsum(long_new),
      # Intersecciones acumuladas
      pre_long_cum    = cumsum(pre_long_new),
      n_long_post_acum= cumsum(long_post_new),
      n_completos     = cumsum(completos_new),
      # Stocks derivados
      n_pre_no_long   = cumsum(pre_new)  - pre_long_cum,
      n_long_no_pre   = cumsum(long_new) - pre_long_cum,
      abandono        = cumsum(aband_exits)
    ) |>
    # Activos (entradas por long_first, salidas por exit = last+cutoff)
    dplyr::mutate(
      activos = cumsum(long_new) - abandono
    ) |>
    dplyr::select(
      fecha,
      n_pre, n_pre_acum,
      n_long_dia, n_long_acum, n_long_personas,
      n_post, n_post_acum,
      n_pre_no_long, n_long_no_pre,
      n_long_post_acum, n_completos,
      activos, abandono
    )
  
  if (by == "dia") return(daily)
  
  # Semanal: sumar flujos; tomar último para stocks
  weekly <- daily |>
    dplyr::mutate(semana_inicio = as.Date(cut(fecha, "week"))) |>
    dplyr::arrange(fecha) |>
    dplyr::group_by(semana_inicio) |>
    dplyr::summarise(
      n_pre             = sum(n_pre),
      n_pre_acum        = dplyr::last(n_pre_acum),
      n_long_dia        = sum(n_long_dia),
      n_long_acum       = dplyr::last(n_long_acum),
      n_long_personas   = dplyr::last(n_long_personas),
      n_post            = sum(n_post),
      n_post_acum       = dplyr::last(n_post_acum),
      n_pre_no_long     = dplyr::last(n_pre_no_long),
      n_long_no_pre     = dplyr::last(n_long_no_pre),
      n_long_post_acum  = dplyr::last(n_long_post_acum),
      n_completos       = dplyr::last(n_completos),
      activos           = dplyr::last(activos),
      abandono          = dplyr::last(abandono),
      .groups = "drop"
    ) |>
    dplyr::rename(fecha = semana_inicio)
  
  weekly
}
