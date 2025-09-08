#' Métricas longitudinales (día o semana)
#'
#' Calcula series diarias/semanales a partir de PRE (una fila/persona),
#' LONG (múltiples filas/persona) y POST (una fila/persona).
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
#' @details
#' - Las fechas se convierten con `as.Date()`.
#' - **activos**: stock de personas con al menos una LONG y sin “salida”
#'   (entrada por `long_first`, salida en `long_last + cutoff_days`).
#' - **abandono**: acumulado de salidas ocurridas en la fecha
#'   `long_last + cutoff_days`.
#' - Modo **"semana"**: se agregan ventanas de 7 días con
#'   `as.Date(cut(fecha, "week"))` y se toma el **último** valor semanal
#'   para stocks/acumulados.
#'
#' @param pre  data.frame con `email`, `date` (fecha PRE, una por persona).
#' @param long data.frame con `email`, `date` (fechas LONG, varias por persona).
#' @param post data.frame con `email`, `date` (fecha POST, una por persona).
#' @param cutoff_days Días sin medir para considerar abandono (por defecto 7).
#' @param by "dia" (default) o "semana".
#' @return Tibble con columnas:
#'   - `fecha`
#'   - `n_pre`, `n_pre_acum`
#'   - `n_long_dia`, `n_long_acum`, `n_long_personas`
#'   - `n_post`, `n_post_acum`
#'   - `n_pre_no_long`, `n_long_no_pre`
#'   - `n_long_post_acum`, `n_completos`
#'   - `activos`, `abandono`
#'   
#' @examples
#' # Datos mínimos reproducibles
#' pre  <- data.frame(
#'   email = c("a@x.com","b@x.com","c@x.com"),
#'   date  = as.Date(c("2025-01-01","2025-01-03","2025-01-05"))
#' )
#' long <- data.frame(
#'   email = c("a@x.com","a@x.com","b@x.com","c@x.com","c@x.com"),
#'   date  = as.Date(c("2025-01-02","2025-01-09","2025-01-04","2025-01-06","2025-01-20"))
#' )
#' post <- data.frame(
#'   email = c("a@x.com","c@x.com"),
#'   date  = as.Date(c("2025-01-15","2025-01-25"))
#' )
#'   
#' # Métricas diarias
#' d <- metricas_longitudinales(pre, long, post, cutoff_days = 7, by = "dia")
#' head(d)
#'   
#' # Métricas semanales
#' w <- metricas_longitudinales(pre, long, post, cutoff_days = 7, by = "semana")
#' w
#'   
#' @seealso [metricas_padron()], [retencion_semanas()]
#' @family metricas
#'   
#' @export
#'
#' @importFrom dplyr mutate filter group_by summarise count inner_join left_join arrange last select all_of
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
metricas_longitudinales <- function(pre, long, 
                                    post = NULL,
                                    cutoff_days = 7,
                                    by = c("dia","semana")) {
  by <- match.arg(by)
  
  stopifnot(all(c("email","date") %in% names(pre)))
  stopifnot(all(c("email","date") %in% names(long)))
  
  # Si post es NULL -> tibble vacío compatible
  if (is.null(post)) {
    post <- tibble::tibble(email = character(), date = as.Date(character()))
  } else {
    stopifnot(all(c("email","date") %in% names(post)))
  }
  
  # Limpieza mínima
  pre  <- dplyr::mutate(pre,  date = as.Date(.data$date))  |> dplyr::filter(!is.na(.data$email), !is.na(.data$date))
  long <- dplyr::mutate(long, date = as.Date(.data$date))  |> dplyr::filter(!is.na(.data$email), !is.na(.data$date))
  post <- dplyr::mutate(post, date = as.Date(.data$date))  |> dplyr::filter(!is.na(.data$email), !is.na(.data$date))
  
  has_post <- nrow(post) > 0L
  
  # Rango temporal
  fechas <- c(pre$date, long$date, post$date)
  if (length(fechas) == 0L) {
    return(tibble::tibble(
      fecha = as.Date(character()),
      n_pre = integer(), n_pre_acum = integer(),
      n_long_dia = integer(), n_long_average = numeric(), n_long_acum = integer(), n_long_personas = integer(),
      n_post = integer(), n_post_acum = integer(),
      n_pre_no_long = integer(), n_long_no_pre = integer(),
      n_long_post_acum = integer(), n_completos = integer(),
      activos = integer(), abandono = integer()
    ))
  }
  eje <- tibble::tibble(fecha = seq(min(fechas, na.rm=TRUE), max(fechas, na.rm=TRUE), by = "day"))
  
  # Primeras/últimas
  pre_first   <- pre  |> dplyr::group_by(.data$email) |>
    dplyr::summarise(pre_first  = min(.data$date), .groups = "drop")
  long_bounds <- long |> dplyr::group_by(.data$email) |>
    dplyr::summarise(long_first = min(.data$date),
                     long_last  = max(.data$date), .groups = "drop")
  
  # ----- BLOQUE POST ROBUSTO -----
  if (has_post) {
    post_first  <- post |> dplyr::group_by(.data$email) |>
      dplyr::summarise(post_first = min(.data$date), .groups = "drop")
    post_day    <- dplyr::count(post, .data$date, name = "n_post")
    post_new_day <- dplyr::count(post_first, .data$post_first, name = "post_new")
    long_post_both <- dplyr::inner_join(long_bounds, post_first, by = "email") |>
      dplyr::mutate(day = pmax(.data$long_first, .data$post_first)) |>
      dplyr::count(.data$day, name = "long_post_new")
  } else {
    post_first      <- tibble::tibble(email = character(), post_first = as.Date(character()))
    post_day        <- tibble::tibble(date  = as.Date(character()), n_post = integer())
    post_new_day    <- tibble::tibble(post_first = as.Date(character()), post_new = integer())
    long_post_both  <- tibble::tibble(day = as.Date(character()), long_post_new = integer())
  }
  # --------------------------------
  
  # Flujos por día (pre/long)
  pre_day   <- dplyr::count(pre,  .data$date, name = "n_pre")
  long_day  <- dplyr::count(long, .data$date, name = "n_long_dia")
  
  # Nuevos por día (primeras ocurrencias)
  pre_new_day  <- dplyr::count(pre_first,   .data$pre_first,  name = "pre_new")
  long_new_day <- dplyr::count(long_bounds, .data$long_first, name = "long_new")
  
  # Intersecciones sin POST y con POST (esta última ya es vacía si no hay post)
  pre_long_both <- dplyr::inner_join(pre_first, long_bounds, by = "email") |>
    dplyr::mutate(day = pmax(.data$pre_first, .data$long_first)) |>
    dplyr::count(.data$day, name = "pre_long_new")
  
  completos_new <- pre_first |>
    dplyr::inner_join(long_bounds, by = "email") |>
    dplyr::inner_join(post_first,  by = "email") |>
    dplyr::mutate(day = pmax(.data$pre_first, pmax(.data$long_first, .data$post_first))) |>
    dplyr::count(.data$day, name = "completos_new")
  
  # Abandonos
  exits_day <- long_bounds |>
    dplyr::mutate(exit = .data$long_last + cutoff_days) |>
    dplyr::count(.data$exit, name = "aband_exits")
  
  # Ensamblado diario
  daily <- eje |>
    dplyr::left_join(pre_day,        by = c("fecha" = "date")) |>
    dplyr::left_join(long_day,       by = c("fecha" = "date")) |>
    dplyr::left_join(post_day,       by = c("fecha" = "date")) |>
    dplyr::left_join(pre_new_day,    by = c("fecha" = "pre_first")) |>
    dplyr::left_join(long_new_day,   by = c("fecha" = "long_first")) |>
    dplyr::left_join(post_new_day,   by = c("fecha" = "post_first")) |>
    dplyr::left_join(pre_long_both,  by = c("fecha" = "day")) |>
    dplyr::left_join(long_post_both, by = c("fecha" = "day")) |>
    dplyr::left_join(completos_new,  by = c("fecha" = "day")) |>
    dplyr::left_join(exits_day,      by = c("fecha" = "exit")) |>
    tidyr::replace_na(list(
      n_pre = 0L, n_long_dia = 0L, n_post = 0L,
      pre_new = 0L, long_new = 0L, post_new = 0L,
      pre_long_new = 0L, long_post_new = 0L, completos_new = 0L,
      aband_exits = 0L
    )) |>
    dplyr::mutate(
      n_pre_acum       = cumsum(.data$n_pre),
      n_long_acum      = cumsum(.data$n_long_dia),
      n_post_acum      = cumsum(.data$n_post),
      n_long_average   = dplyr::cummean(.data$n_long_dia),
      n_long_personas  = cumsum(.data$long_new),
      pre_long_cum     = cumsum(.data$pre_long_new),
      n_long_post_acum = cumsum(.data$long_post_new),
      n_completos      = cumsum(.data$completos_new),
      n_pre_no_long    = cumsum(.data$pre_new)  - .data$pre_long_cum,
      n_long_no_pre    = cumsum(.data$long_new) - .data$pre_long_cum,
      abandono         = cumsum(.data$aband_exits),
      activos          = cumsum(.data$long_new) - .data$abandono
    ) |>
    dplyr::select(dplyr::all_of(c(
      "fecha",
      "n_pre", "n_pre_acum",
      "n_long_dia", "n_long_average" ,"n_long_acum", "n_long_personas",
      "n_post", "n_post_acum",
      "n_pre_no_long", "n_long_no_pre",
      "n_long_post_acum", "n_completos",
      "activos", "abandono"
    )))
  
  if (by == "dia") return(daily)
  
  weekly <- daily |>
    dplyr::mutate(semana_inicio = as.Date(cut(.data$fecha, "week"))) |>
    dplyr::arrange(.data$fecha) |>
    dplyr::group_by(.data$semana_inicio) |>
    dplyr::summarise(
      n_pre             = sum(.data$n_pre),
      n_pre_acum        = dplyr::last(.data$n_pre_acum),
      n_long_dia        = sum(.data$n_long_dia),
      n_long_average    = mean(.data$n_long_dia),
      n_long_acum       = dplyr::last(.data$n_long_acum),
      n_long_personas   = dplyr::last(.data$n_long_personas),
      n_post            = sum(.data$n_post),
      n_post_acum       = dplyr::last(.data$n_post_acum),
      n_pre_no_long     = dplyr::last(.data$n_pre_no_long),
      n_long_no_pre     = dplyr::last(.data$n_long_no_pre),
      n_long_post_acum  = dplyr::last(.data$n_long_post_acum),
      n_completos       = dplyr::last(.data$n_completos),
      activos           = dplyr::last(.data$activos),
      abandono          = dplyr::last(.data$abandono),
      .groups = "drop"
    ) |>
    dplyr::mutate(fecha = .data$semana_inicio) |>
    dplyr::select(-"semana_inicio") |>
    dplyr::select(dplyr::all_of(c(
      "fecha",
      "n_pre", "n_pre_acum",
      "n_long_dia", "n_long_average", "n_long_acum", "n_long_personas",
      "n_post", "n_post_acum",
      "n_pre_no_long", "n_long_no_pre",
      "n_long_post_acum", "n_completos",
      "activos", "abandono"
    )))
  
  weekly
}

