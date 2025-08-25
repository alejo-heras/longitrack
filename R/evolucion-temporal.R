#' Evolución temporal de actividad por email (pre vs long)
#'
#' @description
#' Une datos "pre" y "long" por `email` y `date` y calcula, por periodo,
#' eventos totales, personas únicas, nuevas (primera vez) y recurrentes,
#' con desglose por fuente (pre/long) y totales.
#'
#' @param pre  Data frame con columnas de email y fecha.
#' @param long Data frame con columnas de email y fecha.
#' @param email_col Nombre de la columna con el email (en ambos data frames). Por defecto "email".
#' @param date_col  Nombre de la columna con la fecha (en ambos data frames). Por defecto "date".
#' @param nivel    Granularidad temporal: "dia", "semana" o "mes". Por defecto "semana".
#' @param desde    Fecha mínima (Date, POSIXct o cadena "YYYY-MM-DD"). Si `NULL`, usa el mínimo observado.
#' @param hasta    Fecha máxima (Date, POSIXct o cadena "YYYY-MM-DD"). Si `NULL`, usa el máximo observado.
#' @param incluir_total Si `TRUE`, añade columnas agregadas totales además de las de cada fuente.
#'
#' @return Un tibble con una fila por periodo y columnas:
#' \itemize{
#'   \item \code{periodo_inicio}, \code{periodo_fin}
#'   \item \code{eventos_pre}, \code{personas_pre}
#'   \item \code{eventos_long}, \code{personas_long}
#'   \item \code{eventos_total}, \code{personas_total}
#'   \item \code{nuevos_total}, \code{recurrentes_total}
#'   \item \code{acumulado_personas} (distintos global acumulado)
#' }
#'
#' @examples
#' # evolucion_temporal(pre_df, long_df, nivel = "mes", desde = "2023-01-01")
#'
#' @export
evolucion_temporal <- function(pre,
                               long,
                               email_col = "email",
                               date_col  = "date",
                               nivel = c("semana","mes","dia"),
                               desde = NULL,
                               hasta = NULL,
                               incluir_total = TRUE) {
  
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("lubridate", quietly = TRUE)
  requireNamespace("rlang", quietly = TRUE)
  requireNamespace("purrr", quietly = TRUE)
  
  nivel <- match.arg(nivel)
  
  # Helpers ----
  to_date <- function(x) {
    if (inherits(x, "Date")) return(x)
    if (inherits(x, "POSIXt")) return(as.Date(x))
    # try character
    suppressWarnings(as.Date(x))
  }
  
  floor_unit <- switch(nivel,
                       dia    = function(d) d,
                       semana = function(d) lubridate::floor_date(d, unit = "week", week_start = 1),
                       mes    = function(d) lubridate::floor_date(d, unit = "month")
  )
  
  ceil_unit_minus1 <- switch(nivel,
                             dia    = function(d) d,
                             semana = function(d) lubridate::ceiling_date(d, unit = "week", week_start = 1) - 1,
                             mes    = function(d) lubridate::ceiling_date(d, unit = "month") - 1
  )
  
  # Estandariza columnas y une fuentes ----
  std_cols <- function(df, fuente) {
    if (is.null(df) || nrow(df) == 0) {
      return(dplyr::tibble(fuente = character(), email = character(), fecha = as.Date(character())))
    }
    email_sym <- rlang::sym(email_col)
    date_sym  <- rlang::sym(date_col)
    
    out <- df |>
      dplyr::mutate(
        email = !!email_sym,
        fecha = !!date_sym
      ) |>
      dplyr::select(email, fecha) |>
      dplyr::mutate(
        fuente = fuente,
        .after = 1
      )
    
    out
  }
  
  datos <- dplyr::bind_rows(
    std_cols(pre,  "pre"),
    std_cols(long, "long")
  )
  
  # Limpieza y rango ----
  datos <- datos |>
    dplyr::filter(!is.na(email)) |>
    dplyr::mutate(fecha = to_date(fecha)) |>
    dplyr::filter(!is.na(fecha))
  
  if (nrow(datos) == 0) {
    return(dplyr::tibble(
      periodo_inicio = as.Date(character()),
      periodo_fin = as.Date(character())
    ))
  }
  
  rango_min <- min(datos$fecha, na.rm = TRUE)
  rango_max <- max(datos$fecha, na.rm = TRUE)
  
  if (!is.null(desde)) {
    desde <- to_date(desde)
  } else {
    desde <- rango_min
  }
  if (!is.null(hasta)) {
    hasta <- to_date(hasta)
  } else {
    hasta <- rango_max
  }
  
  datos <- datos |>
    dplyr::filter(fecha >= desde, fecha <= hasta)
  
  if (nrow(datos) == 0) {
    return(dplyr::tibble(
      periodo_inicio = as.Date(character()),
      periodo_fin = as.Date(character())
    ))
  }
  
  # Periodificación ----
  datos <- datos |>
    dplyr::mutate(
      periodo_inicio = floor_unit(fecha),
      periodo_fin    = ceil_unit_minus1(fecha)
    )
  
  # Primera aparición global (para "nuevos") ----
  primeras <- datos |>
    dplyr::group_by(email) |>
    dplyr::summarise(primera_fecha = min(fecha, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(primera_periodo = floor_unit(primera_fecha))
  
  datos <- datos |>
    dplyr::left_join(primeras, by = "email") |>
    dplyr::mutate(es_nuevo_en_periodo = (periodo_inicio == primera_periodo))
  
  # Resumen por periodo y fuente ----
  por_fuente <- datos |>
    dplyr::group_by(periodo_inicio, periodo_fin, fuente) |>
    dplyr::summarise(
      eventos  = dplyr::n(),
      personas = dplyr::n_distinct(email),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      names_from = fuente,
      values_from = c(eventos, personas),
      values_fill = 0
    )
  
  # Totales y nuevos/recurrentes ----
  totales <- datos |>
    dplyr::group_by(periodo_inicio, periodo_fin) |>
    dplyr::summarise(
      eventos_total   = dplyr::n(),
      personas_total  = dplyr::n_distinct(email),
      nuevos_total    = dplyr::n_distinct(dplyr::if_else(es_nuevo_en_periodo, email, NA_character_)),
      .groups = "drop"
    ) |>
    dplyr::mutate(recurrentes_total = personas_total - nuevos_total)
  
  # Acumulado de personas únicas (global)
  acumulado <- datos |>
    dplyr::arrange(periodo_inicio, email) |>
    dplyr::group_by(periodo_inicio) |>
    dplyr::summarise(personas_distintas_periodo = dplyr::n_distinct(email), .groups = "drop") |>
    dplyr::arrange(periodo_inicio) |>
    dplyr::mutate(acumulado_personas = cumsum(personas_distintas_periodo)) |>
    dplyr::select(periodo_inicio, acumulado_personas)
  
  # Unimos todo ----
  res <- totales |>
    dplyr::left_join(por_fuente, by = c("periodo_inicio","periodo_fin")) |>
    dplyr::left_join(acumulado,  by = "periodo_inicio") |>
    dplyr::arrange(periodo_inicio)
  
  # Si no se desean los totales, se pueden quitar (dejamos opción simple)
  if (!isTRUE(incluir_total)) {
    res <- res |>
      dplyr::select(
        periodo_inicio, periodo_fin,
        dplyr::matches("^eventos_(pre|long)$"),
        dplyr::matches("^personas_(pre|long)$"),
        acumulado_personas
      )
  }
  
  # Columnas ordenadas y seguras (añade las que falten si una fuente estaba vacía)
  for (nm in c("eventos_pre","eventos_long","personas_pre","personas_long")) {
    if (!nm %in% names(res)) res[[nm]] <- 0L
  }
  
  dplyr::relocate(
    res,
    periodo_inicio, periodo_fin,
    eventos_pre, eventos_long, eventos_total,
    personas_pre, personas_long, personas_total,
    nuevos_total, recurrentes_total, acumulado_personas
  )
}
