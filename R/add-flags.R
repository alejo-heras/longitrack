#' Añadir flags entre PRE, LONG y POST (a prueba de colisiones)
#'
#' Enrichce tibbles de PRE/LONG/POST con flags de presencia cruzada y
#' métricas agregadas de LONG por email, evitando colisiones de nombre en joins.
#'
#' @param pre  Tibble con una fila por participante (fase PRE). Requiere columna `email`.
#' @param long Tibble con varias filas por participante (fase LONG). Requiere `email` y `date`.
#' @param post Tibble opcional con una fila por participante (fase POST). Requiere `email`.
#' @param summary_prefix Prefijo a aplicar a las columnas agregadas de \code{long}
#'   antes de unir con \code{pre} y \code{post}. Por defecto `"long_"`.
#'   Los nombres objetivos son \code{paste0(summary_prefix, c("n","first_date","last_date"))}.
#'   Si alguno ya existe en el destino, se crea una variante única (p.ej. `"long_n.1"`).
#' @return Lista con tres tibbles: \code{pre}, \code{long}, \code{post} (este último \code{NULL} si no se pasa).
#'
#' @details
#' - Idempotencia: puedes llamar a \code{add_flags()} varias veces sin que se creen columnas duplicadas
#'   por el \code{left_join()}, porque las columnas agregadas de \code{long} se renombran con prefijo
#'   y, en caso de colisión, se desambigüan con \code{make.unique()}.
#' - Flags recalculados: \code{flag_pre}, \code{flag_long}, \code{flag_post} se recalculan en cada llamada
#'   y sobrescriben si ya existen.
#' - Tipos: \code{date} en \code{long} debe ser \code{Date} o coercible a \code{Date}.
#'
#' @examples
#' # res <- add_flags(pre, long)
#' # res$pre; res$long
#'
#' @export
add_flags <- function(pre, long, post = NULL, summary_prefix = "long_") {
  # ---- comprobaciones mínimas
  stopifnot(
    is.data.frame(pre),  is.data.frame(long),
    "email" %in% names(pre),
    "email" %in% names(long),
    "date"  %in% names(long)
  )
  
  # Coerción conservadora de date si es necesario
  if (!inherits(long$date, "Date")) {
    long$date <- as.Date(long$date)
  }
  
  # ---- resumen de LONG por persona (n, primera y última fecha)
  long_summary <- long |>
    dplyr::group_by(.data$email) |>
    dplyr::summarise(
      n          = dplyr::n(),
      first_date = min(.data$date, na.rm = TRUE),
      last_date  = max(.data$date, na.rm = TRUE),
      .groups    = "drop"
    )
  
  pre_ids  <- unique(pre$email)
  long_ids <- unique(long$email)
  post_ids <- if (!is.null(post)) unique(post$email) else character(0)
  
  # helper: left_join que evita colisiones renombrando solo las columnas de y (no la clave)
  safe_left_join <- function(x, y, by = "email", prefix = "long_") {
    # columnas de y que se van a añadir
    y_cols <- setdiff(names(y), by)
    # nombres objetivo con prefijo
    desired <- paste0(prefix, y_cols)
    # si ya existen en x, usamos make.unique para desambiguar
    unique_names <- make.unique(c(names(x), desired))
    desired_unique <- utils::tail(unique_names, length(desired))
    # renombrar y
    y_renamed <- dplyr::rename(y, !!!stats::setNames(y_cols, desired_unique))
    # join
    out <- dplyr::left_join(x, y_renamed, by = by)
    list(
      data = out,
      # devolvemos el mapping por si hace falta (p.ej., para rellenar NAs a 0)
      added_names = desired_unique
    )
  }
  
  # ---- PRE: une resumen de LONG con prefijo seguro y crea flags
  join_pre <- safe_left_join(pre, long_summary, by = "email", prefix = summary_prefix)
  pre2 <- join_pre$data |>
    dplyr::mutate(
      flag_long = .data$email %in% long_ids,
      flag_post = .data$email %in% post_ids
    )
  
  # Rellenar a 0 la columna de conteo (la que corresponda tras desambiguar)
  # Buscamos en added_names la que proviene de "n"
  n_target <- join_pre$added_names[which(paste0(summary_prefix, "n") == paste0(summary_prefix, setdiff(names(long_summary), "email")))]
  if (length(n_target) == 0L) {
    # fallback por si cambia el orden; detecta la que termina en "n" tras prefijo
    n_target <- join_pre$added_names[grepl(paste0("^", gsub("\\W", "\\\\W", summary_prefix), "n(\\.|$)"), join_pre$added_names)]
  }
  if (length(n_target) == 1L && n_target %in% names(pre2)) {
    pre2[[n_target]] <- tidyr::replace_na(pre2[[n_target]], 0L)
  }
  
  # ---- LONG: añade flags (no hace joins, por lo que no hay colisión)
  long2 <- long |>
    dplyr::mutate(
      flag_pre  = .data$email %in% pre_ids,
      flag_post = .data$email %in% post_ids
    )
  
  # ---- POST: opcional, une resumen de LONG con prefijo seguro y crea flags
  if (!is.null(post)) {
    join_post <- safe_left_join(post, long_summary, by = "email", prefix = summary_prefix)
    post2 <- join_post$data |>
      dplyr::mutate(
        flag_long = .data$email %in% long_ids,
        flag_pre  = .data$email %in% pre_ids
      )
    
    # Rellenar a 0 el conteo en post si existe
    if (length(n_target) == 1L && n_target %in% names(post2)) {
      post2[[n_target]] <- tidyr::replace_na(post2[[n_target]], 0L)
    }
  } else {
    post2 <- NULL
  }
  
  # ---- salida
  list(pre = pre2, long = long2, post = post2)
}




