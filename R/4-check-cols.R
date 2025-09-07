# ------------------------------------------------------------------
# Validaciones mínimas por módulo:
# - check_cols_pre():  email + name
# - check_cols_long(): email + date
# - check_cols_post(): email + name
# Regla general:
#   * Error  -> falta columna / tipo incorrecto / email vacío o NA
#   * Warning-> duplicados
#   * Info   -> validación superada
# ------------------------------------------------------------------

# Helpers internos ---------------------------------------------------

.dupes_vec <- function(x) {
  x <- x[!is.na(x)]
  sort(unique(x[duplicated(x) | duplicated(x, fromLast = TRUE)]))
}

.dupes_pairs <- function(df) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("email", "date")))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$n > 1L) |>
    dplyr::arrange(dplyr::desc(.data$n))
}

#' Validar columnas de PRE
#'
#' Requisitos:
#' - data.frame/tibble
#' - columnas: `email`, `name`
#' - `email` character y sin vacíos/NA
#'
#' @param pre data.frame con `email` y `name`
#' @return (invisible) NULL
#' @export
#' @importFrom utils capture.output head
check_cols_pre <- function(pre) {
  if (!is.data.frame(pre)) cli::cli_abort("`pre` debe ser un data.frame o tibble.")
  
  req <- c("email", "name")
  faltan <- setdiff(req, names(pre))
  if (length(faltan)) cli::cli_abort("Faltan columnas en `pre`: {paste(faltan, collapse = ', ')}")
  
  if (!is.character(pre$email)) cli::cli_abort("`pre$email` debe ser de tipo character.")
  if (any(is.na(pre$email) | pre$email == "")) cli::cli_abort("`pre$email` contiene valores vacíos o NA.")
  
  dups <- .dupes_vec(pre$email)
  if (length(dups)) {
    cli::cli_warn("`pre` contiene emails duplicados: {paste(dups, collapse = ', ')}")
  } else {
    cli::cli_inform("`pre` pasó todas las validaciones.")
  }
  invisible(NULL)
}

#' Validar columnas de LONG
#'
#' Requisitos:
#' - data.frame/tibble
#' - columnas: `email`, `date`
#' - `email` character; `date` clase Date o POSIXt
#' - `email` sin vacíos/NA
#'
#' @param long data.frame con `email` y `date`
#' @return (invisible) NULL
#' @export
check_cols_long <- function(long) {
  if (!is.data.frame(long)) cli::cli_abort("`long` debe ser un data.frame o tibble.")
  
  req <- c("email", "date")
  faltan <- setdiff(req, names(long))
  if (length(faltan)) cli::cli_abort("Faltan columnas en `long`: {paste(faltan, collapse = ', ')}")
  
  if (!is.character(long$email)) cli::cli_abort("`long$email` debe ser de tipo character.")
  if (!(inherits(long$date, "Date") || inherits(long$date, "POSIXt"))) {
    cli::cli_abort("`long$date` debe ser clase Date o POSIXt.")
  }
  if (any(is.na(long$email) | long$email == "")) cli::cli_abort("`long$email` contiene valores vacíos o NA.")
  
  dups <- .dupes_pairs(long)
  if (nrow(dups) > 0) {
    ejemplo <- utils::capture.output(print(utils::head(dups, 10L)))
    cli::cli_warn(c(
      "`long` contiene filas duplicadas en la combinación (email, date).",
      "Top duplicados:" = paste(ejemplo, collapse = "\n")
    ))
  } else {
    cli::cli_inform("`long` pasó todas las validaciones.")
  }
  invisible(NULL)
}

#' Validar columnas de POST
#'
#' Mismas reglas que PRE:
#' - data.frame/tibble
#' - columnas: `email`, `name`
#' - `email` character y sin vacíos/NA
#'
#' @param post data.frame con `email` y `name`
#' @return (invisible) NULL
#' @export
check_cols_post <- function(post) {
  if (!is.data.frame(post)) cli::cli_abort("`post` debe ser un data.frame o tibble.")
  
  req <- c("email", "name")
  faltan <- setdiff(req, names(post))
  if (length(faltan)) cli::cli_abort("Faltan columnas en `post`: {paste(faltan, collapse = ', ')}")
  
  if (!is.character(post$email)) cli::cli_abort("`post$email` debe ser de tipo character.")
  if (any(is.na(post$email) | post$email == "")) cli::cli_abort("`post$email` contiene valores vacíos o NA.")
  
  dups <- .dupes_vec(post$email)
  if (length(dups)) {
    cli::cli_warn("`post` contiene emails duplicados: {paste(dups, collapse = ', ')}")
  } else {
    cli::cli_inform("`post` pasó todas las validaciones.")
  }
  invisible(NULL)
}
