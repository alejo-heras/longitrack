#' Check columnas PRE
#'
#' Valida estructura mínima de `pre`.
#' Errores: columnas faltantes, tipos incorrectos, emails vacíos/NA.
#' Warning: emails duplicados.
#' @param pre data.frame con columnas `email` y `name`.
#' @return (invisible) NULL si todo OK.
#' @export
#' @examples
#' check_cols_pre(data.frame(email = "a@x.com", name = "Ana"))
check_cols_pre <- function(pre) {
  if (!is.data.frame(pre)) stop("`pre` debe ser un data.frame o tibble.")
  req <- c("email", "name")
  faltan <- setdiff(req, names(pre))
  if (length(faltan)) stop("`pre` debe contener: ", paste(faltan, collapse = ", "))
  if (!is.character(pre$email)) stop("`pre$email` debe ser character.")
  if (any(is.na(pre$email) | pre$email == "")) stop("`pre$email` tiene valores vacíos o NA.")
  if (anyDuplicated(pre$email)) warning("`pre` contiene emails duplicados.")
  invisible(NULL)
}


#' Check columnas LONG
#'
#' Valida estructura mínima de `long`.
#' Errores: columnas faltantes, tipos incorrectos, emails vacíos/NA.
#' Warning: filas duplicadas en (email, date).
#' @param long data.frame con columnas `email` y `date`.
#' @return (invisible) NULL si todo OK.
#' @export
#' @examples
#' check_cols_long(data.frame(email="a@x.com", date=as.Date("2025-01-01")))
check_cols_long <- function(long) {
  if (!is.data.frame(long)) stop("`long` debe ser un data.frame o tibble.")
  req <- c("email", "date")
  faltan <- setdiff(req, names(long))
  if (length(faltan)) stop("`long` debe contener: ", paste(faltan, collapse = ", "))
  if (!is.character(long$email)) stop("`long$email` debe ser character.")
  if (!(inherits(long$date, "Date") || inherits(long$date, "POSIXt"))) {
    stop("`long$date` debe ser Date o POSIXt.")
  }
  if (any(is.na(long$email) | long$email == "")) stop("`long$email` tiene valores vacíos o NA.")
  if (any(duplicated(long[c("email","date")]))) {
    warning("`long` contiene filas duplicadas en (email, date).")
  }
  invisible(NULL)
}


#' Check columnas POST
#'
#' Equivalente a PRE (llama internamente a check_cols_pre).
#' @param post data.frame con columnas `email` y `name`.
#' @return (invisible) NULL si todo OK.
#' @export
#' @examples
#' check_cols_post(data.frame(email="a@x.com", name="Ana"))
check_cols_post <- function(post) {
  check_cols_pre(post)
}
