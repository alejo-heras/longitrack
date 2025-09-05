# ------------------------------------------------------------------
# Este script define funciones para validar estructuras mínimas:
# - check_cols_pre(): valida datos PRE (email + name).
# - check_cols_long(): valida datos LONG (email + date).
# - check_cols_post(): valida POST (equivalente a PRE).
#
# Cada función verifica:
#   1. Que el objeto sea un data.frame/tibble.
#   2. Que existan las columnas obligatorias.
#   3. Que las columnas tengan el tipo esperado.
#   4. Que no haya valores vacíos o NA en `email`.
#   5. Condiciones adicionales (duplicados).
# Si algo falla → error (stop).
# Si hay duplicados → warning.
# Si todo está correcto → message de validación exitosa.
# ------------------------------------------------------------------

# Helper: formatea y avisa duplicados en 'cols'
.warn_dups <- function(df, cols, tag) {
  # filas implicadas en duplicidad (en cualquier sentido)
  idx <- duplicated(df[cols]) | duplicated(df[cols], fromLast = TRUE)
  if (any(idx)) {
    keys <- unique(df[idx, cols, drop = FALSE])
    # ordenar por las columnas clave (solo base R)
    ord  <- do.call(order, as.list(keys))
    keys <- keys[ord, , drop = FALSE]
    
    # construimos un mensaje legible con los valores duplicados
    cab <- sprintf("`%s` contiene duplicados por {%s}. Valores implicados:",
                   tolower(tag),
                   paste(cols, collapse = ", "))
    cuerpo <- utils::capture.output(print(keys, row.names = FALSE))
    warning(paste(c(cab, cuerpo), collapse = "\n"), call. = FALSE)
  } else {
    message(sprintf("`%s` pasó todas las validaciones.", tolower(tag)))
  }
  invisible(NULL)
}

.dupes_vec <- function(x) {
  x <- x[!is.na(x)]
  sort(unique(x[duplicated(x) | duplicated(x, fromLast = TRUE)]))
}

# --- helper: pares duplicados (email, date) -----------------------
.dupes_pairs <- function(df) {
  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c("email", "date")))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.data$n > 1L) |>
    dplyr::arrange(dplyr::desc(.data$n))
}


#' Check columnas PRE
#'
#' Verifica que un data.frame de tipo PRE cumpla:
#' - Es un data.frame o tibble.
#' - Tiene columnas obligatorias: `email`, `name`.
#' - `email` es character.
#' - `email` no contiene valores vacíos ni NA.
#'
#' Lanza:
#' - **Error** si falta alguna columna, el tipo es incorrecto, o hay emails vacíos/NA.
#' - **Warning** si hay emails duplicados.
#' - **Message** confirmando validación exitosa si todo está bien.
#'
#' @param pre data.frame con columnas `email` y `name`.
#' @return (invisible) NULL
#' @export
#' @examples
#' check_cols_pre(data.frame(email = "a@x.com", name = "Ana"))
check_cols_pre <- function(pre) {
  if (!is.data.frame(pre)) stop("`pre` debe ser un data.frame o tibble.", call. = FALSE)
  
  req <- c("email", "name")
  faltan <- setdiff(req, names(pre))
  if (length(faltan)) stop("`pre` debe contener: ", paste(faltan, collapse = ", "), call. = FALSE)
  
  if (!is.character(pre$email)) stop("`pre$email` debe ser character.", call. = FALSE)
  if (any(is.na(pre$email) | pre$email == "")) stop("`pre$email` tiene valores vacíos o NA.", call. = FALSE)
  
  dups <- .dupes_vec(pre$email)
  if (length(dups)) {
    warning("`pre` contiene emails duplicados: ", paste(dups, collapse = ", "), call. = FALSE)
  } else {
    message("`pre` pasó todas las validaciones.")
  }
  
  invisible(NULL)
}

#' Check columnas LONG
#'
#' Verifica que un data.frame de tipo LONG cumpla:
#' - Es un data.frame o tibble.
#' - Tiene columnas obligatorias: `email`, `date`.
#' - `email` es character.
#' - `date` es de clase Date o POSIXt.
#' - `email` no contiene valores vacíos ni NA.
#'
#' Lanza:
#' - **Error** si falta alguna columna, los tipos son incorrectos, o hay emails vacíos/NA.
#' - **Warning** si hay filas duplicadas en la combinación (`email`, `date`).
#' - **Message** confirmando validación exitosa si todo está bien.
#'
#' @param long data.frame con columnas `email` y `date`.
#' @return (invisible) NULL
#' @export
#' @examples
#' check_cols_long(data.frame(email="a@x.com", date=as.Date("2025-01-01")))
check_cols_long <- function(long) {
  if (!is.data.frame(long)) stop("`long` debe ser un data.frame o tibble.", call. = FALSE)
  
  req <- c("email", "date")
  faltan <- setdiff(req, names(long))
  if (length(faltan)) stop("`long` debe contener: ", paste(faltan, collapse = ", "), call. = FALSE)
  
  if (!is.character(long$email)) stop("`long$email` debe ser character.", call. = FALSE)
  if (!(inherits(long$date, "Date") || inherits(long$date, "POSIXt"))) {
    stop("`long$date` debe ser Date o POSIXt.", call. = FALSE)
  }
  if (any(is.na(long$email) | long$email == "")) stop("`long$email` tiene valores vacíos o NA.", call. = FALSE)
  
  dups <- .dupes_pairs(long)
  if (nrow(dups) > 0) {
    ejemplo <- utils::capture.output(print(utils::head(dups, 10L)))
    warning(
      "`long` contiene filas duplicadas en (email, date). Top duplicados:\n",
      paste(ejemplo, collapse = "\n"),
      call. = FALSE
    )
  } else {
    message("`long` pasó todas las validaciones.")
  }
  
  invisible(NULL)
}

#' Check columnas POST
#'
#' Verifica que un data.frame de tipo POST cumpla
#' los mismos requisitos que PRE:
#' - Es un data.frame o tibble.
#' - Tiene columnas `email`, `name`.
#' - `email` es character.
#' - `email` no contiene vacíos o NA.
#'
#' Lanza:
#' - **Error** si falla alguna condición.
#' - **Warning** si hay emails duplicados.
#' - **Message** confirmando validación exitosa si todo está bien.
#'
#' @param post data.frame con columnas `email` y `name`.
#' @return (invisible) NULL
#' @export
#' @importFrom utils capture.output head
#' @examples
#' check_cols_post(data.frame(email="a@x.com", name="Ana"))
check_cols_post <- function(post) {
  if (!is.data.frame(post)) stop("`post` debe ser un data.frame o tibble.", call. = FALSE)
  
  req <- c("email", "name")
  faltan <- setdiff(req, names(post))
  if (length(faltan)) stop("`post` debe contener: ", paste(faltan, collapse = ", "), call. = FALSE)
  
  if (!is.character(post$email)) stop("`post$email` debe ser character.", call. = FALSE)
  if (any(is.na(post$email) | post$email == "")) stop("`post$email` tiene valores vacíos o NA.", call. = FALSE)
  
  dups <- .dupes_vec(post$email)
  if (length(dups)) {
    warning("`post` contiene emails duplicados: ", paste(dups, collapse = ", "), call. = FALSE)
  } else {
    message("`post` pasó todas las validaciones.")
  }
  
  invisible(NULL)
}
