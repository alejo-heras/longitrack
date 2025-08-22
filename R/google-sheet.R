

#' Autenticación con Google Sheets (JSON de servicio)
#' @param path Ruta al JSON de credenciales.
#' @return (invisible) TRUE
#' @export
gs_auth <- function(path) {
  stopifnot(length(path) == 1L, file.exists(path))
  googlesheets4::gs4_auth(path = path)
  invisible(TRUE)
}



#' Importar (y opcionalmente mapear) datos desde Google Sheets
#'
#' Lee `pre` o `long` (o ambos) para uno o dos idiomas y, por defecto,
#' aplica el diccionario `diccionario.xlsx` de la raíz del proyecto con
#' `renombrar_con_diccionario()`.
#'
#' @param idioma "es", "en" o "both".
#' @param modulo "pre", "long" o "both".
#' @param urls lista con estructura:
#'   list(es = list(pre=..., long=...), en = list(pre=..., long=...))
#' @param hoja nombre de la hoja a leer (por defecto "Respuestas de formulario 1").
#' @param path_diccionario ruta al Excel del diccionario.
#'   Por defecto "diccionario.xlsx" en el directorio de trabajo actual, 
#'   salvo que se especifique lo contrario
#' @param sheet_diccionario Nombre (character) o índice (numeric) de la hoja del
#'   Excel del diccionario. Por defecto 1. Si no existe, se informa el listado
#'   de hojas disponibles.
#' @param add_idioma si TRUE añade columna `idioma` a los datos leídos.
#' @param mapping si FALSE, no se aplica mapeo (por defecto TRUE).
#' @param mapping_verbose si TRUE imprime los resúmenes de mapeo (por defecto TRUE).
#'
#' @return
#' - Si `modulo = "both"`:
#'   `list(pre = list(datos=<df>, mapeos=<lista_por_idioma>),
#'         long = list(datos=<df>, mapeos=<lista_por_idioma>))`
#' - Si `modulo != "both"`:
#'   `list(datos=<df>, mapeos=<lista_por_idioma>)`
#'
#' @export
importar_gs <- function(idioma = c("es","en","both"),
                        modulo = c("pre","long","both"),
                        urls,
                        hoja = "Respuestas de formulario 1",
                        path_diccionario = "diccionario.xlsx",
                        sheet_diccionario = 1,
                        add_idioma = TRUE,
                        mapping = TRUE,
                        mapping_verbose = TRUE) {

  idioma <- match.arg(idioma)
  modulo <- match.arg(modulo)
  stopifnot(is.list(urls), all(c("es","en") %in% names(urls)))

  # Cargar diccionario solo si se mapea
  dic <- NULL
  if (isTRUE(mapping)) {
    if (!file.exists(path_diccionario))
      stop("No se encuentra el diccionario en: ", path_diccionario, call. = FALSE)
    
    # Validación de hoja
    sheets <- readxl::excel_sheets(path_diccionario)
    ok <- (is.numeric(sheet_diccionario) && sheet_diccionario %in% seq_along(sheets)) ||
      (is.character(sheet_diccionario) && sheet_diccionario %in% sheets)
    if (!ok) {
      stop(
        "La hoja '", sheet_diccionario, "' no existe en el diccionario. ",
        "Hojas disponibles: ", paste(sheets, collapse = ", "),
        call. = FALSE
      )
    }
    
    dic <- readxl::read_excel(path_diccionario, sheet = sheet_diccionario)
  }

  # pre -> "pretest", long -> "seguimiento" (para el diccionario)
  mod_map <- c(pre = "pretest", long = "seguimiento")

  get_url <- function(idm, mod) {
    u <- urls[[idm]][[mod]]
    if (is.null(u) || !nzchar(u))
      stop(sprintf("URL faltante para %s/%s", idm, mod), call. = FALSE)
    u
  }

  # Construye un "resultado de mapeo" estándar cuando mapping = FALSE
  resumen_skip <- function(idm, mod) {
    list(
      cols_no_renombradas = character(0),
      vars_no_encontradas = character(0),
      resumen = sprintf("Mapping desactivado (mapping = FALSE) para %s/%s.", idm, mod)
    )
  }

  # Lee y (si procede) mapea; SIEMPRE devuelve lista con datos + resumen/métricas
  leer_unit <- function(idm, mod) {
    df <- googlesheets4::read_sheet(get_url(idm, mod), sheet = hoja)
    if (isTRUE(add_idioma)) df$idioma <- idm

    if (!isTRUE(mapping)) {
      return(c(list(datos = df), resumen_skip(idm, mod)))
    }

    # mapping = TRUE
    out <- renombrar_con_diccionario(
      datos       = df,
      diccionario = dic,
      idioma      = idm,
      modulo      = unname(mod_map[[mod]]),
      verbose     = FALSE
    )
    # out es lista: datos, cols_no_renombradas, vars_no_encontradas, resumen
    out
  }

  # Helper para quedarse solo con los campos de resumen
  solo_resumen <- function(x) x[c("cols_no_renombradas", "vars_no_encontradas", "resumen")]

  idiomas <- if (idioma == "both") c("es","en") else idioma

  if (modulo == "both") {
    pre_res  <- stats::setNames(lapply(idiomas, leer_unit, mod = "pre"),  idiomas)
    long_res <- stats::setNames(lapply(idiomas, leer_unit, mod = "long"), idiomas)

    pre_df   <- dplyr::bind_rows(lapply(pre_res,  `[[`, "datos"))
    long_df  <- dplyr::bind_rows(lapply(long_res, `[[`, "datos"))

    if (isTRUE(mapping_verbose)) {
      cat(paste(vapply(pre_res,  function(z) z$resumen,  ""), collapse = "\n\n"), "\n\n")
      cat(paste(vapply(long_res, function(z) z$resumen, ""), collapse = "\n\n"), "\n\n")
    }

    return(list(
      pre  = list(datos = pre_df,  mapeos = lapply(pre_res,  solo_resumen)),
      long = list(datos = long_df, mapeos = lapply(long_res, solo_resumen))
    ))
  }

  # modulo único ("pre" o "long")
  res_list <- stats::setNames(lapply(idiomas, leer_unit, mod = modulo), idiomas)
  df       <- dplyr::bind_rows(lapply(res_list, `[[`, "datos"))

  if (isTRUE(mapping_verbose)) {
    cat(paste(vapply(res_list, function(z) z$resumen, ""), collapse = "\n\n"), "\n\n")
  }

  list(datos = df, mapeos = lapply(res_list, solo_resumen))
}

