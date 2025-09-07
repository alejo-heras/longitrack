# ------------------------------------------------------------------
# dic_renombrar_cols():
# - Toma un data.frame y un diccionario con columnas "variable"/"etiqueta".
# - Opcionalmente filtra el diccionario por idioma y/o modulo.
# - Limpia nombres/etiquetas, hace el match y renombra columnas.
# - Devuelve lista con:
#     * datos renombrados
#     * columnas sin coincidencia
#     * variables del diccionario no usadas
#     * resumen en texto
# Puede usarse dentro de importar_gs() o de forma independiente.
# ------------------------------------------------------------------

#' Renombrar columnas usando un diccionario
#'
#' Dado un `data.frame` y un diccionario con columnas `variable` y `etiqueta`,
#' renombra columnas cuyos nombres coincidan con `etiqueta`. Opcionalmente filtra
#' el diccionario por `idioma` y/o `modulo` si esas columnas existen.
#'
#' @param datos Data frame con las columnas a renombrar.
#' @param diccionario Puede ser:
#'   - `NULL` (por defecto): se carga el archivo indicado en `path_diccionario`.
#'   - Un `data.frame`: se usa directamente como diccionario.
#'   - Una cadena de texto (ruta): se interpreta como ruta a un archivo Excel (`.xlsx`/`.xls`) o CSV (`.csv`).
#'   Debe contener, como mínimo, columnas `variable` y `etiqueta`. Opcionalmente `idioma` y/o `modulo`.
#' @param path_diccionario Ruta al diccionario cuando `diccionario = NULL` (defecto `"diccionario.xlsx"`).
#' @param sheet_diccionario Hoja si es Excel (nombre o índice). Por defecto `1`.
#' @param idioma (opcional) valor para filtrar `diccionario$idioma`.
#' @param modulo (opcional) valor para filtrar `diccionario$modulo`.
#' @param verbose Mostrar un resumen por consola (`FALSE` por defecto).
#'
#' @return Lista con:
#' \itemize{
#'   \item `datos`: data frame con columnas renombradas
#'   \item `cols_no_renombradas`: nombres del dataset sin coincidencia
#'   \item `vars_no_encontradas`: etiquetas del diccionario no usadas
#'   \item `resumen`: texto de resumen
#' }
#' @export
#' @importFrom utils read.csv
dic_renombrar_cols <- function(datos,
                               diccionario = NULL,
                               path_diccionario = "diccionario.xlsx",
                               sheet_diccionario = 1,
                               idioma = NULL,
                               modulo = NULL,
                               verbose = FALSE) {
  stopifnot(is.data.frame(datos))
  
  # ---- Cargar/normalizar diccionario ----
  cargar_diccionario <- function(path, sheet) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("xlsx","xls")) {
      readxl::read_excel(path, sheet = sheet)
    } else if (ext == "csv") {
      utils::read.csv(path, stringsAsFactors = FALSE)
    } else {
      stop("Extensión de diccionario no soportada: ", ext, " (use .xlsx, .xls o .csv).", call. = FALSE)
    }
  }
  
  if (is.null(diccionario)) {
    if (!file.exists(path_diccionario)) {
      stop("No se encontró el diccionario en: ", path_diccionario, call. = FALSE)
    }
    # si es Excel, validar hoja
    if (tolower(tools::file_ext(path_diccionario)) %in% c("xlsx","xls")) {
      sheets <- readxl::excel_sheets(path_diccionario)
      ok_sheet <- (is.numeric(sheet_diccionario) && sheet_diccionario %in% seq_along(sheets)) ||
        (is.character(sheet_diccionario) && sheet_diccionario %in% sheets)
      if (!ok_sheet) {
        stop("La hoja '", sheet_diccionario, "' no existe. Hojas: ",
             paste(sheets, collapse = ", "), call. = FALSE)
      }
    }
    dic <- cargar_diccionario(path_diccionario, sheet_diccionario)
  } else if (is.data.frame(diccionario)) {
    dic <- diccionario
  } else if (is.character(diccionario) && length(diccionario) == 1L) {
    if (!file.exists(diccionario)) {
      stop("No se encontró el diccionario en: ", diccionario, call. = FALSE)
    }
    dic <- cargar_diccionario(diccionario, sheet_diccionario)
  } else {
    stop("`diccionario` debe ser NULL, data.frame o una ruta a archivo (.xlsx/.xls/.csv).", call. = FALSE)
  }
  
  # ---- Verificaciones mínimas ----
  req_cols <- c("variable","etiqueta")
  if (!all(req_cols %in% names(dic))) {
    stop("El diccionario debe contener columnas: ", paste(req_cols, collapse = ", "), call. = FALSE)
  }
  
  # ---- Filtros opcionales por idioma/modulo (robustos) ----
  norm <- function(x) tolower(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
  if (!is.null(idioma) && "idioma" %in% names(dic)) {
    dic <- dic[norm(dic$idioma) == norm(idioma), , drop = FALSE]
  }
  if (!is.null(modulo) && "modulo" %in% names(dic)) {
    dic <- dic[norm(dic$modulo) == norm(modulo), , drop = FALSE]
  }
  if (nrow(dic) == 0L) {
    stop("Tras filtrar por idioma/modulo, el diccionario quedó vacío. Revisa valores (tildes/espacios/mayúsculas).", call. = FALSE)
  }
  
  # ---- Limpieza de textos (etiquetas y nombres actuales) ----
  clean_label <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)                          # compactar espacios
    x <- stringr::str_replace_all(x, '"', '')            # quitar comillas
    x <- stringr::str_replace_all(x, "[\r\n]+", " ")     # saltos -> espacio
    x <- gsub("/", "-", x, fixed = TRUE)                 # / -> -
    x <- gsub("dd\\s*/\\s*mm\\s*/\\s*aaaa", "dd-mm-aaaa", x, ignore.case = TRUE)
    x
  }
  
  nombres_actuales      <- clean_label(names(datos))
  etiquetas_diccionario <- clean_label(trimws(dic$etiqueta))
  variables_diccionario <- trimws(as.character(dic$variable))
  
  # Aviso opcional por etiquetas duplicadas en el diccionario
  dup_etq <- etiquetas_diccionario[duplicated(etiquetas_diccionario)]
  if (length(dup_etq) > 0L && isTRUE(verbose)) {
    msg <- paste0("Aviso: hay etiquetas duplicadas en el diccionario: ",
                  paste(unique(dup_etq), collapse = ", "),
                  ". Se usará la primera coincidencia para cada etiqueta.")
    cat(msg, "\n")
  }
  
  # ---- Match y renombrado ----
  idx <- match(nombres_actuales, etiquetas_diccionario)          # posición de etiqueta en dic
  nuevo_nombre <- ifelse(!is.na(idx), variables_diccionario[idx], nombres_actuales)
  names(datos) <- nuevo_nombre
  
  # ---- Resumen ----
  cols_no_renombradas <- nombres_actuales[is.na(idx)]
  vars_no_encontradas <- setdiff(etiquetas_diccionario, nombres_actuales)
  
  resumen_lines <- c(
    "Resumen de renombrado:",
    sprintf(" - %d de %d columnas renombradas",
            length(nombres_actuales) - length(cols_no_renombradas),
            length(nombres_actuales)),
    sprintf(" - %d variables del diccionario no utilizadas", length(vars_no_encontradas))
  )
  
  if (length(cols_no_renombradas) > 0) {
    resumen_lines <- c(resumen_lines, "Columnas sin coincidencia en el diccionario:")
    for (col in cols_no_renombradas) {
      pos <- which(nombres_actuales == col)
      etiqueta <- if (length(pos) == 1) sprintf(" - Columna %d: '%s'", pos, col) else sprintf(" - '%s'", col)
      resumen_lines <- c(resumen_lines, etiqueta)
    }
  }
  
  if (length(vars_no_encontradas) > 0) {
    resumen_lines <- c(resumen_lines, "Variables del diccionario no encontradas en los datos:")
    for (etq in vars_no_encontradas) {
      filas <- dic[etiquetas_diccionario == etq, , drop = FALSE]
      for (i in seq_len(nrow(filas))) {
        resumen_lines <- c(resumen_lines, paste0(" - ", filas$variable[i], ": ", filas$etiqueta[i]))
      }
    }
  }
  
  resumen_texto <- paste(resumen_lines, collapse = "\n")
  if (isTRUE(verbose)) cat(resumen_texto, "\n")
  
  list(
    datos = datos,
    cols_no_renombradas = cols_no_renombradas,
    vars_no_encontradas  = vars_no_encontradas,
    resumen = resumen_texto
  )
}
