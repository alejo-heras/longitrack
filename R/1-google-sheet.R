# ------------------------------------------------------------------
# gs_auth(): autentica en Google Sheets con JSON de servicio
# importar_gs(): lee pre/long/post (en es/en), opcionalmente mapea
#   con renombrar_con_diccionario() y castea con class_dictionary().
#
# Pseudocódigo (importar_gs):
# 1) Validar args (idioma(s), modulos) y estructura de urls.
# 2) Resolver diccionario:
#    - NULL  -> cargar de path_diccionario (+ sheet_diccionario)
#    - data.frame -> usar tal cual
#    - character  -> tratar como ruta y cargar
# 3) Para cada (idioma, módulo) pedido:
#      - si falta URL => message y omitir
#      - leer hoja de GSheets
#      - (opcional) añadir columna idioma
#      - (opcional) mapping -> renombrar_con_diccionario(...)
#      - (opcional) cast    -> class_dictionary(...)
# 4) Bind_rows por módulo (combinando idiomas).
# 5) (opcional) check_cols_* por cada módulo presente (después de mapping/cast).
# 6) Devolver:
#      - si 1 módulo: list(datos = <df>, mapeos = <por_idioma>)
#      - si >1 módulo: list(pre = ..., long = ..., post = ...)
# ------------------------------------------------------------------

#' Autenticación con Google Sheets (JSON de servicio)
#' @param path Ruta al JSON de credenciales.
#' @return (invisible) TRUE
#' @export
gs_auth <- function(path) {
  stopifnot(length(path) == 1L, file.exists(path))
  googlesheets4::gs4_auth(path = path)
  invisible(TRUE)
}

#' Importar (y opcionalmente mapear/castear) datos desde Google Sheets
#'
#' Lee cualquiera de los módulos `"pre"`, `"long"` y/o `"post"` en uno o dos
#' idiomas. Omite automáticamente combinaciones idioma/módulo sin URL.
#'
#' @param idioma "es", "en" o "both".
#' @param modulos Vector con cualquiera de c("pre","long","post") o "all".
#' @param urls Lista con estructura:
#'   `list(es = list(pre=..., long=..., post=...), en = list(pre=..., long=..., post=...))`.
#'   Las URLs ausentes se omiten sin error (se informa con `message()`).
#' @param hoja Nombre de la hoja a leer en cada Google Sheet
#'   (defecto "Respuestas de formulario 1").
#' @param diccionario Puede ser:
#'   - `NULL` (por defecto) → se carga `path_diccionario`/`sheet_diccionario`.
#'   - `data.frame` → se usa directamente.
#'   - `character` (ruta a archivo xlsx/csv) → se carga desde esa ruta.
#'   Debe contener, como mínimo, columnas requeridas por las funciones
#'   de mapeo/casteo (p.ej. `variable`/`etiqueta` para mapeo y `variable`/`tipo`
#'   para casteo). Opcionalmente `idioma` y/o `modulo`.
#' @param path_diccionario Ruta por defecto cuando `diccionario = NULL`
#'   (defecto "diccionario.xlsx").
#' @param sheet_diccionario Hoja si es Excel (nombre o índice). Por defecto 1.
#' @param add_idioma Si `TRUE`, añade columna `idioma` tras la lectura.
#' @param mapping Si `TRUE`, aplica `renombrar_con_diccionario()`.
#' @param mapping_verbose Si `TRUE`, imprime resúmenes de mapeo.
#' @param cast Si `TRUE`, aplica `class_dictionary()` (tras el mapeo si lo hubo).
#' @param check Si `TRUE`, valida estructuras mínimas con `check_cols_pre()`,
#'   `check_cols_long()` y/o `check_cols_post()` **después** de mapping/cast
#'   y solo sobre los módulos efectivamente cargados.
#'
#' @return
#' - Si se solicitó **un único** módulo: `list(datos=<df>, mapeos=<lista_por_idioma>)`
#' - Si se solicitaron **varios** módulos: `list(pre = ..., long = ..., post = ...)`
#'   (solo para los módulos realmente cargados).
#'
#' @examples
#' \dontrun{
#' urls <- list(
#'   es = list(pre="https://...", long="https://..."),
#'   en = list(pre="https://...")
#' )
#' # Solo PRE en español, sin mapeo/cast/check:
#' importar_gs(idioma="es", modulos="pre", urls)
#'
#' # PRE+LONG en ambos idiomas, con mapeo y check (omite post si falta):
#' importar_gs(idioma="both", modulos=c("pre","long"), urls,
#'             mapping=TRUE, cast=FALSE, check=TRUE)
#'
#' # ALL (pre,long,post) en ES, con mapeo+cast usando diccionario ya cargado:
#' dic <- readxl::read_excel("diccionario.xlsx", sheet = 1)
#' importar_gs("es", "all", urls, diccionario = dic,
#'             mapping=TRUE, cast=TRUE, check=TRUE)
#' }
#' @export
importar_gs <- function(idioma = c("es","en","both"),
                        modulos = c("pre","long","post","all"),
                        urls,
                        hoja = "Respuestas de formulario 1",
                        diccionario = NULL,
                        path_diccionario = "diccionario.xlsx",
                        sheet_diccionario = 1,
                        add_idioma = TRUE,
                        mapping = TRUE,
                        mapping_verbose = TRUE,
                        cast = FALSE,
                        check = FALSE) {
  
  # -------- Args básicos --------
  idioma  <- match.arg(idioma)
  stopifnot(is.list(urls), all(c("es","en") %in% names(urls)))
  
  # Normalizar 'modulos'
  if (length(modulos) == 1L && tolower(modulos) == "all") {
    modulos <- c("pre","long","post")
  } else {
    # Validar valores
    ok <- modulos %in% c("pre","long","post")
    if (!all(ok)) {
      stop("`modulos` debe ser 'all' o un subconjunto de c('pre','long','post').", call. = FALSE)
    }
    modulos <- unique(modulos)
  }
  
  # -------- Resolver diccionario (solo si mapping o cast) --------
  dic <- NULL
  if (isTRUE(mapping) || isTRUE(cast)) {
    # Helper de carga
    load_dic <- function(path, sheet) {
      ext <- tolower(tools::file_ext(path))
      if (ext %in% c("xlsx","xls")) {
        readxl::read_excel(path, sheet = sheet)
      } else if (ext == "csv") {
        utils::read.csv(path, stringsAsFactors = FALSE)
      } else {
        stop("Extensión del diccionario no soportada: ", ext, call. = FALSE)
      }
    }
    
    if (is.null(diccionario)) {
      if (!file.exists(path_diccionario)) {
        stop("No se encuentra el diccionario en: ", path_diccionario, call. = FALSE)
      }
      # Si es Excel, validar hoja
      if (tolower(tools::file_ext(path_diccionario)) %in% c("xlsx","xls")) {
        sheets <- readxl::excel_sheets(path_diccionario)
        ok_sheet <- (is.numeric(sheet_diccionario) && sheet_diccionario %in% seq_along(sheets)) ||
          (is.character(sheet_diccionario) && sheet_diccionario %in% sheets)
        if (!ok_sheet) {
          stop("La hoja '", sheet_diccionario, "' no existe. Hojas: ",
               paste(sheets, collapse = ", "), call. = FALSE)
        }
      }
      dic <- load_dic(path_diccionario, sheet_diccionario)
      
    } else if (is.data.frame(diccionario)) {
      dic <- diccionario
      
    } else if (is.character(diccionario) && length(diccionario) == 1L) {
      if (!file.exists(diccionario)) {
        stop("No se encontró el diccionario en: ", diccionario, call. = FALSE)
      }
      dic <- load_dic(diccionario, sheet_diccionario)
      
    } else {
      stop("`diccionario` debe ser NULL, data.frame o una ruta a archivo.", call. = FALSE)
    }
  }
  
  # -------- Parámetros comunes --------
  idiomas <- if (idioma == "both") c("es","en") else idioma
  mod_map <- c(pre = "pretest", long = "seguimiento", post = "postest")  # etiqueta 'modulo' en dic
  
  get_url <- function(idm, mod) {
    x <- tryCatch(urls[[idm]][[mod]], error = function(e) NULL)
    if (is.null(x) || !nzchar(x)) return(NULL)
    x
  }
  
  resumen_skip <- function(idm, mod) {
    list(
      cols_no_renombradas = character(0),
      vars_no_encontradas = character(0),
      resumen = sprintf("Mapping desactivado (mapping = FALSE) para %s/%s.", idm, mod)
    )
  }
  
  # -------- Lector por (idioma, módulo) --------
  leer_unit <- function(idm, mod) {
    u <- get_url(idm, mod)
    if (is.null(u)) {
      message(sprintf("Omitiendo %s/%s: URL no proporcionada.", idm, mod))
      return(NULL)
    }
    
    df <- googlesheets4::read_sheet(u, sheet = hoja)
    
    if (isTRUE(add_idioma)) df$idioma <- idm
    
    # Mapping
    if (!isTRUE(mapping)) {
      out <- c(list(datos = df), resumen_skip(idm, mod))
    } else {
      out <- renombrar_con_diccionario(
        datos       = df,
        diccionario = dic,
        idioma      = idm,
        modulo      = unname(mod_map[[mod]]),
        verbose     = FALSE
      )
    }
    
    # Cast
    if (isTRUE(cast)) {
      out$datos <- class_dictionary(
        datos       = out$datos,
        diccionario = dic,
        idioma      = idm,
        modulo      = unname(mod_map[[mod]])
      )
    }
    
    out
  }
  
  solo_resumen <- function(x) x[c("cols_no_renombradas", "vars_no_encontradas", "resumen")]
  
  # -------- Ejecutar por módulo --------
  build_modulo <- function(mod) {
    # leer por idioma, omitiendo NULLs
    res_list <- stats::setNames(lapply(idiomas, leer_unit, mod = mod), idiomas)
    res_list <- Filter(Negate(is.null), res_list)
    if (!length(res_list)) {
      message(sprintf("No se cargaron datos para '%s'.", mod))
      return(NULL)
    }
    
    # bind de datos por idioma
    df <- dplyr::bind_rows(lapply(res_list, `[[`, "datos"))
    
    if (isTRUE(mapping_verbose) && isTRUE(mapping)) {
      cat(paste(vapply(res_list, function(z) z$resumen, ""), collapse = "\n\n"), "\n\n")
    }
    
    # check (después de mapping/cast)
    if (isTRUE(check)) {
      if (mod == "pre")  check_cols_pre(df)
      if (mod == "long") check_cols_long(df)
      if (mod == "post") check_cols_post(df)
    }
    
    list(datos = df, mapeos = lapply(res_list, solo_resumen))
  }
  
  # ¿Uno o varios módulos?
  if (length(modulos) == 1L) {
    mod_res <- build_modulo(modulos)
    if (is.null(mod_res)) {
      stop(sprintf("No se pudo cargar el módulo '%s' (no había ninguna URL válida).", modulos),
           call. = FALSE)
    }
    return(mod_res)
  }
  
  # Varios módulos
  out <- lapply(modulos, build_modulo)
  names(out) <- modulos
  # Quitar los NULL (módulos vacíos)
  out <- Filter(Negate(is.null), out)
  if (!length(out)) {
    stop("No se pudo cargar ningún módulo (revisa URLs/idiomas).", call. = FALSE)
  }
  out
}
