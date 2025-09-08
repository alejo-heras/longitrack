# ------------------------------------------------------------------
# gs_auth(): autentica en Google Sheets con JSON de servicio
# importar_gs(): lee pre/long/post (en es/en), opcionalmente renombra
#   con dic_renombrar_cols() y asigna tipos con dic_asignar_tipos().
#
# Flujo (importar_gs):
# 1) Validar args (idioma(s), módulos) y estructura de urls.
# 2) Resolver diccionario:
#    - NULL        -> cargar de path_diccionario (+ sheet_diccionario)
#    - data.frame  -> usar tal cual
#    - character   -> tratar como ruta y cargar
# 3) Para cada (idioma, módulo) solicitado:
#      - si falta URL => informar y omitir
#      - leer hoja de GSheets
#      - (opcional) añadir columna idioma
#      - (opcional) renombrar columnas -> dic_renombrar_cols(...)
#      - (opcional) asignar tipos       -> dic_asignar_tipos(...)
# 4) Unir por módulo (combinando idiomas) con bind_rows.
# 5) (opcional) check_cols_* por cada módulo presente (después de renombrar y asignar tipos).
# 6) Devolver:
#      - si 1 módulo: list(datos = <df>, mapeos = <por_idioma>)
#      - si >1 módulo: list(pre = ..., long = ..., post = ...)
# ------------------------------------------------------------------

#' Autenticación con Google Sheets (JSON de servicio)
#' 
#' Necesario cuando los datos que tenemos en Google Sheets no están en abierto.
#' 
#' @param path Ruta al JSON de credenciales.
#' @return (invisible) TRUE
#' @export
gs_auth <- function(path) {
  stopifnot(length(path) == 1L, file.exists(path))
  googlesheets4::gs4_auth(path = path)
  invisible(TRUE)
}

#' Importar datos desde Google Sheets (opcionalmente renombrar, chequear y asignar tipos)
#'
#' Lee cualquiera de los módulos `"pre"`, `"long"` y/o `"post"` en uno o dos
#' idiomas simultaneamente. Omite automáticamente combinaciones idioma/módulo sin URL.
#'
#' @param idioma `"es"`, `"en"` o `"both"`.
#' @param modulos Vector con cualquiera de `c("pre","long","post")` o `"all"`.
#' @param urls Lista con estructura:
#'   `list(es = list(pre=..., long=..., post=...), en = list(pre=..., long=..., post=...))`.
#'   Las URLs ausentes se omiten.
#' @param hoja Nombre de la hoja a leer en cada Google Sheet
#'   (por defecto `"Respuestas de formulario 1"`).
#' @param diccionario Puede ser:
#'   - `NULL` (por defecto) → se carga de `path_diccionario`/`sheet_diccionario`.
#'   - `data.frame` → se usa directamente.
#'   - `character` (ruta a `.xlsx`/`.csv`) → se carga desde esa ruta.
#'   Debe contener, como mínimo, las columnas que requieren las funciones de
#'   renombrado y coerción de clase (p.ej. `variable`/`etiqueta` para renombrar y `variable`/`tipo`
#'   para tipos de variable). Opcionalmente `idioma` y/o `modulo`.
#' @param path_diccionario Ruta por defecto cuando `diccionario = NULL`
#'   (defecto `"diccionario.xlsx"`).
#' @param sheet_diccionario Hoja si es Excel (nombre o índice). Por defecto `1`.
#' @param add_idioma Si `TRUE`, añade columna `idioma` tras la lectura.
#' @param renombrar Si `TRUE`, aplica `dic_renombrar_cols()`.
#' @param renombrar_verbose Si `TRUE`, imprime resúmenes del renombrado.
#' @param asignar_tipos Si `TRUE`, aplica `dic_asignar_tipos()` (tras el renombrado si lo hubo).
#' @param validar Si `TRUE`, valida estructuras mínimas con `check_cols_pre()`,
#'   `check_cols_long()` y/o `check_cols_post()` **después** de renombrar/tipar
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
#' # Solo PRE en español, sin renombrar/tipos/validación:
#' importar_gs(idioma="es", modulos="pre", urls)
#'
#' # PRE+LONG en ambos idiomas, con renombrado y validación:
#' importar_gs(idioma="both", modulos=c("pre","long"), urls,
#'             renombrar=TRUE, asignar_tipos=FALSE, validar=TRUE)
#'
#' # ALL (pre,long,post) en ES, con renombrado+tipos usando diccionario ya cargado:
#' dic <- readxl::read_excel("diccionario.xlsx", sheet = 1)
#' importar_gs("es", "all", urls, diccionario = dic,
#'             renombrar=TRUE, asignar_tipos=TRUE, validar=TRUE)
#' }
#' @export
#' @importFrom utils read.csv
importar_gs <- function(idioma = c("es","en","both"),
                        modulos = c("pre","long","post","all"),
                        urls,
                        hoja = "Respuestas de formulario 1",
                        diccionario = NULL,
                        path_diccionario = "diccionario.xlsx",
                        sheet_diccionario = 1,
                        add_idioma = TRUE,
                        renombrar = TRUE,
                        renombrar_verbose = TRUE,
                        asignar_tipos = FALSE,
                        validar = FALSE) {
  
  # -------- Validación básica de argumentos --------
  idioma <- match.arg(idioma)
  
  if (!is.list(urls) || !all(c("es","en") %in% names(urls))) {
    cli::cli_abort("`urls` debe ser una lista con nombres {.val es} y {.val en}, cada uno con sublistas {.val pre}, {.val long}, {.val post}.")
  }
  
  # Normalizar 'modulos'
  if (length(modulos) == 1L && is.character(modulos) && tolower(modulos) == "all") {
    modulos <- c("pre","long","post")
  } else {
    ok <- modulos %in% c("pre","long","post")
    if (!all(ok)) {
      cli::cli_abort("`modulos` debe ser 'all' o un subconjunto de {.code c('pre','long','post')}.")
    }
    modulos <- unique(modulos)
  }
  
  # -------- Resolver diccionario (solo si hay renombrado o tipado) --------
  dic <- NULL
  if (isTRUE(renombrar) || isTRUE(asignar_tipos)) {
    
    cargar_diccionario <- function(path, sheet) {
      ext <- tolower(tools::file_ext(path))
      if (ext %in% c("xlsx","xls")) {
        readxl::read_excel(path, sheet = sheet)
      } else if (ext == "csv") {
        utils::read.csv(path, stringsAsFactors = FALSE)
      } else {
        cli::cli_abort("Extensión de diccionario no soportada: {.val {ext}} (use .xlsx, .xls o .csv).")
      }
    }
    
    if (is.null(diccionario)) {
      if (!file.exists(path_diccionario)) {
        cli::cli_abort("No se encontró el diccionario en {.path {path_diccionario}}.")
      }
      if (tolower(tools::file_ext(path_diccionario)) %in% c("xlsx","xls")) {
        sheets <- readxl::excel_sheets(path_diccionario)
        ok_sheet <- (is.numeric(sheet_diccionario) && sheet_diccionario %in% seq_along(sheets)) ||
          (is.character(sheet_diccionario) && sheet_diccionario %in% sheets)
        if (!ok_sheet) {
          cli::cli_abort("La hoja {.val {sheet_diccionario}} no existe en el diccionario. Hojas disponibles: {paste(sheets, collapse = ', ')}.")
        }
      }
      dic <- cargar_diccionario(path_diccionario, sheet_diccionario)
      
    } else if (is.data.frame(diccionario)) {
      dic <- diccionario
      
    } else if (is.character(diccionario) && length(diccionario) == 1L) {
      if (!file.exists(diccionario)) {
        cli::cli_abort("No se encontró el diccionario en {.path {diccionario}}.")
      }
      dic <- cargar_diccionario(diccionario, sheet_diccionario)
      
    } else {
      cli::cli_abort("`diccionario` debe ser NULL, un data.frame o una ruta a archivo (.xlsx/.xls/.csv).")
    }
  }
  
  # -------- Parámetros comunes --------
  idiomas <- if (idioma == "both") c("es","en") else idioma
  # etiqueta 'modulo' (si el diccionario la usa)
  modulo_etiqueta <- c(pre = "pretest", long = "seguimiento", post = "postest")
  
  get_url <- function(idm, mod) {
    x <- tryCatch(urls[[idm]][[mod]], error = function(e) NULL)
    if (is.null(x) || !nzchar(x)) return(NULL)
    x
  }
  
  resumen_skip <- function(idm, mod) {
    list(
      cols_no_renombradas = character(0),
      vars_no_encontradas = character(0),
      resumen = sprintf("Renombrado desactivado (renombrar = FALSE) para %s/%s.", idm, mod)
    )
  }
  
  # -------- Lector por (idioma, módulo) --------
  leer_unit <- function(idm, mod) {
    u <- get_url(idm, mod)
    if (is.null(u)) {
      cli::cli_inform("Omitiendo {idm}/{mod}: URL no proporcionada.")
      return(NULL)
    }
    
    df <- googlesheets4::read_sheet(u, sheet = hoja)
    
    if (isTRUE(add_idioma)) df$idioma <- idm
    
    # Renombrado de columnas
    if (!isTRUE(renombrar)) {
      out <- c(list(datos = df), resumen_skip(idm, mod))
    } else {
      out <- dic_renombrar_cols(
        datos       = df,
        diccionario = dic,
        idioma      = idm,
        modulo      = unname(modulo_etiqueta[[mod]]),
        verbose     = FALSE
      )
    }
    
    # Asignación de tipos
    if (isTRUE(asignar_tipos)) {
      out$datos <- dic_asignar_tipos(
        datos       = out$datos,
        diccionario = dic,
        idioma      = idm,
        modulo      = unname(modulo_etiqueta[[mod]])
      )
    }
    
    out
  }
  
  solo_resumen <- function(x) x[c("cols_no_renombradas", "vars_no_encontradas", "resumen")]
  
  # -------- Ejecutar por módulo --------
  build_modulo <- function(mod) {
    res_list <- stats::setNames(lapply(idiomas, leer_unit, mod = mod), idiomas)
    res_list <- Filter(Negate(is.null), res_list)
    if (!length(res_list)) {
      cli::cli_inform("No se cargaron datos para el módulo {.val {mod}}.")
      return(NULL)
    }
    
    # Unir datos por idioma
    df <- dplyr::bind_rows(lapply(res_list, `[[`, "datos"))
    
    if (isTRUE(renombrar_verbose) && isTRUE(renombrar)) {
      cat(paste(vapply(res_list, function(z) z$resumen, ""), collapse = "\n\n"), "\n\n")
    }
    
    # Validación (después de renombrar / asignar tipos)
    if (isTRUE(validar)) {
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
      cli::cli_abort("No se pudo cargar el módulo {.val {modulos}} (no había ninguna URL válida).")
    }
    return(mod_res)
  }
  
  # Varios módulos
  out <- lapply(modulos, build_modulo)
  names(out) <- modulos
  out <- Filter(Negate(is.null), out) # quitar módulos vacíos
  
  if (!length(out)) {
    cli::cli_abort("No se pudo cargar ningún módulo (revisa URLs/idiomas).")
  }
  
  out
}
