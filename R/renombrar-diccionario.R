#' Renombrar (y castear) columnas usando un diccionario
#'
#' Dado un `data.frame` y un diccionario con columnas mínimas `variable` y
#' `etiqueta`, renombra columnas cuyos nombres coinciden con `etiqueta`.
#' Opcionalmente filtra por `idioma` y/o `modulo` si existen esas columnas.
#'
#' Si el diccionario incluye `tipo`, convierte la clase de las columnas renombradas:
#' tipos canónicos (se admiten sinónimos entre paréntesis):
#' - "character" (char, texto, string)
#' - "numeric"   (double, numero)
#' - "integer"   (entero)
#' - "logical"   (bool, logico, boolean)
#' - "date"      (fecha)       -> usa `lubridate::as_date()`
#' - "datetime"  (fechahora, posixct)
#' - "factor"
#'
#' Campos opcionales del diccionario:
#' - `formato`: cadena de formato para parseo de fecha/fecha-hora (p.ej. "%d/%m/%Y").
#'   Para `date` con `formato` se usa `as.Date(x, format=...)` y luego `as_date`.
#'   Sin `formato`, se intentan órdenes comunes con `parse_date_time`.
#' - `niveles`: para `factor`. Acepta:
#'   - "A,B,C" o "A|B|C"  (fija niveles y su orden)
#'   - "1=No,2=Sí" (o con "|")  (niveles = 1,2 ; etiquetas = No,Sí)
#'
#' @param datos Data frame origen.
#' @param diccionario Data frame diccionario. Requiere `variable`, `etiqueta`.
#'        Puede incluir `idioma`, `modulo`, `tipo`, `formato`, `niveles`.
#' @param idioma (opcional) filtro para `diccionario$idioma`.
#' @param modulo (opcional) filtro para `diccionario$modulo`.
#' @param verbose si TRUE imprime un resumen.
#'
#' @return Lista con:
#' \itemize{
#'   \item `datos`: data frame renombrado (y casteado si procede)
#'   \item `cols_no_renombradas`: nombres originales sin match en el diccionario
#'   \item `vars_no_encontradas`: etiquetas del diccionario no presentes en `datos`
#'   \item `resumen`: texto (incluye conversiones de tipo)
#' }
#'
#' @details
#' Matching etiqueta/nombre: se limpian espacios múltiples, comillas y saltos de línea.
#' Conversión: si una conversión introduce nuevos `NA`, se emite `warning()`.
#' Zona horaria para `datetime`: "UTC".
#'
#' @examples
#' df <- data.frame(
#'   "Identificador" = c("007","042"),
#'   "Edad (años)"   = c("30","40"),
#'   "Fecha"         = c("01/08/2025","22/08/2025"),
#'   "¿Válido?"      = c("sí","no"),
#'   "Grupo"         = c("B","A"),
#'   check.names = FALSE
#' )
#' dic <- data.frame(
#'   variable = c("id","edad","fecha","ok","grupo"),
#'   etiqueta = c("Identificador","Edad (años)","Fecha","¿Válido?","Grupo"),
#'   idioma   = "es",
#'   modulo   = "caracterizacion",
#'   tipo     = c("character","integer","date","logical","factor"),
#'   formato  = c(NA, NA, "%d/%m/%Y", NA, NA),
#'   niveles  = c(NA, NA, NA, NA, "A,B"),
#'   stringsAsFactors = FALSE
#' )
#' out <- renombrar_con_diccionario(df, dic, idioma="es", modulo="caracterizacion", verbose=TRUE)
#' str(out$datos)
#'
#' @export
renombrar_con_diccionario <- function(datos, diccionario, idioma = NULL, modulo = NULL, verbose = FALSE) {
  stopifnot(is.data.frame(datos), is.data.frame(diccionario))
  dic <- diccionario
  
  # Filtrado opcional
  if (!is.null(idioma) && "idioma" %in% names(dic)) dic <- dic[dic$idioma == idioma, , drop = FALSE]
  if (!is.null(modulo) && "modulo" %in% names(dic)) dic <- dic[dic$modulo == modulo, , drop = FALSE]
  
  # Verificaciones mínimas
  if (!all(c("variable","etiqueta") %in% names(dic))) {
    stop("El diccionario debe contener al menos 'variable' y 'etiqueta'.", call. = FALSE)
  }
  if (anyDuplicated(dic$etiqueta)) warning("Diccionario: etiquetas duplicadas detectadas.")
  if (anyDuplicated(dic$variable)) warning("Diccionario: variables destino duplicadas detectadas.")
  
  # Limpieza ligera (base R)
  clean_label <- function(x) {
    x <- as.character(x)
    x <- gsub("[\r\n]+"," ", x)
    x <- gsub('"',"", x)
    x <- gsub("\\s+"," ", x)
    trimws(x)
  }
  
  nombres_actuales      <- clean_label(names(datos))
  etiquetas_diccionario <- clean_label(dic$etiqueta)
  indices               <- match(nombres_actuales, etiquetas_diccionario)
  
  # Renombrado
  nuevo_nombre <- ifelse(!is.na(indices), as.character(dic$variable[indices]), nombres_actuales)
  names(datos) <- nuevo_nombre
  
  cast_logs <- character(0)
  
  # ---- Casteo según 'tipo' ----
  if ("tipo" %in% names(dic)) {
    
    canonizar_tipo <- function(x) {
      x <- tolower(trimws(as.character(x)))
      x <- ifelse(x %in% c("char","texto","string"), "character", x)
      x <- ifelse(x %in% c("double","numero"), "numeric", x)
      x <- ifelse(x %in% c("entero"), "integer", x)
      x <- ifelse(x %in% c("bool","logico","boolean"), "logical", x)
      x <- ifelse(x %in% c("fecha"), "date", x)
      x <- ifelse(x %in% c("fechahora","posixct"), "datetime", x)
      x
    }
    
    tipo_por_var <- as.character(dic$tipo); names(tipo_por_var) <- as.character(dic$variable)
    formato_por_var <- if ("formato" %in% names(dic)) { f <- as.character(dic$formato); names(f) <- as.character(dic$variable); f } else NULL
    niveles_por_var <- if ("niveles" %in% names(dic)) { n <- as.character(dic$niveles); names(n) <- as.character(dic$variable); n } else NULL
    
    # Helpers
    warn_new_NA <- function(before, after, col) {
      n <- sum(is.na(after)) - sum(is.na(before))
      if (n > 0) warning(sprintf("%s: %d valores no convertibles -> NA", col, n), call. = FALSE)
    }
    parse_date_multi <- function(v, fmt = NULL) {
      if (!is.null(fmt) && nzchar(fmt)) {
        # strptime format -> base parse, luego as_date
        out <- suppressWarnings(as.Date(as.character(v), format = fmt))
        return(lubridate::as_date(out))
      }
      dt <- suppressWarnings(lubridate::parse_date_time(as.character(v),
                                                        orders = c("Y-m-d","d/m/Y","m/d/Y"),
                                                        tz = "UTC"))
      lubridate::as_date(dt)
    }
    parse_dt_multi <- function(v, fmt = NULL) {
      if (!is.null(fmt) && nzchar(fmt)) {
        return(suppressWarnings(as.POSIXct(as.character(v), format = fmt, tz = "UTC")))
      }
      suppressWarnings(lubridate::parse_date_time(as.character(v),
                                                  orders = c("Y-m-d H:M:S","Y-m-dTH:M:S","d/m/Y H:M:S"),
                                                  tz = "UTC"))
    }
    parse_levels <- function(spec) {
      if (is.null(spec) || !nzchar(spec)) return(NULL)
      parts <- unlist(strsplit(spec, "[,|]"))
      parts <- trimws(parts)
      if (any(grepl("=", parts, fixed = TRUE))) {
        kv <- strsplit(parts, "=", fixed = TRUE)
        keys <- vapply(kv, function(p) trimws(p[1]), character(1))
        vals <- vapply(kv, function(p) trimws(p[2]), character(1))
        list(levels = keys, labels = vals)
      } else {
        list(levels = parts, labels = NULL)
      }
    }
    
    comunes <- intersect(names(datos), names(tipo_por_var))
    for (nm in comunes) {
      tipo <- canonizar_tipo(tipo_por_var[[nm]])
      old  <- datos[[nm]]
      oldc <- class(old)[1]
      
      new <- switch(
        tipo,
        "character" = as.character(old),
        "numeric"   = suppressWarnings(as.numeric(old)),
        "integer"   = suppressWarnings(as.integer(old)),
        "logical"   = {
          x <- tolower(trimws(as.character(old)))
          true_vals  <- c("true","t","1","sí","si","yes","y")
          false_vals <- c("false","f","0","no","n")
          res <- ifelse(x %in% true_vals, TRUE, ifelse(x %in% false_vals, FALSE, NA))
          as.logical(res)
        },
        "date"      = parse_date_multi(old, if (!is.null(formato_por_var)) formato_por_var[[nm]] else NULL),
        "datetime"  = parse_dt_multi(old, if (!is.null(formato_por_var)) formato_por_var[[nm]] else NULL),
        "factor"    = {
          spec <- if (!is.null(niveles_por_var)) niveles_por_var[[nm]] else NULL
          lv <- parse_levels(spec)
          if (is.null(lv)) {
            factor(old)
          } else if (is.null(lv$labels)) {
            factor(old, levels = lv$levels)
          } else {
            factor(old, levels = lv$levels, labels = lv$labels)
          }
        },
        old
      )
      
      # Aviso por nuevos NA
      warn_new_NA(old, new, nm)
      
      datos[[nm]] <- new
      newc <- class(datos[[nm]])[1]
      if (!identical(oldc, newc)) cast_logs <- c(cast_logs, sprintf(" - %s: %s -> %s", nm, oldc, newc))
    }
  }
  
  # Resúmenes
  no_renombradas <- nombres_actuales[is.na(indices)]
  no_utilizadas  <- setdiff(etiquetas_diccionario, nombres_actuales)
  
  resumen_lines <- c(
    "Resumen:",
    sprintf(" - %d de %d columnas renombradas",
            length(nombres_actuales) - length(no_renombradas),
            length(nombres_actuales)),
    sprintf(" - %d variables del diccionario no utilizadas", length(no_utilizadas))
  )
  if (length(cast_logs)) {
    resumen_lines <- c(resumen_lines, "Conversiones de tipo:", cast_logs)
  }
  if (length(no_renombradas)) {
    resumen_lines <- c(resumen_lines, "Columnas sin coincidencia en el diccionario:",
                       paste0(" - '", no_renombradas, "'"))
  }
  if (length(no_utilizadas)) {
    resumen_lines <- c(resumen_lines, "Variables del diccionario no encontradas en los datos:",
                       paste0(" - ", no_utilizadas))
  }
  
  resumen_texto <- paste(resumen_lines, collapse = "\n")
  if (isTRUE(verbose)) cat(resumen_texto, "\n")
  
  list(
    datos = datos,
    cols_no_renombradas = no_renombradas,
    vars_no_encontradas = no_utilizadas,
    resumen = resumen_texto
  )
}
