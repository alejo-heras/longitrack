#' Renombrar columnas usando un diccionario
#'
#' Dado un `data.frame` y un diccionario con columnas `variable` y `etiqueta`,
#' renombra columnas cuyos nombres coincidan con `etiqueta`. Opcionalmente filtra
#' el diccionario por `idioma` y/o `modulo` si esas columnas existen.
#'
#' @param datos Data frame con las columnas a renombrar.
#' @param diccionario Data frame con, al menos, columnas `variable` y `etiqueta`.
#'        Opcionalmente `idioma` y/o `modulo`.
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
#' @examples
#' df <- data.frame("Identificador" = 1:2, "Edad (años)" = c(30, 40))
#' dic <- data.frame(
#'   variable = c("id", "edad"),
#'   etiqueta = c("Identificador", "Edad (años)")
#' )
#' out <- renombrar_con_diccionario(df, dic)
#' names(out$datos)
#' @export
renombrar_con_diccionario <- function(datos, diccionario, idioma = NULL, modulo = NULL, verbose = FALSE) {
  stopifnot(is.data.frame(datos), is.data.frame(diccionario))
  
  dic <- diccionario
  
  # # Filtrado opcional si las columnas existen
  # if (!is.null(idioma) && "idioma" %in% names(dic))  dic <- dic[dic$idioma  == idioma, , drop = FALSE]
  # if (!is.null(modulo) && "modulo" %in% names(dic))  dic <- dic[dic$modulo  == modulo, , drop = FALSE]
  
  # Filtrado opcional robusto
  norm <- function(x) tolower(stringi::stri_trans_general(trimws(as.character(x)), "Latin-ASCII"))
  
  if (!is.null(idioma) && "idioma" %in% names(dic)) {
    dic <- dic[norm(dic$idioma) == norm(idioma), , drop = FALSE]
  }
  if (!is.null(modulo) && "modulo" %in% names(dic)) {
    dic <- dic[norm(dic$modulo) == norm(modulo), , drop = FALSE]
  }
  
  if (nrow(dic) == 0L) {
    stop("Tras filtrar por idioma/modulo el diccionario qued\u00F3 vac\u00EDo. Revisa valores (tildes/espacios/may\u00FAsculas).", call. = FALSE)
  }
  
  
  # Verificaciones mínimas
  stopifnot(all(c("variable", "etiqueta") %in% names(dic)))
  
  # Función para limpiar texto (quita saltos, dobles comillas, etc.)
  # clean_label <- function(x) {
  #   x %>%
  #     stringr::str_squish() %>%                       # 1) squish
  #     stringr::str_replace_all('"', '') %>%           # 2) quitar comillas
  #     stringr::str_replace_all('[\r\n]+', ' ') %>%    # 3) saltos -> espacio
  #     {x <- .; x <- gsub("/", "-", x, fixed = TRUE);  # 4) normaliza / -> -
  #     gsub("dd\\s*/\\s*mm\\s*/\\s*aaaa", "dd-mm-aaaa", x, ignore.case = TRUE)}
  # }
  clean_label <- function(x) {
    x <- as.character(x)
    x <- stringr::str_squish(x)                         # 1) squish
    x <- stringr::str_replace_all(x, '"', '')           # 2) quitar comillas
    x <- stringr::str_replace_all(x, "[\r\n]+", " ")    # 3) saltos -> espacio
    x <- gsub("/", "-", x, fixed = TRUE)                # 4) normaliza / -> -
    x <- gsub("dd\\s*/\\s*mm\\s*/\\s*aaaa", "dd-mm-aaaa", x, ignore.case = TRUE)
    x
  }
  
  
  # Limpiar nombres y etiquetas
  nombres_actuales <- clean_label(names(datos))
  etiquetas_diccionario <- clean_label(trimws(dic$etiqueta))
  
  # Índices de match 1:1 por posición en diccionario
  indices <- match(nombres_actuales, etiquetas_diccionario)
  
  # Nuevos nombres: si hay match, usar dic$variable; si no, dejar nombre original
  nuevo_nombre <- ifelse(!is.na(indices), dic$variable[indices], nombres_actuales)
  names(datos) <- nuevo_nombre
  
  no_renombradas <- nombres_actuales[is.na(indices)]
  no_utilizadas  <- setdiff(etiquetas_diccionario, nombres_actuales)
  
  resumen_lines <- c(
    "Resumen:",
    sprintf(" - %d de %d columnas renombradas",
            length(nombres_actuales) - length(no_renombradas),
            length(nombres_actuales)),
    sprintf(" - %d variables del diccionario no utilizadas", length(no_utilizadas))
  )
  
  if (length(no_renombradas) > 0) {
    resumen_lines <- c(resumen_lines, "Columnas sin coincidencia en el diccionario:")
    for (col in no_renombradas) {
      idx <- which(nombres_actuales == col)
      etiqueta <- if (length(idx) == 1) sprintf(" - Columna %d: '%s'", idx, col) else sprintf(" - '%s'", col)
      resumen_lines <- c(resumen_lines, etiqueta)
    }
  }
  
  if (length(no_utilizadas) > 0) {
    resumen_lines <- c(resumen_lines, "Variables del diccionario no encontradas en los datos:")
    for (etq in no_utilizadas) {
      filas <- dic[dic$etiqueta == etq, , drop = FALSE]
      for (i in seq_len(nrow(filas))) {
        resumen_lines <- c(resumen_lines, paste0(" - ", filas$variable[i], ": ", filas$etiqueta[i]))
      }
    }
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
