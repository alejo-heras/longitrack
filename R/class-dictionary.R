#' Castear columnas según diccionario (carga automática + filtro idioma/módulo)
#'
#' Reglas:
#' - "date"      -> NO tocar
#' - "character" -> as.character()
#' - "numerical" -> as.numeric()  (acepta "numeric"/"double")
#' - "logical"   -> as.logical()
#' - "factor"    -> as.factor(); si 'niveles' existe:
#'      * "A,B" o "A|B|C" fija niveles
#'      * "1=No,2=Sí" mapea códigos a etiquetas
#'   Si 'niveles' no casa con los datos, cae a as.factor() y avisa.
#'
#' Además, si la conversión introduce nuevos NA, se emite un warning.
#'
#' @param datos data.frame con nombres ya iguales a diccionario$variable.
#' @param diccionario NULL (por defecto), un data.frame, o ruta (xlsx/csv).
#' @param path_diccionario Ruta por defecto cuando diccionario=NULL (def. "diccionario.xlsx").
#' @param sheet_diccionario Hoja si es Excel (def. 1).
#' @param idioma (opcional) filtra diccionario$idioma si existe.
#' @param modulo (opcional) filtra diccionario$modulo si existe.
#' @return data.frame con clases convertidas.
#' @export
class_dictionary <- function(datos,
                      diccionario = NULL,
                      path_diccionario = "diccionario.xlsx",
                      sheet_diccionario = 1,
                      idioma = NULL,
                      modulo = NULL) {
  
  stopifnot(is.data.frame(datos))
  
  # --- Cargar diccionario si hace falta ---
  load_dic <- function(path, sheet) {
    ext <- tolower(tools::file_ext(path))
    if (ext %in% c("xlsx","xls")) readxl::read_excel(path, sheet = sheet)
    else if (ext == "csv") utils::read.csv(path, stringsAsFactors = FALSE)
    else stop("Extensión del diccionario no soportada: ", ext, call. = FALSE)
  }
  
  dic <- if (is.null(diccionario)) {
    if (!is.character(path_diccionario) || length(path_diccionario) != 1L)
      stop("Argumento 'path_diccionario' inválido.", call. = FALSE)
    if (!file.exists(path_diccionario))
      stop("No se encontró el diccionario en: ", path_diccionario, call. = FALSE)
    load_dic(path_diccionario, sheet_diccionario)
  } else if (is.character(diccionario) && length(diccionario) == 1L) {
    if (!file.exists(diccionario))
      stop("No se encontró el diccionario en: ", diccionario, call. = FALSE)
    load_dic(diccionario, sheet_diccionario)
  } else if (is.data.frame(diccionario)) {
    diccionario
  } else stop("'diccionario' debe ser NULL, ruta o data.frame.", call. = FALSE)
  
  # --- Validaciones mínimas ---
  req <- c("variable","tipo")
  if (!all(req %in% names(dic))) stop("El diccionario debe tener 'variable' y 'tipo'.", call. = FALSE)
  
  # --- Filtro por idioma/módulo si existen esas columnas ---
  if (!is.null(idioma) && "idioma" %in% names(dic))  dic <- dic[dic$idioma == idioma, , drop = FALSE]
  if (!is.null(modulo) && "modulo" %in% names(dic))  dic <- dic[dic$modulo == modulo, , drop = FALSE]
  
  # Filas válidas
  dic <- dic[!is.na(dic$variable) & nzchar(as.character(dic$variable)), , drop = FALSE]
  
  # Vectores nombrados
  tipos <- tolower(trimws(as.character(dic$tipo)))
  names(tipos) <- trimws(as.character(dic$variable))
  
  if ("niveles" %in% names(dic)) {
    niveles_por_var <- as.character(dic$niveles)
    names(niveles_por_var) <- trimws(as.character(dic$variable))
  } else {
    niveles_por_var <- NULL
  }
  
  # ---- Helpers ----
  norm <- function(x) {
    x <- stringr::str_squish(as.character(x))
    x <- tolower(x)
    # quita diacríticos si es posible (sin depender de stringi)
    tryCatch(iconv(x, from = "", to = "ASCII//TRANSLIT"), error = function(e) x)
  }
  
  parse_levels_labels <- function(spec) {
    if (is.null(spec)) return(NULL)
    spec <- stringr::str_squish(as.character(spec))
    if (!nzchar(spec) || is.na(spec)) return(NULL)   # <- vacío o NA => sin especificación
    parts <- trimws(unlist(strsplit(spec, "[,|]")))
    if (!length(parts)) return(NULL)
    if (any(grepl("=", parts, fixed = TRUE))) {
      kv  <- strsplit(parts, "=", fixed = TRUE)
      lev <- vapply(kv, function(p) trimws(p[1]), character(1))
      lab <- vapply(kv, function(p) trimws(p[2]), character(1))
      list(levels = lev, labels = lab)
    } else list(levels = parts, labels = NULL)
  }
  
  cast_factor <- function(old, spec, nm) {
    old_chr <- stringr::str_squish(as.character(old))
    lvls <- parse_levels_labels(spec)
    if (is.null(lvls)) return(as.factor(old_chr))
  
    n_old    <- norm(old_chr)
    n_levels <- norm(lvls$levels)
    n_labels <- if (!is.null(lvls$labels)) norm(lvls$labels) else NULL
    
    in_levels <- n_old %in% n_levels
    in_labels <- if (is.null(n_labels)) rep(FALSE, length(n_old)) else n_old %in% n_labels
    
    # Si no cubre todos los valores -> fallback seguro
    if (!all(in_levels | in_labels | is.na(old_chr))) {
      warning(sprintf("%s: 'niveles' no coincide con los datos; se usa as.factor() sin niveles", nm), call. = FALSE)
      return(as.factor(old_chr))
    }
    
    # Preferir la vía que mejor explica los datos (más aciertos)
    sum_levels <- sum(in_levels, na.rm = TRUE)
    sum_labels <- sum(in_labels, na.rm = TRUE)
    
    if (!is.null(lvls$labels) && sum_labels >= sum_levels) {
      # Datos parecen etiquetas: canoniza a labels exactas
      idx   <- match(n_old, n_labels)
      canon <- ifelse(is.na(idx), old_chr, lvls$labels[idx])
      return(factor(canon, levels = lvls$labels))
    }
    
    if (!is.null(lvls$labels)) {
      # Datos parecen códigos: mapear a labels
      idx    <- match(n_old, n_levels)
      mapped <- ifelse(is.na(idx), NA_character_, lvls$labels[idx])
      return(factor(mapped, levels = lvls$labels))
    }
    
    # Solo levels (sin labels). Si hay valores fuera, ya se hizo fallback arriba.
    idx   <- match(n_old, n_levels)
    canon <- ifelse(is.na(idx), old_chr, lvls$levels[idx])
    factor(canon, levels = lvls$levels)
  }
  
  warn_new_NA <- function(before, after, nm) {
    inc <- sum(is.na(after)) - sum(is.na(before))
    if (inc > 0) warning(sprintf("%s: %d valores no convertibles -> NA", nm, inc), call. = FALSE)
  }
  
  # ---- Casteo ----
  comunes <- intersect(names(datos), names(tipos))
  for (nm in comunes) {
    t <- tipos[[nm]]
    old <- datos[[nm]]
    
    # sinónimos de numerical
    t <- switch(t, "numeric" = "numerical", "double" = "numerical", t)
    
    new <- switch(t,
                  "date"      = old,                                   # NO tocar
                  "character" = as.character(old),
                  "numerical" = suppressWarnings(as.numeric(old)),
                  "logical"   = as.logical(old),
                  "factor"    = cast_factor(old,
                                            if (!is.null(niveles_por_var)) niveles_por_var[[nm]] else NULL,
                                            nm),
                  old
    )
    
    warn_new_NA(old, new, nm)
    datos[[nm]] <- new
  }
  
  datos
}
