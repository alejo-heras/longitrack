
# ================================================================
# emails-corregir.R 
# ------------------------------------------------
# Objetivo: corrección de emails una vez normalizados.
# Diferencia con normalizar-emails.R:
#   * aquí se aplican reglas determinísticas (diccionario) y heurísticas (Levenshtein).
#   * trabaja sobre data.frames completos (pre, long, post).
# ================================================================
# ---- 1) diff_email_google() ------------------------------------
# entrada: df con columnas email, email_google
# pasos:
#   - comparar email y email_google (ignorando mayúsculas y espacios)
#   - seleccionar solo filas con diferencias
#   - devolver solo: name_norm, date, email, email_google


# ---- 2) correct_domains_from_excel() ---------------------------
# entrada: df con columna email
# pasos:
#   - leer Excel con columnas: erroneo, correcto
#   - convertir a diccionario (erroneo → correcto)
#   - extraer dominio de cada email
#   - sustituir si coincide con diccionario
#   - reconstruir email con parte_local + dominio_corregido
#   - devolver df con email corregido


# ---- 3) suggest_domains_lv() -----------------------------------
# entrada: df con emails y vector de dominios válidos
# pasos:
#   - extraer dominio observado
#   - contar frecuencia de cada dominio
#   - calcular distancia Levenshtein a todos los válidos
#   - elegir sugerencia más cercana + distancia mínima
#   - marcar sospechoso si distancia <= max_dist y no está en lista blanca
# salida: tibble con dominio, n, sugerencia, distancia, sospechoso


# ---- 4) review_domains_lv() ------------------------------------
# entrada: lista de data.frames (ej. pre, long, post)
# pasos:
#   - aplicar suggest_domains_lv() a cada dataset
#   - extraer además subconjunto sospechoso (filter sospechoso == TRUE)
# salida: lista con dos niveles:
#   - suggestions: tablas completas por dataset
#   - suspects:   solo sospechosos por dataset


# ---- NOTAS ------------------------------------------------------
# * Este script asume que antes ya se aplicó normalización (lowercase, acentos, etc).
# * El Excel de diccionario captura errores frecuentes conocidos.
# * Levenshtein detecta errores nuevos o no cubiertos por el diccionario.
# * Se recomienda revisar manualmente los "sospechosos".






#' Diferencias entre `email` y `email_google`
#'
#' Compara las columnas \code{email} (campo principal que se normaliza y corrige)
#' y \code{email_google} (campo capturado por Google Forms) ignorando
#' mayúsculas y espacios en blanco. Devuelve solo las filas donde ambos
#' valores difieren, junto con variables mínimas de contexto.
#'
#' @param df data.frame con columnas:
#'   \itemize{
#'     \item \code{name_norm}: nombre normalizado del participante.
#'     \item \code{date}: fecha asociada al registro.
#'     \item \code{email}: dirección de email principal (a normalizar/corregir).
#'     \item \code{email_google}: dirección original capturada por Google Forms.
#'   }
#'
#' @return Un tibble con las filas donde \code{email} y \code{email_google}
#'   no coinciden (ignorando mayúsculas y espacios), y solo las columnas:
#'   \code{name_norm}, \code{date}, \code{email}, \code{email_google}.
#'
#' @details
#' Esta función sirve como paso de \strong{auditoría}, para identificar
#' participantes que escribieron su email de forma distinta al inicio y al
#' final del formulario. Es útil para revisar discrepancias manualmente antes
#' de aplicar correcciones sistemáticas con
#' \code{\link{correct_domains_from_excel}} o revisiones por
#' \code{\link{review_domains_lv}}.
#'
#' @seealso
#'   \code{\link{normalizar_email}},
#'   \code{\link{emails_check}},
#'   \code{\link{correct_domains_from_excel}},
#'   \code{\link{review_domains_lv}}
#' @family utilidades-email
#'
#' @examples
#' library(tibble)
#'
#' pre <- tibble(
#'   name_norm = c("ana", "luis"),
#'   date = as.Date(c("2025-08-01","2025-08-02")),
#'   email = c("ana@gmai.com", "luis@HOTMAIL,com"),
#'   email_google = c("ana@gmail.com", "luis@hotmail.com")
#' )
#'
#' # Detectar discrepancias entre email y email_google
#' diff_email_google(pre)
#'
#' @export
#' @importFrom dplyr filter select
#' @importFrom stringr str_trim
diff_email_google <- function(df) {
  .lc <- function(x) tolower(stringr::str_trim(x))
  dplyr::filter(df, .lc(email) != .lc(email_google)) |>
    dplyr::select(name_norm, date, email, email_google)
}



#' Corrige dominios de email usando un diccionario en Excel
#'
#' Sustituye dominios escritos con errores comunes (ej. \code{"gmai.com"},
#' \code{"hotmail,com"}) por sus equivalentes correctos, tomando las reglas
#' de un archivo Excel externo. Esto permite mantener y actualizar un
#' diccionario de correcciones de forma editable sin tocar el código del
#' paquete.
#' 
#' Puede aplicarse a una o varias columnas (p. ej. `email` y `email_google`).
#'
#' @param df data.frame con columna \code{email}.
#'   
#' @param path Ruta al Excel con el diccionario **o** un data.frame/tibble ya cargado
#'   con dos columnas: `erroneo` y `correcto`. Por defecto usa
#'   "diccionario_emails.xlsx" en la raíz del proyecto.
#'   
#' @param vars Vector de nombres de columnas a corregir (por defecto `"email"`).
#'
#' @return El mismo \code{data.frame} con la columna \code{email} corregida
#'   según el diccionario.
#'
#' @details
#' El Excel debe tener exactamente dos columnas:
#' \itemize{
#'   \item \code{erroneo}: dominio mal escrito.
#'   \item \code{correcto}: dominio corregido.
#' }
#'
#' El flujo recomendado es:
#' \enumerate{
#'   \item Normalizar emails con \code{\link{normalizar_email}}.
#'   \item Aplicar \code{correct_domains_from_excel()} para errores conocidos.
#'   \item Revisar errores nuevos o raros con
#'         \code{\link{review_domains_lv}} (Levenshtein).
#' }
#'
#' @seealso
#'   \code{\link{normalizar_email}},
#'   \code{\link{emails_check}},
#'   \code{\link{review_domains_lv}}
#' @family utilidades-email
#'
#' @examples
#' # --- Ejemplo de diccionario en Excel ---
#' # erroneo   | correcto
#' # gmai.com  | gmail.com
#' # hotmail,com | hotmail.com
#'
#' library(tibble)
#' pre <- tibble(
#'   email = c("ana@gmai.com", "luis@HOTMAIL,com", "mario@outlook.com")
#' )
#'
#' # Normalizar primero
#' pre$email <- normalizar_email(pre$email)
#'
#' # Aplicar correcciones desde Excel
#' # pre <- correct_domains_from_excel(pre, "dev/diccionario_emails.xlsx")
#'
#' # Resultado esperado: dominios corregidos según el diccionario
#'
#' @export
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate
#' @importFrom stringr str_extract
correct_domains_from_excel <- function(df,
                                       path = "diccionario_emails.xlsx",
                                       vars = "email") {
  # Acepta ruta a Excel O un tibble/data.frame directamente
  if (is.character(path) && length(path) == 1) {
    if (!file.exists(path)) {
      stop("No se encuentra el archivo de diccionario en: ", path)
    }
    dicc <- readxl::read_excel(path)
  } else if (is.data.frame(path)) {
    dicc <- path
  } else {
    stop("`path` debe ser una ruta a .xlsx o un data.frame/tibble con columnas 'erroneo' y 'correcto'.")
  }
  
  if (!all(c("erroneo", "correcto") %in% names(dicc))) {
    stop("El Excel debe tener columnas: 'erroneo' y 'correcto'")
  }
  
  # normalizar el diccionario para evitar misses por mayúsculas/espacios
  dicc$erroneo  <- trimws(tolower(as.character(dicc$erroneo)))
  dicc$correcto <- trimws(tolower(as.character(dicc$correcto)))
  dict <- stats::setNames(dicc$correcto, dicc$erroneo)
  
  # helper local
  .email_domain  <- function(x) sub("^.*@", "", x)
  .email_local   <- function(x) sub("@.*$", "", x)
  .email_rebuild <- function(local, dom) paste0(local, "@", dom)
  
  cols <- intersect(vars, names(df))
  if (length(cols) == 0) return(df)  # nada que hacer
  
  for (v in cols) {
    x <- as.character(df[[v]])
    # no tocamos NAs
    ok <- !is.na(x) & grepl("@", x, fixed = TRUE)
    if (!any(ok)) next
    
    dom       <- .email_domain(x[ok])
    local     <- .email_local(x[ok])
    dom_corr  <- ifelse(dom %in% names(dict), unname(dict[dom]), dom)
    x[ok]     <- .email_rebuild(local, dom_corr)
    
    df[[v]] <- x
  }
  
  df
}



#' Sugerencias de corrección por distancia de Levenshtein
#'
#' Calcula, para cada \emph{dominio observado} en un data.frame, la sugerencia
#' de dominio más cercana dentro de una \emph{lista blanca} usando distancia
#' de Levenshtein. Marca como \code{sospechoso} a todo dominio cuya distancia
#' mínima sea \code{<= max_dist} y que no esté ya en \code{valid_domains}.
#'
#' @param df data.frame con una columna de emails indicada en \code{var}.
#'   Suele ser \code{"email"} (campo operativo) o \code{"email_google"}
#'   (campo crudo del formulario).
#' @param valid_domains \code{character}. Lista blanca de dominios válidos
#'   (p.ej. \code{c("gmail.com","hotmail.com","outlook.com", ...)}).
#' @param max_dist \code{integer}. Umbral de distancia Levenshtein para marcar
#'   un dominio como \code{sospechoso}. Por defecto, \code{2}.
#' @param var \code{character}. Nombre de la columna donde buscar dominios:
#'   \code{"email"} o \code{"email_google"}. Por defecto, \code{"email"}.
#'
#' @return Un \strong{tibble} con una fila por dominio observado y columnas:
#' \itemize{
#'   \item \code{dominio}: dominio observado en \code{df[[var]]}.
#'   \item \code{n}: frecuencia de aparición de ese dominio.
#'   \item \code{sugerencia}: dominio en \code{valid_domains} con menor distancia.
#'   \item \code{distancia}: distancia Levenshtein mínima encontrada.
#'   \item \code{sospechoso}: \code{TRUE} si \code{distancia <= max_dist} y
#'         \code{dominio} \emph{no} está en \code{valid_domains}.
#' }
#'
#' @details
#' Uso recomendado en el flujo:
#' \enumerate{
#'   \item Normalizar emails con \code{\link{normalizar_email}}.
#'   \item Corregir errores conocidos con \code{\link{correct_domains_from_excel}}.
#'   \item Ejecutar \code{suggest_domains_lv()} para detectar dominios restantes
#'         a revisar (\code{sospechoso == TRUE}).
#' }
#' Esta función no modifica datos; solo propone. Para revisar varios datasets
#' (pre/long/post) de una vez, use \code{\link{review_domains_lv}}.
#'
#' \strong{Notas}:
#' \itemize{
#'   \item Si no hay dominios (o \code{df} está vacío), devuelve un tibble vacío.
#'   \item Elegir \code{max_dist} conservador (p.ej., 1–2) reduce falsos positivos.
#' }
#'
#' @seealso
#'   \code{\link{review_domains_lv}},
#'   \code{\link{correct_domains_from_excel}},
#'   \code{\link{normalizar_email}}
#' @family utilidades-email
#'
#' @examples
#' # --- Working example (fragmento) ---
#' library(tibble)
#'
#' pre <- tibble(
#'   email = c("ana@gmai.com", "luis@hotmail.com", "mario@outlook.com", "eva@gmailcom"),
#'   email_google = c("ana@gmail.com", "luis@hotmail.com", "mario@outlook.com", "eva@gmail.com")
#' )
#'
#' validos <- c(
#'   "gmail.com","hotmail.com","outlook.com","yahoo.com","yahoo.es",
#'   "yahoo.com.ar","yahoo.com.mx","aol.com","live.de","me.com",
#'   "supramax.com.mx","telefonica.net"
#' )
#'
#' # Tras normalizar y aplicar diccionario, revisar con Levenshtein:
#' suggest_domains_lv(pre, valid_domains = validos, max_dist = 2, var = "email")
#'
#' # También se puede auditar el campo crudo del formulario:
#' suggest_domains_lv(pre, valid_domains = validos, max_dist = 2, var = "email_google")
#'
#' # Filtrar solo sospechosos (si se desea):
#' # dplyr::filter(suggest_domains_lv(pre, valid_domains = validos), sospechoso)
#'
#' @export
#' @importFrom dplyr count arrange desc
#' @importFrom tibble tibble
#' @importFrom stringdist stringdist
suggest_domains_lv <- function(df, valid_domains, max_dist = 2, var = "email") {
  stopifnot(var %in% names(df))
  
  # extraer dominios; ignora NA y cadenas vacías
  doms <- sub("^.*@", "", df[[var]])
  obs  <- dplyr::count(tibble::tibble(dominio = doms), dominio, sort = TRUE)
  obs  <- dplyr::filter(obs, !is.na(dominio), dominio != "")
  
  # tibble vacío con el esquema esperado
  empty <- tibble::tibble(
    dominio    = character(),
    n          = integer(),
    sugerencia = character(),
    distancia  = numeric(),
    sospechoso = logical()
  )
  if (nrow(obs) == 0) return(empty)
  
  best <- lapply(obs$dominio, function(d) {
    dists <- stringdist::stringdist(d, valid_domains, method = "lv")
    i <- which.min(dists)
    list(sugerencia = valid_domains[i], distancia = dists[i])
  })
  
  obs$sugerencia <- vapply(best, `[[`, character(1), "sugerencia")
  obs$distancia  <- vapply(best, `[[`, numeric(1),   "distancia")
  obs$sospechoso <- obs$distancia <= max_dist & !(obs$dominio %in% valid_domains)
  dplyr::arrange(obs, dplyr::desc(sospechoso), distancia, dplyr::desc(n))
}




#' Corrección por Levenshtein en varios datasets (pre/long/post)
#'
#' Aplica \code{\link{suggest_domains_lv}} en lote sobre una lista de
#' data.frames (por ejemplo \code{list(pre=pre, long=long, post=post)}),
#' y devuelve, para cada uno, la tabla completa de sugerencias y el
#' subconjunto de dominios \code{sospechosos}.
#'
#' @param datos Lista con data.frames. Cada elemento debe contener la columna
#'   indicada en \code{var} (p.ej., \code{"email"} o \code{"email_google"}).
#' @param valid_domains \code{character}. Lista blanca de dominios válidos.
#' @param max_dist \code{integer}. Umbral de distancia Levenshtein para marcar
#'   \code{sospechoso}. Por defecto, \code{2}.
#' @param var \code{character}. Columna objetivo en cada data.frame:
#'   \code{"email"} (recomendado) o \code{"email_google"}.
#'
#' @return Una \strong{lista} con dos elementos:
#' \itemize{
#'   \item \code{suggestions}: lista de tibbles (uno por dataset) con
#'         \code{dominio}, \code{n}, \code{sugerencia}, \code{distancia},
#'         \code{sospechoso}.
#'   \item \code{suspects}: lista de tibbles (uno por dataset) filtrados a
#'         \code{sospechoso == TRUE}.
#' }
#'
#' @details
#' Flujo recomendado:
#' \enumerate{
#'   \item Normalizar con \code{\link{normalizar_email}}.
#'   \item Corregir errores conocidos con \code{\link{correct_domains_from_excel}}.
#'   \item Ejecutar \code{review_domains_lv()} para detectar dominios restantes
#'         que requieren revisión (\code{suspects}).
#' }
#' Esta función no modifica los datos de entrada; sirve para \emph{auditar}
#' y priorizar correcciones. Para trabajar sobre un único data.frame, use
#' \code{\link{suggest_domains_lv}}.
#'
#' @seealso
#'   \code{\link{suggest_domains_lv}},
#'   \code{\link{normalizar_email}},
#'   \code{\link{correct_domains_from_excel}}
#' @family utilidades-email
#'
#' @examples
#' library(tibble)
#'
#' pre <- tibble(
#'   email = c("ana@gmai.com", "luis@hotmail.com", "mario@outlook.com", "eva@gmailcom")
#' )
#' long <- tibble(
#'   email = c("ana@gmail.com", "luis@hormail.com")
#' )
#' post <- tibble(
#'   email = c("eva@hotomail.com", "eva@hotmail.com")
#' )
#'
#' validos <- c(
#'   "gmail.com","hotmail.com","outlook.com","yahoo.com","yahoo.es",
#'   "yahoo.com.ar","yahoo.com.mx","aol.com","live.de","me.com",
#'   "supramax.com.mx","telefonica.net"
#' )
#'
#' # Tras normalizar y aplicar diccionario, revisar en lote con Levenshtein:
#' out <- review_domains_lv(
#'   datos = list(pre = pre, long = long, post = post),
#'   valid_domains = validos,
#'   max_dist = 2,
#'   var = "email"
#' )
#'
#' out$suggestions  # tablas completas por dataset
#' out$suspects     # solo sospechosos por dataset
#'
#' @export
#' @importFrom purrr map
#' @importFrom dplyr filter
review_domains_lv <- function(datos, valid_domains, max_dist = 2, var = "email") {
  sug <- purrr::map(datos, ~ suggest_domains_lv(.x, valid_domains, max_dist, var))
  sus <- purrr::map(sug, ~ dplyr::filter(.x, sospechoso))
  list(suggestions = sug, suspects = sus)
}

