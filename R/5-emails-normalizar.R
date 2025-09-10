# ================================================================
# normalizar-emails.R — PSEUDOCÓDIGO (mínimo y claro)
# ------------------------------------------------
# Objetivo: limpieza básica y diagnóstico de emails.
# Diferencia con corregir-emails.R:
#   * aquí SOLO se normaliza normalizar_email(), quitando acentos y espacios
#   * y se reportan problemas (no se corrige), con emails_check ().
# ---- NOTAS ------------------------------------------------------
# * Este script NO aplica correcciones de dominio.
# * Úsalo antes de joins/matches para estandarizar y auditar.
# * Para corregir dominios (diccionario / Levenshtein) usar corregir-emails.R.
# ================================================================


#' Quita tildes/acentos y normaliza caracteres a ASCII
#'
#' Translitera caracteres latinos con diacríticos a su forma base
#' usando \code{stringi::stri_trans_general("Latin-ASCII")} y elimina
#' cualquier carácter no imprimible remanente. Útil como paso previo
#' a la normalización de emails o claves de unión.
#'
#' @param texto Vector de caracteres.
#' @return Un vector de caracteres sin acentos ni caracteres no imprimibles.
#' @details La función es vectorizada. Si \code{texto} no es \code{character},
#'   se lanza un error. No modifica espacios ni cambia a minúsculas (eso lo hace
#'   \code{\link{normalizar_email}}).
#'
#' @seealso \code{\link{normalizar_email}}, \code{\link{emails_check}}
#' @family utilidades-email
#'
#' @examples
#' # Ejemplos simples
#' quitar_acentos(c("Camión", "AÑA", "José  "))
#'
#' # En un flujo típico con emails:
#' x <- c("PEDRÓ@HOTMAIL,COM", "ana@gmail.com")
#' quitar_acentos(x)              # "PEDRO@HOTMAIL,COM"  "ana@gmail.com"
#'
#' # Normalización completa (minúsculas + sin acentos + sin espacios):
#' # normalizar_email(x)
#'
#' # Con datos de ejemplo del flujo
#' pre$email <- quitar_acentos(pre$email)
#' head(pre$email)
#' @importFrom stringi stri_trans_general
#' @export
quitar_acentos <- function(texto) {
  stopifnot(is.character(texto))
  y <- stringi::stri_trans_general(texto, "Latin-ASCII")
  # elimina caracteres no imprimibles (incl. restos de transliteración)
  y <- gsub("[^[:print:]]", "", y, perl = TRUE)
  y
}




#' Normaliza emails (minúsculas, sin acentos, sin espacios)
#'
#' Convierte un vector de emails a una representación estándar:
#' \itemize{
#' \item pasa a minúsculas,
#' \item elimina tildes/acentos (\code{\link{quitar_acentos}}),
#' \item elimina espacios de inicio/fin e internos.
#' }
#' No corrige dominios ni typos; para eso use
#' \code{\link{correct_domains_from_excel}} o la revisión por Levenshtein
#' con \code{\link{review_domains_lv}}.
#'
#' @param texto Vector de emails (character o factor).
#' @return Vector \code{character} con emails normalizados.
#' @details
#' \strong{Uso y orden recomendado en el flujo:}
#' \enumerate{
#' \item Detectar problemas de escritura con \code{\link{emails_check}} (opcional).
#' \item \code{normalizar_email()} para estandarizar.
#' \item Corregir dominios con diccionario: \code{\link{correct_domains_from_excel}}.
#' \item Revisar casos restantes con \code{\link{review_domains_lv}}.
#' }
#' Esta función no altera el contenido semántico del email (solo limpieza
#' lexicográfica). No modifica \code{email_google}; use \code{\link{diff_email_google}}
#' para comparar \code{email} vs \code{email_google}.
#'
#' @seealso
#'   \code{\link{quitar_acentos}},
#'   \code{\link{emails_check}},
#'   \code{\link{correct_domains_from_excel}},
#'   \code{\link{review_domains_lv}},
#'   \code{\link{diff_email_google}}
#' @family utilidades-email
#'
#' @examples
#' # --- Ejemplos simples
#' normalizar_email(c("  PEDRÓ @HOTMAIL,COM  ", "Ana.García@GMAIL.COM"))
#' # [1] "pedro@hotmail,com" "ana.garcia@gmail.com"
#'
#' # --- Fragmentos del working example ---
#' library(tibble)
#'
#' pre  <- tibble(
#'   name_norm = c("ana", "luis", "mario", "eva"),
#'   date = as.Date(c("2025-08-01","2025-08-02","2025-08-03","2025-08-04")),
#'   email = c("ana@gmai.com", "luis@HOTMAIL,com", "mario@outlook.com", "eva@gmailcom"),
#'   email_google = c("ana@gmail.com", "luis@hotmail.com", "mario@outlook.com", "eva@gmail.com")
#' )
#'
#' # 1) Diagnóstico (opcional)
#' # emails_check(pre$email)
#'
#' # 2) Normalización
#' pre$email <- normalizar_email(pre$email)
#' head(pre$email)
#'
#' # 3) (Después) Corrección de dominios con diccionario:
#' # pre <- correct_domains_from_excel(pre, "dev/diccionario_emails.xlsx")
#'
#' # 4) (Después) Revisión por Levenshtein:
#' out <- review_domains_lv(
#'   list(pre = pre),
#'   valid_domains = c(
#'     "gmail.com",
#'     "hotmail.com",
#'     "outlook.com"
#'   )
#' )
#'
#' @export
normalizar_email <- function(texto) {
  stopifnot(is.character(texto) | is.factor(texto))
  texto <- trimws(as.character(texto))
  texto <- quitar_acentos(tolower(texto))
  gsub("\\s+", "", texto, perl = TRUE)
}


#' Chequea y reporta errores comunes en emails (múltiples flags)
#'
#' Esta función analiza un vector de emails y marca patrones de escritura
#' problemáticos, como tildes, \code{ñ}, espacios, comas o mayúsculas.  
#' A diferencia de \code{\link{normalizar_email}}, aquí no se corrigen
#' los problemas: solo se reportan con flags lógicos y una descripción
#' resumida en \code{tipo_error}.
#'
#' \itemize{
#'   \item \code{tipo_error}: errores concatenados con \code{"; "} (NA si ninguno).
#'   \item \code{has_*}: columnas lógicas para filtrar rápidamente (incluye \code{has_coma}).
#' }
#'
#' @param texto Vector de emails (character o factor).
#'
#' @return Un tibble con columnas:
#' \itemize{
#'   \item \code{original}: valor original.
#'   \item \code{normalizado}: salida de \code{\link{normalizar_email}}.
#'   \item \code{tipo_error}: etiquetas concatenadas de errores detectados.
#'   \item \code{has_tildes}, \code{has_enye}, \code{has_espacios}, 
#'         \code{has_coma}, \code{has_mayus}: flags lógicos individuales.
#' }
#'
#' @details
#' \strong{Uso recomendado en el flujo:}
#' \enumerate{
#'   \item Diagnóstico previo con \code{emails_check()} para detectar patrones sospechosos.
#'   \item Normalización con \code{\link{normalizar_email}}.
#'   \item Corrección de dominios (\code{\link{correct_domains_from_excel}}).
#'   \item Revisión por Levenshtein (\code{\link{review_domains_lv}}).
#' }
#'
#' @seealso 
#'   \code{\link{normalizar_email}}, 
#'   \code{\link{correct_domains_from_excel}}, 
#'   \code{\link{review_domains_lv}}
#' @family utilidades-email
#'
#' @examples
#' # Ejemplo simple
#' emails_check(c("PEDRÓ@HOTMAIL,COM", " ana @gmail.com "))
#'
#' # Fragmento del working example
#' library(tibble)
#' pre <- tibble(
#'   email = c("ana@gmai.com", "luis@HOTMAIL,com", 
#'             "mario@outlook.com", "eva@gmailcom")
#' )
#'
#' emails_check(pre$email)
#'
#' @export
emails_check <- function(texto) {
  original    <- trimws(as.character(texto))
  normalizado <- normalizar_email(original)
  
  # Flags vectorizados
  has_tildes   <- grepl("[áéíóúÁÉÍÓÚ]", original)
  has_enye     <- grepl("[ñÑ]", original)
  has_espacios <- grepl("\\s", original)
  has_coma     <- grepl(",", original, fixed = TRUE)
  has_mayus    <- grepl("[A-Z]", original)
  
  # Construir tipo_error con todos los que apliquen
  etiquetas <- c("tildes", "ñ", "espacios", "coma", "mayúsculas")
  flag_mat  <- cbind(has_tildes, has_enye, has_espacios, has_coma, has_mayus)
  
  tipo_error <- apply(flag_mat, 1, function(r) {
    if (!any(r)) return(NA_character_)
    paste(etiquetas[which(r)], collapse = "; ")
  })
  
  tibble::tibble(
    original,
    normalizado,
    tipo_error,
    has_tildes,
    has_enye,
    has_espacios,
    has_coma,     # <- el flag que pedías explícitamente
    has_mayus
  )
}

