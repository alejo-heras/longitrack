#' longitrack: Seguimiento de trabajo de campo longitudinal
#'
#' Herramientas para **importar**, **normalizar** y **monitorizar** trabajo de
#' campo cuantitativo longitudinal con tres módulos:
#' - **Pretest** (uno por persona - cuestionario inicial),
#' - **Mediciones longitudinales** en **formato long** (varias por individuo en el tiempo),
#' - **Postest** (uno por persona - cuestionario final).
#'
#' Incluye:
#' - **Importación** desde Google Sheets.
#' - **Normalización** de identificadores personales (emails, nombres).
#' - **Renombrado** de variables según libro de códigos (variable, etiqueta, tipo).
#' - **Chequeos y validación** de `email` en todos los módulos.
#' - **Corrección de emails** según la distancia de Levenshtein y errores comunes al escribir el dominio.
#' - Construcción de un **padrón único de participantes** y **flags** de presencia por módulo.
#' - **Métricas longitudinales** de participación, seguimiento y retención.
#'
#' @docType package
#' @name longitrack
#'
#'
#' @author
#' Alejandro González Heras
#'
#' @seealso
#' Vignettes (en desarrollo)
#'
#'
#' @examples
#' \dontrun{
#' # 1) Importar Google Sheets (ejemplo)
#' urls <- list(
#'   es = list(
#'     pre  = "https://docs.google.com/spreadsheets/d/ID_PRE",
#'     long = "https://docs.google.com/spreadsheets/d/ID_LONG",
#'     post = "https://docs.google.com/spreadsheets/d/ID_POST"
#'   )
#' )
#' data  <- importar_gs(idioma = "es", modulos = "all", urls,
#'                      mapping = TRUE, cast = TRUE, check = TRUE)
#' pre  <- data$pre$datos
#' long <- data$long$datos
#' post <- data$post$datos
#'
#' # 3) Chequear emails (no modifica la variable)
#'
#' emails_norm_pre  <- emails_check(pre$email)
#'
#' # 2) Normalizar emails (cambia mayúsculas, acentos...)
#' pre$email  <- normalizar_email(pre$email)
#'
#' # 4) Padrón, métricas longitudinales y curva de retención
#' padron <- padron_participantes(pre, long, post)
#' m_long <- metricas_longitudinales(pre, long, post, periodo = 7, by = "dia")
#' curva  <- retencion_semanas(long, 7)
#' }
"_PACKAGE"


