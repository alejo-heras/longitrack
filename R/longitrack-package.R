#' longitrack: Seguimiento de trabajo de campo longitudinal
#'
#' Herramientas para **importar**, **limpiar** y **monitorizar** trabajo de campo
#' cuantitativo longitudinal. Incluye:
#'
#' - **Importación** de datos de Google Sheets.
#' - **Normalización** de identificadores de personas (emails, nombres).
#' - **Mapear variables** según un libro de códigos (variable, etiqueta, tipo de variable)
#' - **Chequeos** y validación de `email` en todas las hojas de datos importadas.
#' - **Corrección de emails** según la distancia de Levenshtein y errores comunes al escribir el dominio.
#' - **Construcción de padrón** (base de datos única) y **flags** de presencia según los diferentes cuestionarios (pre/long/post).
#' - **Métricas longitudinales** (actividad, retención, acumulados) y utilidades de reporte.
#'
#' @docType package
#' @name longitrack
#'
#' @section Flujo típico:
#' 1. Importar datos (pre/long/post) y mapear variables.
#' 2. Estandarizar emails y nombres; contrastar `email` vs `email_google`.
#' 3. Aplicar correcciones por diccionario y revisión difusa de dominios.
#' 4. Construir padrón y añadir \code{flag_pre}, \code{flag_long}, \code{flag_post}.
#' 5. Excluir casos (listas externas o bajas voluntarias).
#' 6. Calcular métricas y curvas de retención; exportar tabulados/gráficos.
#'
#'
#' @seealso
#' Vignettes (en desarrollo)
#'
#' @author
#' Alejandro González Heras
#'
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
#' # 2) Normalizar y chequear
#' pre$email  <- normalizar_email(pre$email)
#' long$email <- normalizar_email(long$email)
#' post$email <- normalizar_email(post$email)
#' pre$name_norm  <- normalizar_nombre(pre$name)
#' long$name_norm <- normalizar_nombre(long$name)
#' post$name_norm <- normalizar_nombre(post$name)
#' diff_pre  <- diff_email_google(pre)
#'
#' # 3) Correcciones determinísticas + revisión difusa
#' pre  <- correct_domains_from_excel(pre,  "diccionario_emails.xlsx",
#'                                    vars = c("email","email_google"))
#' validos <- c("gmail.com","outlook.com","hotmail.com","yahoo.com","yahoo.es")
#' rv <- review_domains_lv(
#'   datos = list(pre = pre, long = long, post = post),
#'   valid_domains = validos, max_dist = 2, var = "email"
#' )
#'
#' # 4) Padrón y métricas
#' padron  <- padron_participantes(pre, long, post)
#' padron$name_norm <- normalizar_nombre(padron$name_norm)
#' m_long <- metricas_longitudinales(pre, long, post, periodo = 7, by = "dia")
#' curva  <- retencion_semanas(long, 7)
#' }
"_PACKAGE"


