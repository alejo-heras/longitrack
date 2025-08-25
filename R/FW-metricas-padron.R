


#' Métricas clave sobre el padrón
#' @param padron data.frame con flag_pre, flag_long, (opcional) flag_post y last_long
#' @param cutoff_days días de inactividad para abandono (por defecto 7)
#' @return data.frame con métricas y conteos
#' @export
metricas_padron <- function(padron, cutoff_days = 7) {
  pre  <- as.logical(padron$flag_pre);  pre[is.na(pre)]   <- FALSE
  long <- as.logical(padron$flag_long); long[is.na(long)] <- FALSE
  post <- if ("flag_post" %in% names(padron)) as.logical(padron$flag_post) else rep(FALSE, nrow(padron))
  post[is.na(post)] <- FALSE
  
  last <- suppressWarnings(as.Date(padron$last_long))
  abandono <- long & !is.na(last) & last <= (Sys.Date() - cutoff_days)
  
  out <- data.frame(
    perfiles = c(
      "pretest",
      "pretest + seguimiento",
      "seguimiento",
      "seguimiento + pretest",
      paste0("abandono de >", cutoff_days, "días"),
      "postest",
      "postest + pretest",
      "completo"
    ),
    n = c(
      sum(pre),
      sum(pre & long),
      sum(long),
      sum(long & pre),
      sum(abandono),
      sum(post), 
      sum(post & pre), 
      sum(post & pre & long)
    ),
    row.names = NULL
  )
  out
}
