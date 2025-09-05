#' Padrón único de participantes 
#' 
#' Devuelve emails únicos que aparezcan en PRE, LONG o POST,
#' con flags y métricas básicas.
#'
#' @param pre  data.frame con `email` y opcionalmente `name`/`nombre`.
#' @param long data.frame con `email`, `date` y opcionalmente `name`/`nombre`.
#' @param post data.frame con `email` y opcionalmente `name`/`nombre`. Opcional.
#' @return Tibble/data.frame con columnas:
#'   - `email`
#'   - `name_norm` 
#'   - `flag_pre`, `flag_long`, `flag_post`
#'   - `n_long`, `first_long`, `last_long`
#'   
#' @details
#' - `name_norm` se obtiene como el primer no-NA entre `pre$name_norm`,
#'   `long$name_norm` y `post$name_norm` (en ese orden).
#' - `n_long` es el nº de filas LONG por email; `first_long`/`last_long` son la
#'   primera/última fecha LONG (clase `Date` cuando sea posible).
#'
#' @examples
#' pre  <- data.frame(email = c("a@x.com","b@x.com"), name_norm = c("Ana", NA))
#' long <- data.frame(email = c("a@x.com","a@x.com","c@x.com"),
#'                    date  = as.Date(c("2025-01-01","2025-01-08","2025-02-01")),
#'                    name_norm = c(NA,"Ana P.", "Carlos"))
#' post <- data.frame(email = "d@x.com", name_norm = "Dana")
#' padron <- padron_participantes(pre, long, post)
#' head(padron)
#'      
#' @export
#'
#' @importFrom tibble tibble
#' @importFrom dplyr filter group_by summarise left_join mutate coalesce select n
#' @importFrom rlang .data
padron_participantes <- function(pre, long, post = NULL) {
  tolower_col <- function(x) tolower(trimws(as.character(x)))
  clean_name  <- function(x) { x <- trimws(as.character(x)); x[x == ""] <- NA_character_; x }
  pick_name <- function(x) { x <- clean_name(x); if (all(is.na(x))) return(NA_character_); names(sort(table(x), decreasing = TRUE))[1] }
  get_name <- function(df) { if ("name" %in% names(df)) df[["name"]] else if ("nombre" %in% names(df)) df[["nombre"]] else rep(NA_character_, nrow(df)) }
  
  stopifnot("email" %in% names(pre), all(c("email", "date") %in% names(long)))
  if (!is.null(post)) stopifnot("email" %in% names(post))
  
  pre_email  <- tolower_col(pre$email)
  long_email <- tolower_col(long$email)
  post_email <- if (!is.null(post)) tolower_col(post$email) else character(0)
  
  todos <- tibble::tibble(email = unique(c(pre_email, long_email, post_email)))
  
  long_dates <- suppressWarnings(as.Date(long$date))
  if (all(is.na(long_dates)) && !inherits(long$date, "Date")) long_dates <- long$date
  
  long_summ <- tibble::tibble(email = long_email, date = long_dates) |>
    dplyr::filter(!is.na(.data$email)) |>
    dplyr::group_by(.data$email) |>
    dplyr::summarise(
      n_long     = dplyr::n(),
      first_long = suppressWarnings(min(.data$date, na.rm = TRUE)),
      last_long  = suppressWarnings(max(.data$date, na.rm = TRUE)),
      .groups = "drop"
    )
  
  pre_names  <- tibble::tibble(email = pre_email,  nm = get_name(pre))  |> dplyr::group_by(.data$email) |> dplyr::summarise(pre_name  = pick_name(.data$nm),  .groups = "drop")
  long_names <- tibble::tibble(email = long_email, nm = get_name(long)) |> dplyr::group_by(.data$email) |> dplyr::summarise(long_name = pick_name(.data$nm), .groups = "drop")
  post_names <- if (!is.null(post)) tibble::tibble(email = post_email, nm = get_name(post)) |> dplyr::group_by(.data$email) |> dplyr::summarise(post_name = pick_name(.data$nm), .groups = "drop")
  else tibble::tibble(email = character(0), post_name = character(0))
  
  out <- todos |>
    dplyr::left_join(long_summ,  by = "email") |>
    dplyr::left_join(pre_names,  by = "email") |>
    dplyr::left_join(long_names, by = "email") |>
    dplyr::left_join(post_names, by = "email") |>
    dplyr::mutate(
      name_norm = dplyr::coalesce(.data$pre_name, .data$long_name, .data$post_name),
      flag_pre  = .data$email %in% pre_email,
      flag_long = !is.na(.data$n_long) & .data$n_long > 0L,
      flag_post = if (length(post_email)) .data$email %in% post_email else FALSE
    )
  
  out |>
    dplyr::mutate(
      n_long     = dplyr::coalesce(as.integer(.data$n_long), 0L),
      first_long = if (inherits(long_dates, "Date")) as.Date(.data$first_long) else .data$first_long,
      last_long  = if (inherits(long_dates, "Date"))  as.Date(.data$last_long)  else .data$last_long
    ) |>
    dplyr::select(.data$email, .data$name_norm, .data$flag_pre, .data$flag_long, .data$flag_post,
                  .data$n_long, .data$first_long, .data$last_long)
}
