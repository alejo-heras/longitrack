# data-raw/data_ejemplo_longitrack.R
# Datos minimalistas para la vignette

library(dplyr)
library(tidyr)
library(tibble)

set.seed(1)

# --- helpers de "ruido" -----------------
err_name <- function(x) {
  # introduce un (1) error ligero en el nombre
  tweak <- sample(c("spaces", "upper", "lower", "double_space"), 1)
  if (tweak == "spaces") paste0(" ", x, " ")
  else if (tweak == "upper") toupper(x)
  else if (tweak == "lower") tolower(x)
  else gsub(" ", "  ", x) # double_space
}

err_email <- function(x) {
  # introduce un (1) error común en emails
  tweak <- sample(c("spaces_at", "upper", "comma_dot", "double_dot", "tld_typo"), 1)
  if (tweak == "spaces_at") sub("@", " @ ", x, fixed = TRUE)
  else if (tweak == "upper") toupper(x)
  else if (tweak == "comma_dot") sub("\\.", ",", x)               # hotmail,com
  else if (tweak == "double_dot") sub("\\.com$", "..com", x)      # gmail..com
  else sub("\\.com$|\\.es$", ".con", x)                           # gmail.con / yahoo.con
}

# --- base de personas -------------------
n_id   <- 12
ids    <- sprintf("id%02d", 1:n_id)
names0 <- c("Ana", "Luis", "María", "Eva", "Pablo", "Marta",
            "Jorge", "Nora", "Íñigo", "Sofía", "José Pérez", "Niño")
domains <- c("gmail.com", "hotmail.com", "yahoo.es", "empresa.es")

pre <- tibble(
  email = paste0(ids, "@", sample(domains, n_id, replace = TRUE)),
  name  = names0[1:n_id]
) %>%
  # inyectar errores en ~60% de nombres y ~60% de emails
  mutate(
    name  = if_else(runif(n()) < .6, vapply(name, err_name, character(1)), name),
    email = if_else(runif(n()) < .6, vapply(email, err_email, character(1)), email),
    date  = as.Date("2025-08-01") + sample(0:3, n(), replace = TRUE),
  )

# --- mediciones longitudinales ----------------
# 24 mediciones totales, con probabilidad desigual por persona
target_meas <- 24
w <- runif(n_id); w <- w / sum(w)
long <- tibble(
  email = sample(pre$email, size = target_meas, replace = TRUE, prob = w),
  date  = as.Date("2025-08-01") + sample(0:37, target_meas, replace = TRUE)
) %>%
  arrange(email, date)

# añadir 3 externos (no_pre) para crear perfil "no_pre"
long <- bind_rows(
  long,
  tibble(
    email = paste0("externo", 1:3, "@example.org"),
    date  = as.Date("2025-08-10") + c(5, 12, 28)
  )
)

# marcar algunos como "abandono" forzando última fecha <= hoy-7 (2025-09-07 - 7 = 2025-08-31)
abandon <- sample(unique(long$email), size = 3)
long <- long %>%
  mutate(date = if_else(email %in% abandon & date > as.Date("2025-08-31"),
                        as.Date("2025-08-31"), date))

# --- posttest --------------------------
# solo para ~60% de quienes están en pre y con al menos una medición en long
post <- long %>%
  semi_join(pre, by = "email") %>%
  distinct(email) %>%
  slice_sample(prop = .6) %>%
  mutate(date = as.Date("2025-09-05") + sample(0:2, n(), replace = TRUE))

# --- guardar en data/ -------------------
usethis::use_data(pre, long, post, overwrite = TRUE, compress = "xz")
