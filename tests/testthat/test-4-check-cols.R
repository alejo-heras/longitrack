# tests/testthat/test-4-check-cols.R

testthat::test_that("check_cols_pre: ok con email/name correctos", {
  df <- data.frame(email = c("a@x.com", "b@x.com"),
                   name  = c("Ana", "Bob"),
                   stringsAsFactors = FALSE)
  # cli::cli_inform emite un message(); lo capturamos
  expect_message(check_cols_pre(df), regexp = "pas[oó] todas las validaciones", ignore.case = TRUE)
})

testthat::test_that("check_cols_pre: error por columnas faltantes / tipo / NA-vacíos", {
  # Falta name
  df1 <- data.frame(email = "a@x.com")
  expect_error(check_cols_pre(df1), regexp = "Faltan columnas|debe contener|Faltan", ignore.case = TRUE)
  
  # email no es character
  df2 <- data.frame(email = 1L, name = "Ana")
  expect_error(check_cols_pre(df2), regexp = "character", ignore.case = TRUE)
  
  # email vacío/NA
  df3 <- data.frame(email = c("a@x.com", ""), name = c("Ana","Ana"))
  expect_error(check_cols_pre(df3), regexp = "vac[ií]os|NA", ignore.case = TRUE)
})

testthat::test_that("check_cols_pre: warning por emails duplicados", {
  df <- data.frame(email = c("a@x.com", "a@x.com"), name = c("Ana","Ana"))
  expect_warning(check_cols_pre(df), regexp = "duplicados", ignore.case = TRUE)
})

# -------------------------------------------------------------------

testthat::test_that("check_cols_long: ok con Date y con POSIXt", {
  dfD <- data.frame(email = "a@x.com", date = as.Date("2025-01-01"))
  expect_message(check_cols_long(dfD), regexp = "validaciones", ignore.case = TRUE)
  
  dfP <- data.frame(email = "a@x.com", date = as.POSIXct("2025-01-01 10:00:00", tz = "UTC"))
  expect_message(check_cols_long(dfP), regexp = "validaciones", ignore.case = TRUE)
})

testthat::test_that("check_cols_long: errores por columnas/tipos/NA", {
  # Falta date
  df1 <- data.frame(email = "a@x.com")
  expect_error(check_cols_long(df1), regexp = "Faltan columnas|debe contener|Faltan", ignore.case = TRUE)
  
  # email no character
  df2 <- data.frame(email = 1L, date = as.Date("2025-01-01"))
  expect_error(check_cols_long(df2), regexp = "character", ignore.case = TRUE)
  
  # date no es Date/POSIXt
  df3 <- data.frame(email = "a@x.com", date = "2025-01-01")
  expect_error(check_cols_long(df3), regexp = "Date|POSIXt", ignore.case = TRUE)
  
  # email vacío/NA
  df4 <- data.frame(email = c("a@x.com",""), date = as.Date(c("2025-01-01","2025-01-02")))
  expect_error(check_cols_long(df4), regexp = "vac[ií]os|NA", ignore.case = TRUE)
})

testthat::test_that("check_cols_long: warning por duplicados en (email, date)", {
  df <- data.frame(
    email = c("a@x.com","a@x.com","b@x.com"),
    date  = as.Date(c("2025-01-01","2025-01-01","2025-01-02"))
  )
  expect_warning(check_cols_long(df), regexp = "duplicadas|duplicados", ignore.case = TRUE)
})

# -------------------------------------------------------------------

testthat::test_that("check_cols_post: ok y errores/duplicados como PRE", {
  ok <- data.frame(email = c("a@x.com","b@x.com"), name = c("Ana","Bob"))
  expect_message(check_cols_post(ok), regexp = "validaciones", ignore.case = TRUE)
  
  falta <- data.frame(email = "a@x.com")
  expect_error(check_cols_post(falta), regexp = "Faltan|contener", ignore.case = TRUE)
  
  dup <- data.frame(email = c("a@x.com","a@x.com"), name = c("Ana","Ana"))
  expect_warning(check_cols_post(dup), regexp = "duplicados", ignore.case = TRUE)
})

