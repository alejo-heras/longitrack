test_that("quitar_acentos elimina tildes y no imprimibles", {
  x <- c("Camión", "AÑA", "José", "niño")
  out <- quitar_acentos(x)
  esperado <- c("Camion", "ANA", "Jose", "nino")
  expect_equal(out, esperado)
})

test_that("normalizar_email devuelve vector normalizado", {
  v <- c("  José.Pérez @ GmÁil.com ", "ANA@EMPRESA.ES", "nino @ dominio.es", "pepe@ejemplo.es")
  out <- normalizar_email(v)
  esperado <- c("jose.perez@gmail.com", "ana@empresa.es", "nino@dominio.es", "pepe@ejemplo.es")
  expect_equal(out, esperado)
})

test_that("emails_check etiqueta el tipo de error del original", {
  v <- c("  José.Pérez @ GmÁil.com ", "ANA@EMPRESA.ES", "nino @ dominio.es")
  df <- emails_check(v)
  
  # estructura básica
  expect_s3_class(df, "tbl_df")
  expect_named(df, c("original", "normalizado", "tipo_error"))
  
  # normalización correcta
  expect_equal(df$normalizado,
               c("jose.perez@gmail.com", "ana@empresa.es", "nino@dominio.es"))
  
  # tipos de error permitidos
  expect_true(all(df$tipo_error %in% c("tildes", "mayúsculas", "espacios", "ñ", NA_character_)))
})


