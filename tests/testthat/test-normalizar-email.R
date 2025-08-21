test_that("normalizar_email limpia y etiqueta", {
  x <- c("  JOSÉ.PErez @GmAil.com ", "ana_ñ@example.es", "user con espacios@ex.com")
  res <- normalizar_email(x)

  expect_equal(res$normalizado[1], "jose.perez@gmail.com")
  expect_equal(res$tipo_error[1], "tildes")

  expect_equal(res$normalizado[2], "ana_n@example.es")
  expect_equal(res$tipo_error[2], "ñ")

  expect_equal(res$normalizado[3], "userconespacios@ex.com")
  expect_equal(res$tipo_error[3], "espacios")
})
