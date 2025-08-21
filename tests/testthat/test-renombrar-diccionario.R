test_that("renombra columnas con coincidencias exactas", {
  datos <- data.frame("Identificador" = 1:2, "Edad (años)" = c(30, 40), check.names = FALSE)
  dic   <- data.frame(variable = c("id", "edad"), etiqueta = c("Identificador", "Edad (años)"))

  res <- renombrar_con_diccionario(datos, dic)

  expect_equal(names(res$datos), c("id", "edad"))
  expect_length(res$cols_no_renombradas, 0)
  expect_length(res$vars_no_encontradas, 0)
})

test_that("mantiene columnas sin match y reporta no usadas", {
  datos <- data.frame("Identificador" = 1:2, "Altura" = c(160, 175), check.names = FALSE)
  dic   <- data.frame(variable = c("id", "edad"), etiqueta = c("Identificador", "Edad (años)"))

  res <- renombrar_con_diccionario(datos, dic)

  expect_equal(names(res$datos), c("id", "Altura"))  # solo renombra la primera
  expect_equal(res$cols_no_renombradas, "Altura")
  expect_equal(res$vars_no_encontradas, "Edad (años)")
})

test_that("limpieza de etiquetas: espacios, comillas y saltos", {
  datos <- data.frame('  "Edad  (años)"  ' = c(30, 40), check.names = FALSE)
  dic   <- data.frame(variable = "edad", etiqueta = "Edad (años)")

  res <- renombrar_con_diccionario(datos, dic)

  expect_equal(names(res$datos), "edad")
})
