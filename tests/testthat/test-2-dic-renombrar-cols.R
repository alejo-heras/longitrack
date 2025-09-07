testthat::test_that("renombra columnas según etiqueta (happy path)", {
  df  <- data.frame("Identificador" = 1:2, "Edad (años)" = c(30, 40), check.names = FALSE)
  dic <- data.frame(
    variable = c("id", "edad"),
    etiqueta = c("Identificador", "Edad (años)")
  )
  
  out <- dic_renombrar_cols(df, diccionario = dic, verbose = FALSE)
  
  expect_named(out, c("datos","cols_no_renombradas","vars_no_encontradas","resumen"))
  expect_true(all(c("id","edad") %in% names(out$datos)))
  expect_length(out$cols_no_renombradas, 0)
  expect_length(out$vars_no_encontradas, 0)
})

testthat::test_that("limpia etiquetas: espacios, comillas, saltos y dd/mm/aaaa", {
  df  <- data.frame('  "Fecha (dd/mm/aaaa)"  ' = "01/01/2025",
                    check.names = FALSE)
  dic <- data.frame(
    variable = "fecha",
    etiqueta = 'Fecha (dd/mm/aaaa)'
  )
  out <- dic_renombrar_cols(df, diccionario = dic, verbose = FALSE)
  expect_true("fecha" %in% names(out$datos))
})

testthat::test_that("filtra por idioma y modulo si existen", {
  df  <- data.frame("Identificador" = 1, "Edad (años)" = 30, check.names = FALSE)
  dic <- data.frame(
    variable = c("id", "edad"),
    etiqueta = c("Identificador", "Edad (años)"),
    idioma   = c("es", "es"),
    modulo   = c("pretest","seguimiento"),
    stringsAsFactors = FALSE
  )
  
  # Solo debe aplicar la fila (es, pretest)
  out <- dic_renombrar_cols(df, diccionario = dic, idioma = "es")
  expect_true(all(c("id","edad") %in% names(out$datos)))  # renombra ambas si coinciden
  
  # Si el filtro vacía el diccionario -> error
  expect_error(
    dic_renombrar_cols(df, diccionario = dic, idioma = "fr"),
    "quedó vacío",
    fixed = FALSE
  )
})

testthat::test_that("devuelve columnas sin coincidencia y variables no usadas", {
  df  <- data.frame("A"=1, "B"=2, check.names = FALSE)
  dic <- data.frame(
    variable = c("a","c"),
    etiqueta = c("A","C")
  )
  out <- dic_renombrar_cols(df, diccionario = dic, verbose = FALSE)
  # B no estaba en diccionario
  expect_true("B" %in% out$cols_no_renombradas)
  # C no estaba en datos
  expect_true("C" %in% out$vars_no_encontradas)
})

testthat::test_that("verbose imprime resumen y avisa de etiquetas duplicadas", {
  df  <- data.frame("X"=1, check.names = FALSE)
  dic <- data.frame(
    variable = c("x","x2"),
    etiqueta = c("X","X") # duplicada
  )
  expect_output(
    dic_renombrar_cols(df, diccionario = dic, verbose = TRUE),
    regexp = "Resumen de renombrado:",
    fixed  = FALSE
  )
  expect_output(
    dic_renombrar_cols(df, diccionario = dic, verbose = TRUE),
    regexp = "etiquetas duplicadas",
    fixed  = FALSE
  )
})

testthat::test_that("puede cargar diccionario desde ruta CSV", {
  df  <- data.frame("Identificador" = 1, check.names = FALSE)
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(variable="id", etiqueta="Identificador"),
                   file = tmp, row.names = FALSE)
  out <- dic_renombrar_cols(df, diccionario = tmp, verbose = FALSE)
  expect_true("id" %in% names(out$datos))
})

