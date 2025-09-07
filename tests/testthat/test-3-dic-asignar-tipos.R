testthat::test_that("convierte tipos básicos (character, numerical, logical) y respeta date", {
  datos <- data.frame(
    id = "1",
    x_num = c("1", "2"),
    x_log = c("TRUE", "FALSE"),
    x_chr = c(1, 2),
    fecha = as.Date(c("2025-01-01", "2025-01-02")),
    stringsAsFactors = FALSE
  )
  
  dic <- data.frame(
    variable = c("x_chr", "x_num", "x_log", "fecha"),
    tipo     = c("character", "numerical", "logical", "date"),
    stringsAsFactors = FALSE
  )
  
  out <- dic_asignar_tipos(datos, diccionario = dic)
  
  expect_true(is.character(out$x_chr))
  expect_true(is.numeric(out$x_num))
  expect_true(is.logical(out$x_log))
  expect_true(inherits(out$fecha, "Date"))
})

testthat::test_that("acepta sinónimos numeric/double para numerical", {
  datos <- data.frame(a = c("1","2","3"))
  dic   <- data.frame(variable = "a", tipo = "double")
  out   <- dic_asignar_tipos(datos, diccionario = dic)
  expect_true(is.numeric(out$a))
  
  dic$tipo <- "numeric"
  out2 <- dic_asignar_tipos(datos, diccionario = dic)
  expect_true(is.numeric(out2$a))
})

testthat::test_that("factor: niveles con 'A,B' y etiquetas con '1=No,2=Sí'", {
  datos1 <- data.frame(sexo = c("A","B","A"))
  dic1   <- data.frame(variable = "sexo", tipo = "factor", niveles = "A,B")
  out1   <- dic_asignar_tipos(datos1, diccionario = dic1)
  expect_true(is.factor(out1$sexo))
  expect_identical(levels(out1$sexo), c("A","B"))
  
  datos2 <- data.frame(q1 = c("1","2","1", NA))
  dic2   <- data.frame(variable = "q1", tipo = "factor", niveles = "1=No,2=Sí")
  out2   <- dic_asignar_tipos(datos2, diccionario = dic2)
  expect_true(is.factor(out2$q1))
  expect_identical(levels(out2$q1), c("No","Sí"))
})

testthat::test_that("factor: fallback seguro cuando 'niveles' no casa con los datos", {
  datos <- data.frame(q = c("X","Y"))
  dic   <- data.frame(variable = "q", tipo = "factor", niveles = "1=No,2=Sí")
  expect_warning(
    out <- dic_asignar_tipos(datos, diccionario = dic),
    regexp = "niveles.*no coincide",
    ignore.case = TRUE
  )
  expect_true(is.factor(out$q))
})

testthat::test_that("avisa de nuevos NA creados al convertir a numerical", {
  datos <- data.frame(a = c("1","x","3"))
  dic   <- data.frame(variable = "a", tipo = "numerical")
  expect_warning(dic_asignar_tipos(datos, diccionario = dic),
                 regexp = "valores no convertibles -> NA")
})

testthat::test_that("no toca columnas fuera del diccionario y solo interseca por nombre", {
  datos <- data.frame(a = "1", b = "2")
  dic   <- data.frame(variable = "a", tipo = "numerical")
  out   <- dic_asignar_tipos(datos, diccionario = dic)
  expect_true(is.numeric(out$a))
  expect_identical(class(out$b), class(datos$b))  # b queda igual
})

testthat::test_that("filtrado por idioma/modulo funciona y error si queda vacío", {
  datos <- data.frame(x = c("1","2"))
  dic   <- data.frame(
    variable = c("x","x"),
    tipo     = c("numerical","numerical"),
    idioma   = c("es","en"),
    modulo   = c("pretest","postest")
  )
  
  out <- dic_asignar_tipos(datos, diccionario = dic, idioma = "es", modulo = "pretest")
  expect_true(is.numeric(out$x))
  
  expect_error(
    dic_asignar_tipos(datos, diccionario = dic, idioma = "fr"),
    regexp = "quedó vacío",
    ignore.case = TRUE
  )
})

testthat::test_that("puede cargar diccionario desde CSV y desde ruta Excel inexistente -> error claro", {
  datos <- data.frame(id = c("1","2"))
  tmp   <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(variable = "id", tipo = "numerical"),
                   file = tmp, row.names = FALSE)
  
  out <- dic_asignar_tipos(datos, diccionario = tmp)
  expect_true(is.numeric(out$id))
  
  expect_error(
    dic_asignar_tipos(datos, diccionario = "no-existe.xlsx"),
    regexp = "No se encontró el diccionario",
    ignore.case = TRUE
  )
})

testthat::test_that("error si faltan columnas requeridas en el diccionario", {
  datos <- data.frame(a = "1")
  dic_mal <- data.frame(variable = "a")  # falta 'tipo'
  expect_error(
    dic_asignar_tipos(datos, diccionario = dic_mal),
    regexp = "debe tener columnas: variable, tipo",
    ignore.case = TRUE
  )
})
