# tests/testthat/test-renombrar-diccionario.R

test_that("renombra columnas y reporta correctamente", {
  df <- data.frame(
    "Identificador" = c("007", "042"),
    "Edad (anios)"  = c("30", "40"),
    check.names = FALSE
  )
  
  dic <- data.frame(
    variable = c("id", "edad"),
    etiqueta = c("Identificador", "Edad (anios)"),
    idioma   = c("es", "es"),
    modulo   = c("caracterizacion", "caracterizacion"),
    stringsAsFactors = FALSE
  )
  
  out <- renombrar_con_diccionario(df, dic, idioma = "es", modulo = "caracterizacion")
  
  expect_named(out, c("datos", "cols_no_renombradas", "vars_no_encontradas", "resumen"))
  expect_equal(names(out$datos), c("id", "edad"))
  expect_length(out$cols_no_renombradas, 0)
  expect_length(out$vars_no_encontradas, 0)
  expect_type(out$resumen, "character")
})

test_that("filtra por idioma y modulo antes de renombrar", {
  df <- data.frame("Name" = "X", check.names = FALSE)
  dic <- data.frame(
    variable = c("nombre_es", "name_en"),
    etiqueta = c("Nombre", "Name"),
    idioma   = c("es", "en"),
    modulo   = c("pre", "pre"),
    stringsAsFactors = FALSE
  )
  
  # Con idioma = "en" debería renombrar "Name" -> "name_en"
  out_en <- renombrar_con_diccionario(df, dic, idioma = "en", modulo = "pre")
  expect_equal(names(out_en$datos), "name_en")
  
  # Con idioma = "es" no hay match -> mantiene "Name"
  out_es <- renombrar_con_diccionario(df, dic, idioma = "es", modulo = "pre")
  expect_equal(names(out_es$datos), "Name")
  expect_true("Name" %in% out_es$cols_no_renombradas)
})

test_that("castea a integer y numeric y avisa por no convertibles", {
  df <- data.frame(
    "Edad"  = c("30", "x", "40"),
    "Peso"  = c("70.5", "80.2", "foo"),
    check.names = FALSE
  )
  dic <- data.frame(
    variable = c("edad", "peso"),
    etiqueta = c("Edad", "Peso"),
    tipo     = c("integer", "numeric"),
    stringsAsFactors = FALSE
  )
  
  expect_warning(
    out <- renombrar_con_diccionario(df, dic),
    regexp = "valores no convertibles -> NA"
  )
  
  expect_s3_class(out$datos$edad, "integer")
  expect_equal(out$datos$edad, as.integer(c(30, NA, 40)))
  
  expect_type(out$datos$peso, "double")
  expect_equal(out$datos$peso, suppressWarnings(as.numeric(c("70.5", "80.2", "foo"))))
})

test_that("castea logical con variantes comunes (si/sí/yes/1 etc.)", {
  df <- data.frame(
    "OK" = c("TRUE", "F", "1", "0", "s\u00ED", "no", "y", "n", "NA"),
    check.names = FALSE
  )
  dic <- data.frame(
    variable = "ok",
    etiqueta = "OK",
    tipo     = "logical",
    stringsAsFactors = FALSE
  )
  
  out <- renombrar_con_diccionario(df, dic)
  expect_type(out$datos$ok, "logical")
  expect_equal(
    out$datos$ok,
    c(TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, NA)
  )
})

test_that("castea date con lubridate::as_date y formato opcional", {
  df <- data.frame(
    "Fecha ISO" = c("2025-08-01", "2025-08-22"),
    "Fecha ES"  = c("01/08/2025", "22/08/2025"),
    check.names = FALSE
  )
  
  dic <- data.frame(
    variable = c("fecha_iso", "fecha_es"),
    etiqueta = c("Fecha ISO", "Fecha ES"),
    tipo     = c("date", "date"),
    formato  = c(NA, "%d/%m/%Y"),
    stringsAsFactors = FALSE
  )
  
  out <- renombrar_con_diccionario(df, dic)
  
  expect_s3_class(out$datos$fecha_iso, "Date")
  expect_s3_class(out$datos$fecha_es,  "Date")
  expect_equal(
    out$datos$fecha_es,
    as.Date(c("2025-08-01", "2025-08-22"))
  )
})

test_that("castea datetime con parseo flexible", {
  df <- data.frame(
    "TS1" = c("2025-08-01 10:20:30", "2025-08-22 08:00:00"),
    "TS2" = c("2025-08-01T10:20:30", "2025-08-22T08:00:00"),
    check.names = FALSE
  )
  
  dic <- data.frame(
    variable = c("ts1", "ts2"),
    etiqueta = c("TS1", "TS2"),
    tipo     = c("datetime", "datetime"),
    stringsAsFactors = FALSE
  )
  
  out <- renombrar_con_diccionario(df, dic)
  
  expect_s3_class(out$datos$ts1, "POSIXct")
  expect_s3_class(out$datos$ts2, "POSIXct")
  # UTC por defecto
  expect_equal(attr(out$datos$ts1, "tzone"), "UTC")
  expect_equal(attr(out$datos$ts2, "tzone"), "UTC")
})

test_that("factor con niveles fijos y mapeo nivel=etiqueta", {
  df <- data.frame(
    "Grupo" = c("B", "A", "B"),
    "Resp"  = c("1", "2", "2"),
    check.names = FALSE
  )
  
  dic <- data.frame(
    variable = c("grupo", "resp"),
    etiqueta = c("Grupo", "Resp"),
    tipo     = c("factor", "factor"),
    niveles  = c("A,B", "1=No,2=S\u00ED"),
    stringsAsFactors = FALSE
  )
  
  out <- renombrar_con_diccionario(df, dic)
  
  expect_s3_class(out$datos$grupo, "factor")
  expect_equal(levels(out$datos$grupo), c("A","B"))
  expect_equal(as.character(out$datos$grupo), c("B","A","B"))
  
  expect_s3_class(out$datos$resp, "factor")
  expect_equal(levels(out$datos$resp), c("1","2"))
  expect_equal(as.character(out$datos$resp), c("No","S\u00ED","S\u00ED"))
})

test_that("sin columna tipo solo renombra (no castea)", {
  df <- data.frame("Edad" = c("30", "40"), check.names = FALSE)
  dic <- data.frame(
    variable = "edad",
    etiqueta = "Edad",
    stringsAsFactors = FALSE
  )
  out <- renombrar_con_diccionario(df, dic)
  expect_equal(names(out$datos), "edad")
  # sigue siendo character
  expect_type(out$datos$edad, "character")
})

test_that("limpieza de etiquetas: espacios, comillas y saltos", {
  df <- data.frame("\"Edad\"\n", c("30","40"))
  names(df) <- c(" \"Edad\" \n", "Otro") # etiqueta sucia
  dic <- data.frame(
    variable = c("edad", "otro"),
    etiqueta = c("Edad", "Otro"),  # limpia en diccionario
    stringsAsFactors = FALSE
  )
  out <- renombrar_con_diccionario(df, dic)
  expect_true(all(c("edad","otro") %in% names(out$datos)))
})

test_that("reporta etiquetas del diccionario no usadas", {
  df <- data.frame("A" = 1)
  dic <- data.frame(
    variable = c("a","b"),
    etiqueta = c("A","B"),
    stringsAsFactors = FALSE
  )
  out <- renombrar_con_diccionario(df, dic)
  expect_true("B" %in% out$vars_no_encontradas)
})

test_that("avisa por etiquetas/variables duplicadas en diccionario", {
  df <- data.frame("X" = 1, check.names = FALSE)
  dic <- data.frame(
    variable = c("x","x"),
    etiqueta = c("X","X"),
    stringsAsFactors = FALSE
  )
  expect_warning(renombrar_con_diccionario(df, dic), "duplicadas")
})
