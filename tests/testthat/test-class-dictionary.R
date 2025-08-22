test_that("date: no cambia", {
  df0 <- data.frame(date_col = as.Date(c("2025-01-01", "2025-01-02")))
  dic <- data.frame(variable = "date_col", tipo = "date", stringsAsFactors = FALSE)
  
  df <- class_dictionary(df0, dic)
  expect_s3_class(df$date_col, "Date")
  expect_identical(df$date_col, df0$date_col)
})

test_that("character: as.character()", {
  df0 <- data.frame(x = c(1, 2, 3))
  dic <- data.frame(variable = "x", tipo = "character", stringsAsFactors = FALSE)
  
  expect_no_warning(df <- class_dictionary(df0, dic))
  expect_type(df$x, "character")
  expect_identical(df$x, as.character(df0$x))
})

test_that("numerical: as.numeric() con warning por NA nuevos", {
  df0 <- data.frame(n = c("10", "x", "20"), stringsAsFactors = FALSE)
  dic <- data.frame(variable = "n", tipo = "numerical", stringsAsFactors = FALSE)
  
  expect_warning(df <- class_dictionary(df0, dic), "valores no convertibles")
  expect_type(df$n, "double")
  expect_equal(df$n, suppressWarnings(as.numeric(df0$n)))
})

test_that("logical: as.logical() (solo TRUE/FALSE/T/F) y warning por NA nuevos", {
  df0 <- data.frame(ok = c("TRUE", "F", "1", NA, "foo"), stringsAsFactors = FALSE)
  dic <- data.frame(variable = "ok", tipo = "logical", stringsAsFactors = FALSE)
  
  expect_warning(df <- class_dictionary(df0, dic), "valores no convertibles")
  expect_type(df$ok, "logical")
  expect_equal(df$ok, c(TRUE, FALSE, NA, NA, NA))
})

test_that("factor: as.factor() sin niveles", {
  df0 <- data.frame(g = c("B", "A", "B"), stringsAsFactors = FALSE)
  dic <- data.frame(variable = "g", tipo = "factor", stringsAsFactors = FALSE)
  
  expect_no_warning(df <- class_dictionary(df0, dic))
  expect_s3_class(df$g, "factor")
  expect_equal(levels(df$g), c("A", "B"))  # orden alfabetico por defecto
})

test_that("factor: usa niveles del diccionario", {
  df0 <- data.frame(g = c("B", "A", "B"), stringsAsFactors = FALSE)
  dic <- data.frame(variable = "g", tipo = "factor", niveles = "A,B", stringsAsFactors = FALSE)
  
  expect_no_warning(df <- class_dictionary(df0, dic))
  expect_s3_class(df$g, "factor")
  expect_equal(levels(df$g), c("A", "B"))
  expect_equal(as.character(df$g), c("B", "A", "B"))
})

test_that("factor: si 'niveles' no casa con los datos, hace fallback a as.factor() y avisa", {
  df0 <- data.frame(g = c("B", "C", "A"), stringsAsFactors = FALSE)
  dic <- data.frame(variable = "g", tipo = "factor", niveles = "A,B", stringsAsFactors = FALSE)
  
  expect_warning(df <- class_dictionary(df0, dic), "'niveles' no coincide")
  expect_s3_class(df$g, "factor")
  # Fallback: niveles vienen de los datos (orden alfabético por defecto)
  expect_equal(levels(df$g), sort(unique(df0$g)))
  expect_equal(as.character(df$g), df0$g)
})

test_that("factor: mapea codigo=etiqueta cuando encaja", {
  df0 <- data.frame(resp = c("1","2","2","1"), stringsAsFactors = FALSE)
  dic <- data.frame(variable = "resp", tipo = "factor", niveles = "1=No,2=Sí", stringsAsFactors = FALSE)
  
  expect_no_warning(df <- class_dictionary(df0, dic))
  expect_s3_class(df$resp, "factor")
  expect_equal(levels(df$resp), c("No","Sí"))
  expect_equal(as.character(df$resp), c("No","Sí","Sí","No"))
})

