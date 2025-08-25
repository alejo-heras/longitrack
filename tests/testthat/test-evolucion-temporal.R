test_that("evolucion_temporal funciona en un caso mínimo", {
  pre <- tibble::tibble(
    email = c("a@x.com", "b@x.com"),
    date  = as.Date(c("2023-01-01", "2023-01-02"))
  )
  
  long <- tibble::tibble(
    email = c("a@x.com", "a@x.com", "c@x.com"),
    date  = as.Date(c("2023-01-05", "2023-01-12", "2023-01-07"))
  )
  
  res <- evolucion_temporal(pre, long, nivel = "semana")
  
  expect_s3_class(res, "data.frame")
  expect_true(all(c("periodo_inicio","eventos_total","personas_total") %in% names(res)))
  expect_true(all(res$eventos_total >= 0))
  expect_true(all(res$personas_total >= 0))
})
