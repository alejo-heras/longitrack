test_that("resumen_perfiles cuenta correctamente con post", {
  pre  <- tibble::tibble(email = c("a","b","c"))
  long <- tibble::tibble(email = c("b","b","d"),
                         date  = as.Date(c("2025-01-01","2025-01-08","2025-01-05")))
  post <- tibble::tibble(email = c("b","x"))
  
  res <- resumen_perfiles(pre, long, post, cutoff_days = 7, ref_date = as.Date("2025-01-20"))
  
  expect_named(res, c("grupo","n"))
  expect_true(any(res$grupo == "PRE sin seguimiento"))
  expect_true(any(res$grupo == "Seguimiento sin PRE"))
  expect_true(any(res$grupo == "POST sin PRE"))
  expect_true(any(grepl("^Abandono", res$grupo)))
  
  n_pre_sin_long <- res$n[res$grupo == "PRE sin seguimiento"]        # a,c no están en long -> 2
  n_long_sin_pre <- res$n[res$grupo == "Seguimiento sin PRE"]        # d no está en pre   -> 1
  n_post_sin_pre <- res$n[res$grupo == "POST sin PRE"]               # x no está en pre   -> 1
  n_abandono     <- res$n[grepl("^Abandono", res$grupo)]             # todos inactivos >=7d -> b,d => 2
  
  expect_equal(n_pre_sin_long, 2L)
  expect_equal(n_long_sin_pre, 1L)
  expect_equal(n_post_sin_pre, 1L)
  expect_equal(n_abandono, 2L)
})

test_that("resumen_perfiles funciona sin post (post = NULL)", {
  pre  <- tibble::tibble(email = c("a","b"))
  long <- tibble::tibble(email = c("b","c"),
                         date  = as.Date(c("2025-01-01","2025-01-02")))
  
  res <- resumen_perfiles(pre, long, post = NULL, cutoff_days = 7, ref_date = as.Date("2025-01-20"))
  
  expect_true(any(res$grupo == "PRE sin seguimiento"))
  expect_true(any(res$grupo == "Seguimiento sin PRE"))
  
  if ("POST sin PRE" %in% res$grupo) {
    expect_equal(res$n[res$grupo == "POST sin PRE"], 0L)
  } else {
    succeed()
  }
  
  
  n_pre_sin_long <- res$n[res$grupo == "PRE sin seguimiento"]  # a -> 1
  n_long_sin_pre <- res$n[res$grupo == "Seguimiento sin PRE"]  # c -> 1
  
  expect_equal(n_pre_sin_long, 1L)
  expect_equal(n_long_sin_pre, 1L)
})
