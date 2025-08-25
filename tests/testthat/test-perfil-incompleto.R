test_that("perfil_incompleto filtra correctamente según flag", {
  pre  <- data.frame(email = c("a@x.com","b@x.com"))
  long <- data.frame(
    email = c("a@x.com","c@x.com"),
    date  = as.Date(c("2025-01-01","2025-01-05"))
  )
  post <- data.frame(email = "a@x.com")
  
  res   <- add_flags(pre, long, post)
  pre2  <- res$pre
  long2 <- res$long
  post2 <- res$post
  
  # PRE sin LONG -> b@x.com
  out_pre <- perfil_incompleto(pre2, "flag_long", FALSE)
  expect_equal(out_pre$email, "b@x.com")
  
  # LONG sin PRE -> c@x.com
  out_long <- perfil_incompleto(long2, "flag_pre", FALSE)
  expect_equal(out_long$email, "c@x.com")
  
  # POST sin PRE -> ninguno (porque post = a@x.com y sí está en pre)
  out_post <- perfil_incompleto(post2, "flag_pre", FALSE)
  expect_equal(nrow(out_post), 0)
})

test_that("perfil_incompleto da error si flag no existe", {
  df <- data.frame(email = "a@x.com")
  expect_error(perfil_incompleto(df, "flag_inventado"), "No existe la columna")
})
