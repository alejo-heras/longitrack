test_that("perfil_abandono identifica correctamente abandono", {
  long <- data.frame(
    email = c("a@x.com","a@x.com","b@x.com","c@x.com"),
    date  = as.Date(c("2025-01-01","2025-01-10","2025-01-15","2025-01-19"))
  )
  
  # ref_date = 2025-01-20, cutoff_days = 7
  out <- perfil_abandono(long, cutoff_days = 7, ref_date = as.Date("2025-01-20"))
  
  # a@x.com última medición 2025-01-10 -> abandono
  # b@x.com última medición 2025-01-15 -> NO abandono
  # c@x.com última medición 2025-01-19 -> NO abandono
  expect_equal(out$email, "a@x.com")
  expect_equal(out$ultima_fecha, as.Date("2025-01-10"))
})

test_that("perfil_abandono devuelve data.frame vacío si nadie cumple", {
  long <- data.frame(
    email = c("a@x.com","b@x.com"),
    date  = as.Date(c("2025-01-18","2025-01-19"))
  )
  
  out <- perfil_abandono(long, cutoff_days = 7, ref_date = as.Date("2025-01-20"))
  expect_equal(nrow(out), 0)
  expect_true(all(c("email","ultima_fecha") %in% names(out)))
})

test_that("perfil_abandono maneja POSIXct en long$date", {
  long <- data.frame(
    email = "a@x.com",
    date  = as.POSIXct("2025-01-01 12:00:00", tz = "UTC")
  )
  
  out <- perfil_abandono(long, cutoff_days = 7, ref_date = as.Date("2025-01-20"))
  expect_equal(out$email, "a@x.com")
  expect_s3_class(out$ultima_fecha, "Date")
})

