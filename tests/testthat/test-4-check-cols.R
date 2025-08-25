test_that("check_cols_pre valida estructura básica", {
  pre_ok <- data.frame(email = c("a@x.com","b@x.com"), name = c("Ana","Bea"))
  expect_silent(check_cols_pre(pre_ok))
  
  pre_dup <- data.frame(email = c("a@x.com","a@x.com"), name = c("Ana","Ana"))
  expect_warning(check_cols_pre(pre_dup), "duplicados")
  
  pre_missing <- data.frame(email = c("a@x.com","b@x.com"))
  expect_error(check_cols_pre(pre_missing), "name")
  
  pre_factor <- data.frame(
    email = factor(c("a@x.com","b@x.com")),
    name = c("Ana","Bea")
  )
  expect_error(check_cols_pre(pre_factor), "character")
})

test_that("check_cols_long valida estructura básica", {
  long_ok <- data.frame(
    email = c("a@x.com","b@x.com"),
    date  = as.Date(c("2025-01-01","2025-01-02"))
  )
  expect_silent(check_cols_long(long_ok))
  
  long_dup <- data.frame(
    email = c("a@x.com","a@x.com"),
    date  = as.Date(c("2025-01-01","2025-01-01"))
  )
  expect_warning(check_cols_long(long_dup), "duplicadas")
  
  long_bad <- data.frame(email = c("a@x.com","b@x.com"))
  expect_error(check_cols_long(long_bad), "date")
  
  long_factor <- data.frame(
    email = factor(c("a@x.com","b@x.com")),
    date  = as.Date(c("2025-01-01","2025-01-02"))
  )
  expect_error(check_cols_long(long_factor), "character")
  
  long_bad_date <- data.frame(
    email = c("a@x.com","b@x.com"),
    date  = c("2025-01-01","2025-01-02")  # character, no Date/POSIXt
  )
  expect_error(check_cols_long(long_bad_date), "Date")
})

test_that("check_cols_post llama a check_cols_pre", {
  post_ok <- data.frame(email = "a@x.com", name = "Ana")
  expect_silent(check_cols_post(post_ok))
  
  post_factor <- data.frame(
    email = factor("a@x.com"),
    name = "Ana"
  )
  expect_error(check_cols_post(post_factor), "character")
})

