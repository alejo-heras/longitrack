
test_that("ajustar_particulas pone partículas en minúscula", {
  expect_equal(
    ajustar_particulas("Antonio De La Vega"),
    "Antonio de la Vega"
  )
  expect_equal(
    ajustar_particulas(c("JUAN DEL RÍO", "María VAN DER Meer")),
    c("JUAN del RÍO", "María van der Meer")
  )
})

test_that("nombre_title_case hace Title Case y respeta tildes", {
  expect_equal(
    nombre_title_case("  MARÍA   DEL   PILAR  "),
    "María del Pilar"  # luego la fachada lo volverá a pasar por ajustar_particulas
  )
  expect_equal(
    nombre_title_case("antonio de la vega"),
    "Antonio de la Vega"
  )
})

test_that("normalizar_nombre funciona de extremo a extremo", {
  expect_equal(
    normalizar_nombre("  MARÍA  DEL   PILAR "),
    "María del Pilar"
  )
  expect_equal(
    normalizar_nombre(c("antonio de la vega", "JUAN DEL RÍO")),
    c("Antonio de la Vega", "Juan del Río")
  )
})
