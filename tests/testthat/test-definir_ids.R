test_that("definir_ids works", {

  # passou um parametro a menos
  expect_error(definir_ids('umparametro','doisparametros'))

  # passou dois parametros a menos
  expect_error(definir_ids('umparametro'))


})

