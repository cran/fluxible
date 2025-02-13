test_that("gets error with wrong inputs", {
  expect_message(
    flux_fun_check(
      list(ar1 = 3, ar2 = "blop"),
      fn = list(is.numeric, is.numeric),
      msg = rep("has to be numeric", 2)
    ),
    "Argument ar2 has to be numeric"
  )
})
