test_that("quality flags count works", {
  expect_snapshot(
    flux_flag_count(slopes30lin_flag)
  )
})
