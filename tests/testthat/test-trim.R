test_that("Limpia Bien", {
  expect_equal(trim("   abc  "),"abc")
  expect_error(trim(123))
})
devtools::test_coverage_file()