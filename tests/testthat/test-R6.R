context("R6")

test_that("R6 methods coverage is reported", {
  cov <- as.data.frame(package_coverage("TestR6"))
  expect_equal(cov$value, c(5, 2, 3, 2, 3, 0, 1, 2, 1, 1, 2, 2, 2, 2))
  expect_equal(cov$first_line, c(5, 6, 8, 16, 19, 27, 37, 46, 47, 49, 56, 57, 61, 62))
  expect_equal(cov$last_line, c(5, 6, 8, 16, 19, 27, 37, 46, 47, 49, 56, 60, 61, 62))
  expect_true("some_method" %in% cov$functions)
  expect_true("class_switch" %in% cov$functions)
})
