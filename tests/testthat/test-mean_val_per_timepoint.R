test_that("calculation for non-zero values works", {
  x <- data.frame(days_from_baseline = c(0, 0, 27, 27),
                  MAF = c(0.009, 0.007, 0.012, 0.032))
  y <- tibble::tibble(days_from_baseline = c(0, 27),
                  Mean_MAF = c(0.008, 0.022))
  expect_equal(mean_val_per_timepoint(x, 'days_from_baseline', 'MAF'), y)
})

test_that("calculation for zero-only values works", {
  x <- data.frame(days_from_baseline = c(0, 0, 27, 27),
                  MAF = c(0, 0, 0, 0))
  y <- tibble::tibble(days_from_baseline = c(0, 27),
                      Mean_MAF = c(0, 0))
  expect_equal(mean_val_per_timepoint(x, 'days_from_baseline', 'MAF'), y)
})

test_that("calculation for a mix of zero and non-zero values works", {
  x <- data.frame(days_from_baseline = c(0, 0, 27, 27),
                  MAF = c(0, 0.009, 0, 0.005))
  y <- tibble::tibble(days_from_baseline = c(0, 27),
                      Mean_MAF = c(0.009, 0.005))
  expect_equal(mean_val_per_timepoint(x, 'days_from_baseline', 'MAF'), y)
})

test_that("calculation including NAs works", {
  x <- data.frame(days_from_baseline = c(0, 0, 27, 27),
                  MAF = c(0, NA, 0, 0.005))
  y <- tibble::tibble(days_from_baseline = c(0, 27),
                      Mean_MAF = c(0, 0.005))
  expect_equal(mean_val_per_timepoint(x, 'days_from_baseline', 'MAF'), y)
})

test_that("calculation including strings works", {
  x <- data.frame(days_from_baseline = c(0, 0, 27, 27),
                  MAF = c(0, '0.009', 0, 0.005))
  y <- tibble::tibble(days_from_baseline = c(0, 27),
                      Mean_MAF = c(0.009, 0.005))
  expect_equal(mean_val_per_timepoint(x, 'days_from_baseline', 'MAF'), y)
})
