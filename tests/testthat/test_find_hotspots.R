test_that("drop_effort_cols returns correct type", {
  expect_s3_class(drop_effort_cols(sample_prob_per_loc), "data.frame")
})

test_that("drop_effort_cols reduces the number of columns", {
  expect_lt(ncol(drop_effort_cols(sample_prob_per_loc)), 
            ncol(sample_prob_per_loc))
})

test_that("drop_effort_cols doesn't change number of rows", {
  expect_equal(nrow(drop_effort_cols(sample_prob_per_loc)), 
               nrow(sample_prob_per_loc))
})