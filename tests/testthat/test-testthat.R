library(testthat)
context("checking the marr function")

library(marr)

test_that("checking dimensions of marr object", {
        n_features <- 1200
        n_samples <- 20
        set.seed(3456)
        data_test <- matrix(rnorm(n_features * n_samples),
        n_features, n_samples)
        marr_test <- marr(object = data_test)

        expect_equal(length(marrSamplepairs(marr_test)),
        choose(n_samples,2))
        expect_equal(length(marrFeatures(marr_test)), n_features)
})

test_that("checking marr data", {
        n_features <- 1200
        n_samples <- 20
        set.seed(3456)
        data_test <- matrix(rnorm(n_features * n_samples),
        n_features, n_samples)
        marr_test <- marr(object = data_test)

        expect_equal(marrSamplepairs(marr_test)[184], 0.08333333,
        tolerance = 1e-06)
        expect_equal(marrFeatures(marr_test)[879], 0.5263158,
        tolerance = 1e-06)
})
