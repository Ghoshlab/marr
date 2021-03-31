library(testthat)
context("checking the marr package")

library(marr)

test_that("checking dimensions of Marr object", {
        n_features <- 1200
        n_samples <- 20
        set.seed(3456)
        data_test <- matrix(rnorm(n_features * n_samples),
        n_features, n_samples)
        Marr_test <- Marr(object = data_test)

        expect_equal(nrow(MarrSamplepairs(Marr_test)),
        choose(n_samples,2))
        expect_equal(nrow(MarrFeatures(Marr_test)), n_features)
})

test_that("checking Marr data", {
        n_features <- 1200
        n_samples <- 20
        set.seed(3456)
        data_test <- matrix(rnorm(n_features * n_samples),
        n_features, n_samples)
        Marr_test <- Marr(object = data_test)

        expect_equal(MarrSamplepairs(Marr_test)$reproducibility[184], 
                     0.08333333,
                     tolerance = 1e-06)
        expect_equal(MarrFeatures(Marr_test)$reproducibility[879], 0.5263158,
        tolerance = 1e-06)
})
