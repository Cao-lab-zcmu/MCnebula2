
test_that("compareSpectra works", {
            df <- data.frame(x = 1:10, y = 1:10)
            value <- compute_spectral_similarity(sp1 = df, sp2 = df)
            expect_equal(value, 1)
})
