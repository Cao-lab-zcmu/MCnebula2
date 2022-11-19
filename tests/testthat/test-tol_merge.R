test_that("tol_merge works", {
            set.seed(1)
            main <- data.frame(mz = rnorm(10, 5, 1))
            sub <- main
            sub$mz[1:5] <- sub$mz[1:5] + 1
            sub$mz[6:10] <- sub$mz[6:10] + 0.001
            sub$rt <- 2
            df <- tol_merge(main, sub)
            expect_equal(nrow(df), 10)
            expect_true(any((df$mz %in% main$mz) & (main$mz %in% df$mz)))
})
