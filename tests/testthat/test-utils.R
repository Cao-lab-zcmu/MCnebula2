test_that("multiplication works", {
            x <- data.frame(x = 1:5, y = 1:5, z = 1:5)
            y <- data.frame(x = 1:5, y = 1:5, z = 1:5, a = 1:5)
            df <- checkColMerge(x, y, by = "x", all = T)
            expect_equal(ncol(df), 4)
})
