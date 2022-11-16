test_that("basic workflow works", {
            test <- mcn_5features

            test1 <- filter_structure(test)
            expect_equal(nrow(latest(test1)), 5)

            test1 <- create_reference(test1)
            expect_s3_class(specific_candidate(test1), "tbl")

            test1 <- filter_formula(test1, by_reference = T)
            expect_equal(nrow(latest(test1)), 5)

            test1 <- create_stardust_classes(test1)
            expect_s3_class(stardust_classes(test1), "tbl")

            test1 <- create_features_annotation(test1)
            expect_s3_class(features_annotation(test1), "tbl")

            test1 <- cross_filter_stardust(test1, 2, 1)
            expect_s3_class(stardust_classes(test1), "tbl")

            test1 <- create_nebula_index(test1)
            expect_s3_class(nebula_index(test1), "tbl")

            test1 <- compute_spectral_similarity(test1)
            expect_s3_class(spectral_similarity(test1), "tbl")
            print(class(spectral_similarity(test1)$similarity))
            expect_type(spectral_similarity(test1)$similarity, "double")

            test1 <- create_parent_nebula(test1, 0.01, T)
            expect_s3_class(igraph(parent_nebula(test1)), "igraph")

            test1 <- create_child_nebulae(test1, 0.01, 5)
            expect_s3_class(igraph(child_nebulae(test1))[[1]], "igraph")
            expect_equal(length(unique(nebula_index(test1)$class.name)),
                         length(igraph(child_nebulae(test1)))
            )

            test1 <- create_parent_layout(test1)
            expect_s3_class(layout_ggraph(parent_nebula(test1)), "layout_ggraph")

            test1 <- create_child_layouts(test1)
            expect_s3_class(layout_ggraph(child_nebulae(test1))[[1]], "layout_ggraph")

            test1 <- activate_nebulae(test1)
            expect_s4_class(ggset(parent_nebula(test1)), "ggset")
            expect_s3_class(call_command(ggset(parent_nebula(test1))), "ggplot")
            expect_s3_class(visualize(test1, 1), "ggplot")
})
