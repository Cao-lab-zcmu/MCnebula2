
rda <- paste0(tmp <- tempdir(), "/toActiv30.rdata")
bin <- RCurl::getURLContent("https://raw.githubusercontent.com/Cao-lab-zcmu/utils_tool/master/inst/extdata/toActiv30.rdata")
writeBin(bin, rda)

load(rda)
mcn <- toActiv30

mcn <- filter_structure(mcn)
mcn <- create_reference(mcn)
mcn <- filter_formula(mcn, by_reference = T)

mcn <- create_stardust_classes(mcn)
mcn <- create_features_annotation(mcn)
mcn <- cross_filter_stardust(mcn, 5, .7)

mcn <- MCnebula2:::.simulate_quant_set(mcn)
mcn <- binary_comparison(mcn, model - control)

ids <- sample(features_annotation(mcn)$.features_id, 8)
mcn <- draw_structures(mcn, .features_id = ids)
# plot_msms_mirrors(mcn, ids)

mcn <- create_nebula_index(mcn)
mcn <- compute_spectral_similarity(mcn)
mcn <- create_parent_nebula(mcn, 0.01, 5, T)
mcn <- create_child_nebulae(mcn, 0.01, 5)

mcn <- create_parent_layout(mcn)
mcn <- create_child_layouts(mcn)
mcn <- activate_nebulae(mcn)

visualize_all(mcn)

p <- visualize(mcn, 'parent')
ggsave("parent_nebula.svg", p, height = 5)

svg("child_nebulae.svg", 9, 9)
visualize_all(mcn)
dev.off()

top_data <- top_table(statistic_set(mcn))[[1]]
top_data

tops <- top_data$.features_id[1:5]
mcn2 <- set_tracer(mcn, tops)

mcn2 <- create_child_nebulae(mcn2)
mcn2 <- create_child_layouts(mcn2)
mcn2 <- activate_nebulae(mcn2)
mcn2 <- set_nodes_color(mcn2, use_tracer = T)

svg("tracer_child_nebulae.svg")
visualize_all(mcn2)
dev.off()


save(mcn, file = "demo.rdata")
