---
contributors:
- LiChuang Huang
title: "Basic workflow"
date: "2023 Mar 23 15:35:03 | Thu"
lastmod: "2023 Mar 23 15:35:03 | Thu"
draft: false
images: []
menu:
  docs:
    parent: "workflow"
weight: 150
toc: true
---



About 
[script of generating report of Basic
workflow](/docs/workflow/basic_workflow/#option-workflow-mode)

## Initialize object

Set SIRIUS project path and its version to initialize mcnebula object.


```r
## The `path` is where your SIRIUS project saved.
path <- "."
mcn <- mcnebula()
mcn <- initialize_mcnebula(mcn, "sirius.v5", path)
## "pos" or "neg"
ion_mode(mcn) <- "pos"
```

## Filter candidates

### Filter molecular formula and chemical structure

According to the top chemical structure to build `specific_candidate` of `reference`.
[Learn more](/docs/prologue/introduction/#chemical-structure-and-formula)


```r
mcn <- filter_structure(mcn)
mcn <- create_reference(mcn)
mcn <- filter_formula(mcn, by_reference = T)
```

- Tips:
    1. Use `dplyr::*` in `filter_structure` to customize the filtering,
    such as: `filter_structure(mcn, dplyr::filter, tani.score >= .5)`. This
    is similar in `filter_formula` or `filter_ppcp`.

- Tips (after above): 
    1. Use `latest(mcn, subscript = ".f3_fingerid")` to get the filtered
    candidates of the chemical structure (Top candidate for each feature)
    2. Use `latest(mcn, "project_dataset", subscript = ".f3_fingerid")` to
    get all candidates of all features
    3. Use `reference(mcn)$specific_candidate` to get the selected
    `specific_candidate`
    4. Use `latest(mcn, subscript = ".f2_formula")` to get the filtered
    candidates of the molecular formula (Top candidate for each feature)
    5. Use `latest(mcn, "project_dataset", subscript = ".f2_formula")` to
    get all candidates of all features


### Filter chemical classification

[Learn more](/docs/prologue/introduction/#chemical-classification)
about chemical classification.


```r
mcn <- create_stardust_classes(mcn)
mcn <- create_features_annotation(mcn)
mcn <- cross_filter_stardust(
  mcn, max_ratio = .05,
  cutoff = .4, identical_factor = .6
)
```

- Tips (after above):
    1. Use `stardust_classes(mcn)` to get the data about 'stardust_classes'
    2. Use `features_annotation(mcn)` to get the data about ...
    3. Use `cross_filter_stardust()` to get the default parameters about this methods.

### (Option) Manually check the filtered chemical classes

Get the kept classes or the filtered out classes.


```r
## The kept classes
classes <- unique(stardust_classes(mcn)$class.name)
## The filtered out classes
table.filtered.classes <- backtrack_stardust(mcn)
```

### (Option) Manually filter the chemical classes

Manually filter some repetitive classes or sub-structural classes.
By means of Regex matching, we can obtain a number of recurring
name of chemical classes that would contain manay identical compounds
as their sub-structure.


```r
## Regex match
pattern <- c("stero", "fatty acid", "pyr", "hydroxy", "^orga")
dis <- unlist(lapply(pattern, grep, x = classes, ignore.case = T))
dis <- classes[dis][-1]
## Remove these classes in `stardust_classes(mcn)`
mcn <- backtrack_stardust(mcn, dis, remove = T)
```

- Tips:
    1. Use `backtrack_stardust(mcn, dis)` without `remove` to recover these classes.

## Create Nebula-Index

After following, whether it is all filtered by the algorithm provided by
MCnebula2's function or custom filtered for some chemical classes, we now have
a data called 'nebula_index'.  This data records a number of chemical classes
and the 'features' attributed to them.  The subsequent analysis process or
visualization will be based on it.


```r
mcn <- create_nebula_index(mcn)
```

- Tips (after above):
    1. Use `nebula_index(mcn)` to get the data ...

## Create Nebulae

### Compute spectral similarity

See `help(compute_spectral_similarity)` about the algorithm.


```r
mcn <- compute_spectral_similarity(mcn)
```

### Create Parent-Nebula

Create network data for Parent-Nebula.


```r
mcn <- create_parent_nebula(mcn)
mcn <- create_parent_layout(mcn)
```

- Tips (after above): 
    1. Use function of `igraph::write_graph` to output .graphml (e.g., use for Cytoscape),
    such as: `igraph::write_graph(igraph(parent_nebula(mcn)), "test.graphml", "graphml")`
    2. Use `parent_nebula(mcn)` to get the data ...
    3. Use `igraph(parent_nebula(mcn))` to get the data ...
    4. ...

### Create Child-Nebulae

Create network data for Child-Nebulae.


```r
mcn <- create_child_nebulae(mcn)
mcn <- create_child_layouts(mcn)
```

- Tips (after above): 
    1. See `help(create_child_nebulae)` for example about how to export
    '.graphml' of all Child-Nebulae.

### Activate the Nebulae for visualization

This would create 'ggset' about functions and parameters for ggplot2
to visualize the Nebulae.


```r
mcn <- activate_nebulae(mcn)
```

- Tips (after above): 
    1. Use function of `get_ggset` to get the 'ggset' of Nebulae. Such as:
    `get_ggset(mcn, 'parent', modify_set_labs)`.
    2. Use method of `call_command` to visualize 'ggset', such as:
    `call_command(get_ggset(mcn, 'parent', modify_set_labs))`.

## Visualize Nebulae

### Visualize Parent-Nebula


```r
visualize(mcn, "parent")
```

### Visualize Child-Nebulae


```r
## Visualize the specific item (number or classes name)
visualize(mcn, 1)
## Visualize all into one plot
visualize_all(mcn)
```

## (Option) Workflow Mode

Use the following command to get the code that outputs the full report
containing the above analysis. You can modify the parameters on the basis of
the output code to fit your data or your needs.


```r
workflow(mode = "print")
```

- Tips: Using `sink()` to catch the text output in the command lines, such as:


```r
file <- tempfile(fileext = ".R")
sink(file)
workflow(mode = "print")
sink()
```
