# MCnebula2

**News:** A comprehensive introduction to the MCnebula workflow and R package
'MCnebula2': <https://mcnebula.netlify.app/>.

MCnebula algorithm integrated in S4 system of R.
Use `help(MCnebula2)` to see more details (after installation).

## With SIRIUS version 6

In SIRIUS (v6), the data storage format has changed significantly. In short,
the project workspace can no longer be accessed as a regular folder; instead,
it is stored as a binary file with the `.sirius` extension. Standard file-based
approaches can no longer be used to retrieve data. Fortunately, the SIRIUS team
provides an API R package (see:
[https://github.com/sirius-ms/sirius-client-openAPI](https://github.com/sirius-ms/sirius-client-openAPI))
for accessing the data.

### Dependency package

You need to install `RSirius` to extract data from `.sirius` files.
(As long as this R package works properly, `MCnebula2` will be able to
automatically retrieve the required data.)
(The installation refer to: <https://github.com/sirius-ms/sirius-client-openAPI/tree/master/client-api_r>)

```r
remotes::install_github(
  repo = "sirius-ms/sirius-client-openAPI",
  subdir = "client-api_r/generated", 
  ref = "master", build = TRUE
)
```

### Usage

The only difference from version 5 and earlier is that, before initializing the
`mcnebula` object, you must first initialize the API. This can be done with a
simple command:

```r
api <- initialize_sirius_api("/path/to/sirius", port = 8080L)
```

Note: Replace `"/path/to/sirius"` with the path to your SIRIUS executable.
Based on testing, the SIRIUS API may require you to be logged in at least once before it works properly.
(In general, if SIRIUS runs correctly on your system and RSirius is successfully
installed, data access should work well.)

Once `initialize_sirius_api` runs successfully, you can use `MCnebula2` as usual. For example:

```r
# The only additional step
# NOTE: Please replace "/path/to/bin/sirius" with your SIRIUS excusable file
api <- initialize_sirius_api("/path/to/bin/sirius")

# Usage remains the same as before
mcn <- mcnebula()
# NOTE: Please replace "/path/to/file.sirius" with your '.sirius' file
mcn <- initialize_mcnebula(mcn, "sirius.v6", "/path/to/file.sirius")

mcn <- filter_structure(mcn)
mcn <- create_reference(mcn)

mcn <- filter_formula(mcn, by_reference = TRUE)
mcn <- filter_ppcp(mcn, by_reference = TRUE)

mcn <- create_stardust_classes(mcn)
mcn <- create_features_annotation(mcn)
mcn <- cross_filter_stardust(mcn, 5, .3)
mcn <- create_nebula_index(mcn)

mcn <- compute_spectral_similarity(mcn)

mcn <- create_parent_nebula(mcn, 0.01, 5, FALSE)
mcn <- create_child_nebulae(mcn, 0.01, 5)

mcn <- create_parent_layout(mcn)
mcn <- create_child_layouts(mcn)
mcn <- activate_nebulae(mcn)

```

## Installation

In general, the following code will install the dependent or imported packages
as well as the MCnebula2 package:

```r
remotes::install_github("Cao-lab-zcmu/MCnebula2")
```

However, packages that do not exist in CRAN but exist in Bioconductor may not be installed
automatically, in which case they may need to be installed first:

```r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
  install.packages("BiocManager")
}
BiocManager::install(c("BiocStyle", "ChemmineOB"))
```
If you are using Mac ARM64, please install open-babel first.

```zsh
brew install open-babel
```


