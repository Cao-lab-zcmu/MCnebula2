# MCnebula2

**News:** A comprehensive introduction to the MCnebula workflow and R package
'MCnebula2': <https://mcnebula.netlify.app/>.

MCnebula algorithm integrated in S4 system of R.
Use `help(MCnebula2)` to see more details (after installation).

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


