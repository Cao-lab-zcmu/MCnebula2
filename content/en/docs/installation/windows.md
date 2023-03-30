---
contributors:
- LiChuang Huang
title: "Windows"
date: "2023 Mar 23 15:33:01 | Thu"
lastmod: "2023 Mar 23 15:33:01 | Thu"
draft: false
images: []
menu:
  docs:
    parent: "installation"
weight: 30
toc: true
---



Thanks to 'Rtools', it is probably the easiest to configure an R environment in
Windows.

## Install R

Please install R via 'Rtools' (<https://cran.r-project.org/>).

## Install R packages from 'Bioconductor'

Installing R packages from Bioconductor:


```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install(c("BiocStyle", "ChemmineOB"))
```

## Install MCnebula2 from github

Use `devtools` or `remotes` to install `MCnebula2`:

```r
if (!requireNamespace("remotes", quietly = TRUE))
    install.packages("remotes")
remotes::install_github("Cao-lab-zcmu/MCnebula2")
```

## (Option) Install exMCnebula2 from github 

Please refer to The instructions of Linux.
<a href="/docs/installation/linux/#install-exmcnebula2-from-github-optional">Click here</a>
