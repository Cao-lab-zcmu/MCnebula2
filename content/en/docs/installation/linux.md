---
contributors:
- LiChuang Huang
title: "Linux"
date: "2023 Mar 23 15:33:01 | Thu"
lastmod: "2023 Mar 23 15:33:01 | Thu"
draft: false
images: []
menu:
  docs:
    parent: "installation"
weight: 10
toc: true
---



The following installation has been tested in Pop!_OS 20.04 (Ubuntu 20.04).

### Install R

We have tested MCnebula in R version 4.2 and 4.1. Maybe R version ≥ 4.0 is feasible.
Herein, the codes are giving an example with installing R 4.2.
In bash:


```bash
sudo apt-key adv \
  --keyserver keyserver.ubuntu.com \
  --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
sudo add-apt-repository \
  "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt-get update
sudo apt install --no-install-recommends r-base
```

Or you can install R with latest version from [CRAN](https://cran.r-project.org/).

## Install dependences for system

The following is an example of installing R packages from github with
`devtools`.  If you use `remotes`, maybe some libraries can be installed
without it.  In bash:


```bash
## Libraries for installing 'usethis' and 'devtools'.
sudo apt install libssl-dev libcurl4-openssl-dev libblas-dev \
  liblapack-dev libgfortran-11-dev
## Libraries for installing 'BiocManager' and its some packages.
sudo apt install libnetcdf-dev libopenbabel-dev libeigen3-dev
## Libraries For installing other graphic packages.
sudo apt install libfontconfig1-dev librsvg2-dev libmagick++-dev
```

We are not sure how other Linux distributions will differ in terms of
pre-installation of Libraries. If dependencies are missing from the
installation in the following sections, then it may be that the relevant
Libraries are missing from the system, please follow the prompts to install
them on Bash (or other shell).

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

We provide additional tools with `exMCnebula2` that may be irrelevant for
the experienced cheminformatician.
The installation of `MetaboAnalystR` may not be so straight forward, please refer to:
(<https://github.com/xia-lab/MetaboAnalystR>)

```r
BiocManager::install(c("BiocParallel", "FELLA", "XCMS"))
## For tools query chemical classification via ClassyFire API
remotes::install_github('aberHRML/classyfireR')
## For tools convert CID to KEGG ID
remotes::install_github('xia-lab/MetaboAnalystR')
```

Then install it:

```r
remotes::install_github("Cao-lab-zcmu/exMCnebula2", "light")
```

If you want to repeat the analysis in [workflow](/docs/workflow/), please
install that via (this would also download the inst data):

```r
remotes::install_github("Cao-lab-zcmu/exMCnebula2")
```

