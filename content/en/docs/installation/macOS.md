---
contributors:
- LiChuang Huang
title: "MacOS"
date: "2023 Mar 23 15:33:01 | Thu"
lastmod: "2023 Mar 23 15:33:01 | Thu"
draft: false
images: []
menu:
  docs:
    parent: "installation"
weight: 20
toc: true
---



## Install R

Following the instructions in the installing pages of MacOS to install R:
<https://cran.r-project.org/>

## Install dependences for system

`MCnebula2` requires the R package `ChemmineOB` for visualizing chemical
structures. In our debugging, `ChemmineOB` needs to be pre-installed on MacOS
with 'Open Babel' so that it can be used.

### (Option) Install Open Babel via 'Homebrew'

Please check the official website for detailed guidelines about installation:
<https://github.com/Homebrew/install>.
The following is the copied command for installing that:


```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

For **Chinese people**, the following command can be used if there is some trouble
in network connection:


```bash
/bin/bash -c "$(curl -fsSL https://gitee.com/cunkai/HomebrewCN/raw/master/Homebrew.sh)"
```

After the 'Homebrew' has been installed, restart the terminal, and then:


```bash
## config the github first, so that the `git` can be run successfully.
git config --global --add safe.directory /opt/homebrew/Library/Taps/homebrew/homebrew-core
git config --global --add safe.directory /opt/homebrew/Library/Taps/homebrew/homebrew-cask
brew install open-babel
```

### (Option) Other methods

See <http://openbabel.org/wiki/Main_Page> for guidelines.

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
