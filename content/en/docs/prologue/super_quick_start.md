---
contributors:
- LiChuang Huang
title: "Super quick start"
date: "2023 Mar 28 13:41:37 | Tue"
lastmod: "2023 Mar 28 13:41:37 | Tue"
draft: false
images: []
menu:
  docs:
    parent: "prologue"
weight: 40
toc: true
---



To quickly demonstrate the application of the 'MCnebula2' R package, we will
then use a data extracted from a finished SIRIUS project (using the
`collate_used` function) to demonstrate its use. The following demonstration can
be repeated in R as long as the user has installed the MCnebula2 R package (and
possibly some small R package, such as `RCurl`).

## Set-up


```r
library(MCnebula2)
if (!requireNamespace("RCurl")) {
  install.packages("RCurl")
}
```

## Download and load the demo data

**Note:** The following object (`toActiv30`) just contain 30 Features.
However, the real untargeted LC-MS/MS dataset could hardly be such a small
amount of Features. We collected this dataset only to demonstrate the
application of the MCnebula2 R package, please do not apply the parameters
involved below in real data analysis (for application of the parameters, please
refer to workflow: [Metabolic workflow](/docs/workflow/metabolic_workflow/);
[Chemcal workflow](/docs/workflow/chemical_workflow/); or use `help()` with
your confused functions or methods).


```r
rda <- paste0(tmp <- tempdir(), "/toActiv30.rdata")
bin <- RCurl::getURLContent("https://raw.githubusercontent.com/Cao-lab-zcmu/utils_tool/master/inst/extdata/toActiv30.rdata")
writeBin(bin, rda)

load(rda)
mcn <- toActiv30
```

## Filter candidates and integrate data

All basic workflow was run as following.
Again, the parameters used in following was just with the purpose of
demonstration, do not applied these in real analysis.


```r
mcn <- filter_structure(mcn)
mcn <- create_reference(mcn)
mcn <- filter_formula(mcn, by_reference = T)

mcn <- create_stardust_classes(mcn)
mcn <- create_features_annotation(mcn)
mcn <- cross_filter_stardust(mcn, 5, .7)

mcn <- filter_structure(mcn)
mcn <- create_reference(mcn)
mcn <- filter_formula(mcn, by_reference = T)

mcn <- create_stardust_classes(mcn)
mcn <- create_features_annotation(mcn)
mcn <- cross_filter_stardust(mcn, 5, .7)
```

## Visualize Parent-Nebula and Child-Nebula

Drawing these Nebula and saved as .svg.

### Parent-Nebula

For Parent-Nebula:


```r
p <- visualize(mcn, 'parent')
ggsave("parent_nebula.svg", p, height = 4)
```

<figure>
<center><img src="/docs/prologue/parent_nebula.svg"></center>
<center><figcaption>Demonstration of Parent-Nebula (Only 30 Features)</figcaption></center>
</figure>

### Child-Nebulae

For Child-Nebulae:


```r
svg("child_nebulae.svg", 9, 9)
visualize_all(mcn)
dev.off()
```

<figure>
<center><img src="/docs/prologue/child_nebulae.svg"></center>
<center><figcaption>Demonstration of Child-Nebula</figcaption></center>
</figure>

## Quantification analysis

These was no quantification data in object `mcn`.
So we need to simulate a quantification dataset into that.
A predefined function could done this well:


```r
mcn <- MCnebula2:::.simulate_quant_set(mcn)
```

Let's check what we have simulated:


```r
sample_metadata(mcn)
```

```
## # A tibble: 20 × 2
##    sample    group  
##    <chr>     <chr>  
##  1 control_1 control
##  2 control_2 control
##  3 control_3 control
##  4 control_4 control
##  5 control_5 control
##  6 model_1   model  
##  7 model_2   model  
##  8 model_3   model  
##  9 model_4   model  
## 10 model_5   model  
## 11 treat_1   treat  
## 12 treat_2   treat  
## 13 treat_3   treat  
## 14 treat_4   treat  
## 15 treat_5   treat  
## 16 pos_1     pos    
## 17 pos_2     pos    
## 18 pos_3     pos    
## 19 pos_4     pos    
## 20 pos_5     pos
```

```r
features_quantification(mcn)
```

```
## # A tibble: 30 × 21
##    .features_id control_1 control_2 control_3 control_4 control_5 model_1
##    <chr>            <dbl>     <dbl>     <dbl>     <dbl>     <dbl>   <dbl>
##  1 202               29.1      57.4      18.5      60.5      46.8    27.5
##  2 2020              32.9      63.0      56.6      63.9      69.9    66.1
##  3 2022              73.8      23.6      31.9      45.5      41.5    29.5
##  4 2023              42.6      70.2      71.8      39.1      10.7    64.4
##  5 2024              43.9      78.6      77.4      75.6      58.7    34.8
##  6 2025              24.5      37.2      58.7      25.7      15.6    48.1
##  7 2026              89.8      48.4      52.3      68.2      24.3    57.3
##  8 2027              80.5      66.7      62.1      52.5      48.3    52.2
##  9 2028              55.3      91.3      73.6      76.4      74.1    28.7
## 10 2029              56.6      30.9      15.6      34.6      42.6    21.1
## # … with 20 more rows, and 14 more variables: model_2 <dbl>, model_3 <dbl>,
## #   model_4 <dbl>, model_5 <dbl>, treat_1 <dbl>, treat_2 <dbl>, treat_3 <dbl>,
## #   treat_4 <dbl>, treat_5 <dbl>, pos_1 <dbl>, pos_2 <dbl>, pos_3 <dbl>,
## #   pos_4 <dbl>, pos_5 <dbl>
```

### Statistic analysis

Compare two groups: Model vs Control.


```r
mcn <- binary_comparison(mcn, model - control)
top_data <- top_table(statistic_set(mcn))
top_data
```

```
## $`model - control`
## # A tibble: 30 × 7
##    .features_id  logFC AveExpr     t  P.Value adj.P.Val      B
##    <chr>         <dbl>   <dbl> <dbl>    <dbl>     <dbl>  <dbl>
##  1 2071         -1.76  -0.183  -3.75 0.000630    0.0162 -0.426
##  2 2025          1.22   0.0974  3.56 0.00108     0.0162 -0.919
##  3 2097          1.02  -0.0853  2.85 0.00731     0.0731 -2.66 
##  4 2096         -0.993 -0.0310 -2.56 0.0149      0.112  -3.29 
##  5 209           0.900 -0.0986  2.42 0.0209      0.126  -3.59 
##  6 2095         -0.704 -0.104  -1.82 0.0773      0.386  -4.69 
##  7 2092          0.894 -0.189   1.50 0.142       0.610  -5.18 
##  8 2023          0.602  0.120   1.34 0.188       0.658  -5.39 
##  9 2099          0.575 -0.126   1.25 0.221       0.658  -5.50 
## 10 2029          0.474 -0.0430  1.19 0.242       0.658  -5.57 
## # … with 20 more rows
```

### Tracing top features in Child-Nebulae


