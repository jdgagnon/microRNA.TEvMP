
<!-- README.md is generated from README.Rmd. Please edit that file -->

# microRNA.TEvMP

*by **John D. Gagnon*** <br> *University of California, San Francisco*

### Table of Contents

**[Overview](#overview)**<br> **[Installation](#installation)**<br>
**[Usage](#usage)**<br> **[Session info](#session-info)**<br>
**[License](#license)**<br>

## Overview

A shinyapp-based GUI to visualize microRNA expression over time in
response to infection with LCMV. Uses data from Khan, A.A., L.A. Penny,
Y. Yuzefpolskiy, S. Sarkar, and V. Kalia. 2013. MicroRNA-17∼92 regulates
effector and memory CD8 T-cell fates by modulating proliferation in
response to infections. Blood. 121:4473–4483.
<doi:10.1182/blood-2012-06-435412>.

## Installation

1.  If you do not already have R installed, or your version is out of
    date, download and install the latest
    [version](https://cran.r-project.org).

<!-- end list -->

  - Optionally, install the latest version of [RStudio
    Desktop](https://www.rstudio.com/products/rstudio/#Desktop).

<!-- end list -->

2.  Download the package from GitHub

<!-- end list -->

``` r
install.packages("devtools")
devtools::install_github("jdgagnon/microRNA.TEvMP")
```

## Usage

1.  Load the package into the R session.

`library(microRNA.TEvMP)`

2.  To initialize the shiny app, paste the following code in your R
    console and run it.

`microRNA.TEvMP()`

3.  Select one or more microRNAs from the dropdown menu to be
    highlighted.

## Session info

Here is the output of `sessionInfo()` on the system on which this
package was developed:

``` r
sessionInfo()
#> R version 3.5.1 (2018-07-02)
#> Platform: x86_64-apple-darwin17.6.0 (64-bit)
#> Running under: macOS High Sierra 10.13.6
#> 
#> Matrix products: default
#> BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#> LAPACK: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libLAPACK.dylib
#> 
#> locale:
#> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> loaded via a namespace (and not attached):
#>  [1] compiler_3.5.1  backports_1.1.2 magrittr_1.5    rprojroot_1.3-2
#>  [5] tools_3.5.1     htmltools_0.3.6 yaml_2.2.0      Rcpp_0.12.18   
#>  [9] stringi_1.2.4   rmarkdown_1.10  knitr_1.20      stringr_1.3.1  
#> [13] digest_0.6.17   evaluate_0.11
```

<br><br>

## License

[GNU GPL-3.0-or-later](https://www.gnu.org/licenses/gpl.txt)
