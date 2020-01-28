README
================
David J. Winkel
1/13/2020

``` r
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readr)
library(xlsx)
library(readxl)
library(XLConnect)
library(ggpubr)
library(caret)
library(dplyr)        
library(pROC)
library(ROCR)
library(factoextra)
library(tictoc)
```

This is the README file for the experiment. The concepts and information presented in this repository are based on research results that are not commercially available.

``` r
sessionInfo()
```

    ## R version 3.6.0 (2019-04-26)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 10 x64 (build 17134)
    ## 
    ## Matrix products: default
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United States.1252 
    ## [2] LC_CTYPE=English_United States.1252   
    ## [3] LC_MONETARY=English_United States.1252
    ## [4] LC_NUMERIC=C                          
    ## [5] LC_TIME=English_United States.1252    
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] tictoc_1.0           factoextra_1.0.6     ROCR_1.0-7          
    ##  [4] gplots_3.0.1.2       pROC_1.16.1          caret_6.0-85        
    ##  [7] lattice_0.20-38      ggpubr_0.2.4         magrittr_1.5        
    ## [10] XLConnect_0.2-15     XLConnectJars_0.2-15 readxl_1.3.1        
    ## [13] xlsx_0.6.1           forcats_0.4.0        stringr_1.4.0       
    ## [16] dplyr_0.8.3          purrr_0.3.3          readr_1.3.1         
    ## [19] tidyr_1.0.0          tibble_2.1.3         tidyverse_1.3.0     
    ## [22] ggplot2_3.2.1       
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] nlme_3.1-139         bitops_1.0-6         fs_1.3.1            
    ##  [4] lubridate_1.7.4      httr_1.4.1           tools_3.6.0         
    ##  [7] backports_1.1.5      R6_2.4.1             KernSmooth_2.23-15  
    ## [10] rpart_4.1-15         DBI_1.1.0            lazyeval_0.2.2      
    ## [13] colorspace_1.4-1     nnet_7.3-12          withr_2.1.2         
    ## [16] tidyselect_0.2.5     compiler_3.6.0       cli_2.0.1           
    ## [19] rvest_0.3.5          xml2_1.2.2           caTools_1.17.1.4    
    ## [22] scales_1.1.0         digest_0.6.23        rmarkdown_2.0       
    ## [25] pkgconfig_2.0.3      htmltools_0.4.0      dbplyr_1.4.2        
    ## [28] rlang_0.4.2          rstudioapi_0.10      generics_0.0.2      
    ## [31] jsonlite_1.6         gtools_3.8.1         ModelMetrics_1.2.2.1
    ## [34] Matrix_1.2-17        Rcpp_1.0.3           munsell_0.5.0       
    ## [37] fansi_0.4.1          lifecycle_0.1.0      stringi_1.4.4       
    ## [40] yaml_2.2.0           MASS_7.3-51.4        plyr_1.8.5          
    ## [43] recipes_0.1.9        grid_3.6.0           ggrepel_0.8.1       
    ## [46] gdata_2.18.0         crayon_1.3.4         haven_2.2.0         
    ## [49] splines_3.6.0        xlsxjars_0.6.1       hms_0.5.3           
    ## [52] zeallot_0.1.0        knitr_1.27           pillar_1.4.3        
    ## [55] ggsignif_0.6.0       reshape2_1.4.3       codetools_0.2-16    
    ## [58] stats4_3.6.0         reprex_0.3.0         glue_1.3.1          
    ## [61] evaluate_0.14        data.table_1.12.8    modelr_0.1.5        
    ## [64] vctrs_0.2.1          foreach_1.4.7        cellranger_1.1.0    
    ## [67] gtable_0.3.0         assertthat_0.2.1     xfun_0.12           
    ## [70] gower_0.2.1          prodlim_2019.11.13   broom_0.5.3         
    ## [73] class_7.3-15         survival_2.44-1.1    timeDate_3043.102   
    ## [76] rJava_0.9-11         iterators_1.0.12     lava_1.6.6          
    ## [79] ipred_0.9-9
