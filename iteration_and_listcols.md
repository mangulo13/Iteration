Iteration and list columns
================

``` r
library(tidyverse)
```

    ## -- Attaching packages ---------------------- tidyverse 1.3.0 --

    ## v ggplot2 3.3.2     v purrr   0.3.4
    ## v tibble  3.0.3     v dplyr   1.0.2
    ## v tidyr   1.1.2     v stringr 1.4.0
    ## v readr   1.3.1     v forcats 0.5.0

    ## -- Conflicts ------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)


theme_set(theme_minimal()+theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colur = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

You can put anything in a list.

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(T, T, F,T, F, F),
  mat = matrix(1:8, nrow = 2, ncol = 4),
  summary = summary(rnorm(100))
)

l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $vec_logical
    ## [1]  TRUE  TRUE FALSE  TRUE FALSE FALSE
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -2.05742 -0.63232  0.16363  0.06898  0.65803  2.66773

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## ‘for’ loop

Create a new list.

``` r
 list_norm = 
  list(
    a = rnorm(20, mean  = 3, sd = 1),
    b = rnorm(30, mean  = 0, sd = 5),
    c = rnorm(40, mean  = 10, sd = 0.2),
    d = rnorm(20, mean  = -3, sd = 1)
  )
```

``` r
list_norm
```

    ## $a
    ##  [1] 1.966060 3.984202 2.899489 2.343834 1.148964 4.466643 2.756161 2.917984
    ##  [9] 2.286053 2.923115 2.874689 3.065102 2.604689 1.663139 1.796658 2.455187
    ## [17] 2.741299 4.997197 3.153506 4.701858
    ## 
    ## $b
    ##  [1] -2.5982206  7.3187842  2.1123997  8.6074309 -4.5662143 -8.1382622
    ##  [7] -1.3689697  9.3271799  4.6782589  4.3054746 -5.4736729  6.6710986
    ## [13] -4.9645411 -6.8601864 -1.9700073  4.7204882  9.4139194  0.0315575
    ## [19]  3.0315062 -3.9077961  2.5747744  0.3322160  5.2520756 -2.6206999
    ## [25]  1.4862183 -5.2496231 -3.2078158  7.3251327  0.2176860  4.6513836
    ## 
    ## $c
    ##  [1]  9.942555  9.732013  9.949823  9.757623  9.964058 10.001032  9.914115
    ##  [8]  9.870049 10.018050  9.954770 10.227169  9.996053 10.258754 10.314423
    ## [15]  9.695913 10.063437 10.014862  9.930834 10.249031 10.032501  9.861931
    ## [22]  9.937706 10.156478 10.354727 10.144552 10.170645  9.705862 10.095076
    ## [29] 10.151556 10.021090  9.916652 10.329596  9.963929  9.698759 10.065087
    ## [36] 10.019803  9.899272 10.238791  9.672177 10.174429
    ## 
    ## $d
    ##  [1] -4.1019578 -1.3454298 -3.6686465 -3.8449990 -2.1846814 -3.1888917
    ##  [7]  0.2126618 -4.0678137 -1.5867475 -2.7671282 -3.4756377 -4.2513240
    ## [13] -2.8881290 -3.6542456 -0.4633643 -3.7176126 -3.7892227 -2.6700958
    ## [19] -1.2367504 -3.2005095

Pause and get my old function.

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("Input must be numeric")
  }
  
  if(length(x) < 3) {
    stop("Input must have at least three numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  tibble(
    mean = mean_x,
    sd = sd_x
  )
  
}
```

I can apply that funciton to each list element

``` r
mean_and_sd(list_norm[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.89  1.00

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.04  5.14

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.183

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.79  1.29

But this is really lame that we do the same thing and just change input
each time

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```

## Let’s try map\!

``` r
output = map(list_norm, mean_and_sd)
```

What if we want a different function..?

``` r
output = map(list_norm, IQR)
```

Good example that you can write functions within functions.
