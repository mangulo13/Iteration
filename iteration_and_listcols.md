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
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ## -2.0393 -0.8450 -0.2016 -0.1846  0.3393  2.4142

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
    ##  [1] 3.4342911 3.7354118 3.0125642 0.5582982 4.1887045 2.0322835 3.7853414
    ##  [8] 4.3632764 2.5789681 3.2626079 4.1362765 3.3881328 2.9001845 1.9446111
    ## [15] 2.3834604 3.6537117 4.0452444 4.5510604 3.4075943 2.9258354
    ## 
    ## $b
    ##  [1]  2.97737492 11.50353257  4.62908140 -0.46401796  5.96343786 -0.49307580
    ##  [7]  1.95631928 -2.92678554 -9.88273144 -4.27744130  3.76674487 -1.04894359
    ## [13] -4.44189835  1.05831532 -7.27370933  7.55460737  1.63003734  2.00073061
    ## [19]  0.18812167 -5.36438171 -9.46240721 -2.12762461 -0.84819824  6.93458231
    ## [25]  0.13131130  1.01912943 -2.15028857  2.42292916 -2.35679196  0.05283751
    ## 
    ## $c
    ##  [1] 10.298424 10.080063 10.166255 10.122918 10.190546  9.569490 10.076444
    ##  [8] 10.104708 10.100256 10.170353  9.898022 10.127320  9.477721  9.932914
    ## [15]  9.914905  9.981621  9.726580  9.755220 10.095024  9.986847 10.064863
    ## [22]  9.796363  9.940532 10.152525 10.006452 10.044053  9.784086  9.869887
    ## [29] 10.114997 10.071159  9.918525 10.084808  9.976106  9.885884 10.093375
    ## [36] 10.112400 10.086403  9.654468  9.888492  9.824551
    ## 
    ## $d
    ##  [1] -3.067552 -1.884263 -3.668175 -2.461614 -1.042546 -3.069976 -2.398086
    ##  [8] -3.936507 -2.242290 -2.263967 -3.553775 -4.386961 -2.527051 -3.492482
    ## [15] -1.684578 -3.726930 -1.651379 -1.069275 -1.666208 -4.356279

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
    ## 1  3.21 0.970

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0224  4.80

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.98 0.177

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.71  1.04

But this is really lame that we do the same thing and just change input
each time

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}
```
