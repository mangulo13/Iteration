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
    ## -2.6548 -0.7660 -0.2164 -0.1454  0.4513  1.8744

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
    ##  [1] 3.484394 2.266660 2.813236 3.673888 3.720911 5.473088 5.064549 2.875994
    ##  [9] 2.360615 3.191471 1.392650 3.059307 2.583751 2.131058 1.199976 4.251106
    ## [17] 2.092953 2.408045 3.278344 2.710825
    ## 
    ## $b
    ##  [1]  2.0560209  0.4029419  5.8907019 -0.6099964  5.4886646  2.0814214
    ##  [7]  6.8563576  5.9459649 -1.9628081  2.3943476 -1.7880292  1.8429534
    ## [13] -7.5603436 -0.8168323  1.4912853  6.4930595 -6.9330857  9.6798837
    ## [19]  4.4540008 -2.4492591 -0.1099551 -6.2070341  1.4967491 -7.8443682
    ## [25]  5.9640218 -6.2522222 -1.1104243 -8.3254745  0.3334973  4.7226977
    ## 
    ## $c
    ##  [1] 10.126372 10.473087  9.637701 10.222700 10.329496 10.223614 10.178649
    ##  [8] 10.060418  9.722214  9.819466 10.172549 10.098792 10.225127 10.012064
    ## [15] 10.053525  9.955432 10.079794  9.652878  9.769215 10.224213 10.001341
    ## [22]  9.970102  9.941711 10.009910 10.076574 10.054488 10.003115  9.859720
    ## [29] 10.055858 10.200355 10.068927  9.907413  9.828034  9.923943 10.267531
    ## [36] 10.181207 10.663844 10.222812 10.213220 10.163737
    ## 
    ## $d
    ##  [1] -3.857685 -4.066104 -4.053380 -1.747896 -1.777045 -3.834924 -3.633733
    ##  [8] -5.241104 -3.943326 -2.230060 -4.784773 -5.320495 -2.196204 -3.807657
    ## [15] -3.396409 -3.150310 -5.228088 -3.339589 -3.832300 -1.790969

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
    ## 1  3.00  1.08

``` r
mean_and_sd(list_norm[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.521  4.93

``` r
mean_and_sd(list_norm[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.207

``` r
mean_and_sd(list_norm[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.56  1.14

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

Can make map spit out things that are not lists

``` r
output = map_dbl(list_norm, median, .id = "input")
```

Can make output into one dataframe

``` r
output = map_df(list_norm, mean_and_sd, .id = "input")
```

.id pulls the input titles into a new column

## List Columns\!

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>%  pull(samp)
```

    ## $a
    ##  [1] 3.484394 2.266660 2.813236 3.673888 3.720911 5.473088 5.064549 2.875994
    ##  [9] 2.360615 3.191471 1.392650 3.059307 2.583751 2.131058 1.199976 4.251106
    ## [17] 2.092953 2.408045 3.278344 2.710825
    ## 
    ## $b
    ##  [1]  2.0560209  0.4029419  5.8907019 -0.6099964  5.4886646  2.0814214
    ##  [7]  6.8563576  5.9459649 -1.9628081  2.3943476 -1.7880292  1.8429534
    ## [13] -7.5603436 -0.8168323  1.4912853  6.4930595 -6.9330857  9.6798837
    ## [19]  4.4540008 -2.4492591 -0.1099551 -6.2070341  1.4967491 -7.8443682
    ## [25]  5.9640218 -6.2522222 -1.1104243 -8.3254745  0.3334973  4.7226977
    ## 
    ## $c
    ##  [1] 10.126372 10.473087  9.637701 10.222700 10.329496 10.223614 10.178649
    ##  [8] 10.060418  9.722214  9.819466 10.172549 10.098792 10.225127 10.012064
    ## [15] 10.053525  9.955432 10.079794  9.652878  9.769215 10.224213 10.001341
    ## [22]  9.970102  9.941711 10.009910 10.076574 10.054488 10.003115  9.859720
    ## [29] 10.055858 10.200355 10.068927  9.907413  9.828034  9.923943 10.267531
    ## [36] 10.181207 10.663844 10.222812 10.213220 10.163737
    ## 
    ## $d
    ##  [1] -3.857685 -4.066104 -4.053380 -1.747896 -1.777045 -3.834924 -3.633733
    ##  [8] -5.241104 -3.943326 -2.230060 -4.784773 -5.320495 -2.196204 -3.807657
    ## [15] -3.396409 -3.150310 -5.228088 -3.339589 -3.832300 -1.790969

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try some operations

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.00  1.08

``` r
mean_and_sd(listcol_df$samp[[2]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.521  4.93

``` r
mean_and_sd(listcol_df$samp[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.207

``` r
mean_and_sd(listcol_df$samp[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.56  1.14

Can I just… map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.00  1.08
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.521  4.93
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.207
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.56  1.14

So… can I add a list column?

``` r
listcol_df = 
  listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians= map_dbl(samp, median))
```
