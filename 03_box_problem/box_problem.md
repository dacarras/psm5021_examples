Thurstone’s Box Problem
================
dacarras

# Case Description

-   Empleamos la variante del Thurstone’s Box Problem de Kaiser & Horst
    (1975).
-   Los autores generan una base de datos de 10 indicadores, de cuarenta
    cajas diferentes, agregandoles error aleatorio a las medidas.
-   Sobre esta matriz de datos, se espera ajustar un modelo con tres
    factores.

# Cargar datos

## Abrir datos

``` r
#--------------------------------------------------------------------
# load data from url
#--------------------------------------------------------------------

box_data <- read.csv("https://www.evernote.com/shard/s42/sh/d0f20791-c8e5-4627-972e-5c76625f0ffb/695007b60806fd4ecfa7dbb73c67f8d1/res/4ac06b03-0a8e-4c3d-8d57-29f6e71a6598/box_prob.csv")

# ----------------------------------------------- 
# check data format
# -----------------------------------------------

library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
r4sda::variables_table(box_data) %>%
knitr::kable()
```

    ## Loading required package: purrr

    ## Loading required package: stringr

| variable | type | values                        | labels                    |
|:---------|:-----|:------------------------------|:--------------------------|
| box      | int  | 13, 14, 15, 16, 17, 18, 19,…  | === no variable label === |
| i1       | dbl  | 8, 18.2318, 16.6585, 16.8795… | === no variable label === |
| i2       | dbl  | 1, 1.5926, 4.7423, 11.0320, … | === no variable label === |
| i3       | dbl  | , 0.0842, 2.9603, 0.2534, 4.… | === no variable label === |
| i4       | dbl  | 8, 0.5529, 6.9657, 11.7508, … | === no variable label === |
| i5       | dbl  | , 3.6512, 7.7142, 4.5251, 7.… | === no variable label === |
| i6       | dbl  | , 1.9995, 3.6429, 2.9000, 4.… | === no variable label === |
| i7       | dbl  | , 4.5791, 4.2420, 5.1726, 4.… | === no variable label === |
| i8       | dbl  | , 3.9062, 4.5081, 4.1261, 4.… | === no variable label === |
| i9       | dbl  | , 2.1230, 2.7371, 3.0306, 3.… | === no variable label === |
| i10      | dbl  | 551, 12.4624, 11.8370, 14.20… | === no variable label === |

``` r
# ----------------------------------------------- 
# check data format
# -----------------------------------------------

dplyr::count(box_data, i1)
```

    ##          i1 n
    ## 1    6.8644 1
    ## 2    8.1373 1
    ## 3    8.1753 1
    ## 4    8.2808 1
    ## 5    8.8348 1
    ## 6    8.9451 1
    ## 7    9.0675 1
    ## 8    9.4011 1
    ## 9    9.7058 1
    ## 10  12.1873 1
    ## 11  12.2439 1
    ## 12  13.6259 1
    ## 13  15.0945 1
    ## 14  15.7092 1
    ## 15  15.7529 1
    ## 16  15.7908 1
    ## 17  15.8756 1
    ## 18  16.4200 1
    ## 19  16.4916 1
    ## 20  16.6585 1
    ## 21  16.8795 1
    ## 22  17.0639 1
    ## 23  17.4209 1
    ## 24  18.2318 1
    ## 25  18.5256 1
    ## 26  21.4984 1
    ## 27  22.8731 1
    ## 28  23.1491 1
    ## 29  23.5001 1
    ## 30  24.6375 1
    ## 31  24.7799 1
    ## 32  24.9122 1
    ## 33  25.1631 1
    ## 34  25.1676 1
    ## 35  25.2887 1
    ## 36  25.3243 1
    ## 37  25.6121 1
    ## 38  26.3386 1
    ## 39  26.5447 1
    ## 40 116.6115 1

``` r
# ----------------------------------------------- 
# isolate data from indicators
# -----------------------------------------------

box_items <- dplyr::select(box_data, i1:i10)


# ----------------------------------------------- 
# display data
# -----------------------------------------------

knitr::kable(box_items, digits = 2)
```

|     i1 |    i2 |    i3 |    i4 |    i5 |    i6 |   i7 |   i8 |   i9 |   i10 |
|-------:|------:|------:|------:|------:|------:|-----:|-----:|-----:|------:|
|   8.95 |  4.98 |  2.71 |  5.60 |  3.39 |  2.14 | 4.00 | 3.29 | 2.53 |  9.76 |
|   9.40 |  4.48 |  3.75 |  5.43 |  8.10 |  3.42 | 3.46 | 3.89 | 2.87 |  9.24 |
|   6.86 | 10.19 |  0.75 | 10.29 |  4.19 |  3.23 | 4.30 | 2.85 | 3.08 | 11.87 |
|  12.24 |  9.58 |  4.07 |  7.78 |  6.09 |  6.65 | 4.27 | 3.46 | 3.90 | 11.67 |
|   8.83 |  9.12 |  9.18 |  9.00 |  9.57 |  8.24 | 4.20 | 4.65 | 4.25 | 12.06 |
|  18.23 |  1.59 |  0.08 |  0.55 |  3.65 |  2.00 | 4.58 | 3.91 | 2.12 | 12.46 |
|  16.66 |  4.74 |  2.96 |  6.97 |  7.71 |  3.64 | 4.24 | 4.51 | 2.74 | 11.84 |
|  16.88 | 11.03 |  0.25 | 11.75 |  4.53 |  2.90 | 5.17 | 4.13 | 3.03 | 14.21 |
|  18.53 |  8.72 |  4.03 | 13.72 |  7.67 |  4.82 | 4.91 | 4.30 | 3.63 | 14.67 |
|  15.79 |  8.94 | 10.42 | 11.77 | 11.60 |  9.43 | 4.84 | 4.76 | 4.48 | 13.43 |
|  13.63 | 15.80 |  0.17 | 14.89 |  4.68 |  3.90 | 5.88 | 4.13 | 4.56 | 15.75 |
|  15.09 | 14.40 |  4.12 | 13.43 |  7.70 |  8.46 | 5.89 | 4.81 | 4.49 | 16.56 |
|  16.42 | 14.01 |  9.35 | 14.26 | 11.71 | 11.71 | 5.81 | 4.99 | 5.23 | 16.61 |
|  26.54 |  3.60 |  1.55 | 10.00 |  4.27 |  1.71 | 5.89 | 5.33 | 1.91 | 13.79 |
|  25.32 |  4.12 |  3.66 |  9.86 | 10.14 |  2.63 | 5.23 | 5.12 | 2.59 | 13.07 |
|  22.87 | 10.71 |  3.82 | 14.34 | 12.16 |  5.62 | 5.77 | 5.61 | 3.61 | 16.35 |
|  25.16 |  7.47 |  9.24 | 15.03 | 13.78 | 10.41 | 5.46 | 5.89 | 3.89 | 15.24 |
|  25.61 | 15.79 |  0.39 | 19.43 |  4.75 |  4.33 | 6.31 | 5.12 | 4.21 | 18.44 |
|  24.91 | 15.98 |  3.29 | 22.56 |  8.66 |  7.29 | 6.50 | 5.68 | 4.15 | 17.83 |
|  25.29 | 16.49 |  8.49 | 19.58 | 15.14 | 13.16 | 6.45 | 5.82 | 5.18 | 18.65 |
|   9.71 |  2.89 |  1.75 |  5.18 |  2.65 |  2.81 | 3.38 | 3.04 | 2.52 |  9.83 |
|   9.07 |  3.86 |  4.49 |  5.69 |  6.50 |  3.32 | 3.46 | 3.62 | 2.97 | 10.31 |
|   8.14 |  7.96 |  1.01 | 10.98 |  2.82 |  1.63 | 4.53 | 3.16 | 3.23 | 12.00 |
|   8.28 |  7.48 |  3.91 |  8.31 |  6.17 |  6.29 | 4.35 | 3.69 | 3.53 | 13.59 |
|   8.18 | 10.78 |  9.44 |  9.11 |  9.59 |  9.65 | 4.16 | 4.38 | 4.35 | 12.50 |
|  15.88 |  2.95 |  0.75 |  8.79 |  4.13 |  1.61 | 4.54 | 3.95 | 2.32 | 11.74 |
| 116.61 |  5.65 |  4.25 |  8.60 |  9.44 |  4.51 | 4.66 | 4.42 | 2.89 | 12.74 |
|  15.71 |  8.97 |  0.60 | 11.72 |  4.68 |  2.80 | 5.13 | 4.05 | 3.09 | 13.71 |
|  12.19 |  8.51 |  3.49 | 11.21 |  7.45 |  6.26 | 5.21 | 4.31 | 4.02 | 14.37 |
|  17.06 |  8.01 | 10.14 | 11.38 | 12.99 |  9.28 | 4.81 | 4.85 | 4.10 | 14.44 |
|  17.42 | 16.30 |  1.82 | 17.26 |  3.93 |  5.36 | 5.17 | 3.91 | 3.90 | 15.46 |
|  15.75 | 14.77 |  4.87 | 15.76 |  8.02 |  8.81 | 5.69 | 4.33 | 4.77 | 16.60 |
|  16.49 | 14.23 |  8.64 | 14.62 | 10.10 | 11.44 | 5.80 | 4.88 | 4.89 | 15.95 |
|  24.64 |  4.56 |  1.33 | 11.34 |  5.24 |  0.41 | 5.21 | 4.99 | 2.11 | 15.07 |
|  26.34 |  3.79 |  5.79 |  8.73 | 11.04 |  3.67 | 5.53 | 5.23 | 2.66 | 14.42 |
|  21.50 |  8.53 |  4.07 | 14.85 | 10.08 |  6.49 | 5.99 | 5.24 | 3.63 | 15.66 |
|  23.15 |  8.66 |  8.31 | 14.41 | 14.38 |  8.17 | 5.79 | 6.13 | 4.36 | 14.87 |
|  23.50 | 17.56 |  2.08 | 18.50 |  4.80 |  3.53 | 6.60 | 5.32 | 3.95 | 17.50 |
|  25.17 | 15.95 |  4.33 | 19.89 |  9.05 |  7.91 | 6.43 | 5.21 | 4.60 | 18.48 |
|  24.78 | 15.69 |  8.32 | 19.90 | 13.88 | 12.39 | 6.25 | 5.71 | 4.78 | 11.70 |

# Dimensionalidad

## Scree test

``` r
#--------------------------------------------------------------------
# dimensionality
#--------------------------------------------------------------------

# ----------------------------------------------- 
# scree test, based PCA
# -----------------------------------------------

psych::scree(box_items, factors = FALSE, pc = TRUE, hline = 1)
```

![](box_problem_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
# Nota: scree test generado sobre los eigenvalues de un 
#       análisis de componentes principales.


# ----------------------------------------------- 
# scree test, based EFA
# -----------------------------------------------

psych::scree(box_items, factors = FALSE, pc = FALSE, hline = 1)
```

![](box_problem_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
# Nota: scree test generado sobre los eigenvalues de un 
#       análisis de "principal axis factoring".
```

## Parallel

``` r
#--------------------------------------------------------------------
# dimensionality
#--------------------------------------------------------------------

# ----------------------------------------------- 
# parallel
# -----------------------------------------------

psych::fa.parallel(box_items, 
  fa = "fa", 
  fm = "ml",
  show.legend = FALSE 
  )
```

![](box_problem_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

    ## Parallel analysis suggests that the number of factors =  3  and the number of components =  NA

``` r
# ----------------------------------------------- 
# parallel based on minimum rank factor analysis
# -----------------------------------------------

EFA.MRFA::parallelMRFA(box_items, 
  Ndatsets = 500, 
  percent  = 95, 
  corr     = "Pearson", 
  display  = TRUE,
  graph    = TRUE
  )
```

    ## Computing PA. Time remaining  23 seconds                                                                  Computing PA. Time remaining  23 seconds                                                                  Computing PA. Time remaining  23 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  22 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  21 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  20 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  19 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  18 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  17 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  16 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  15 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  14 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  13 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  12 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  11 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  10 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  9 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  8 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  7 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  6 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  5 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  4 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  3 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  2 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  1 seconds                                                                  Computing PA. Time remaining  0 seconds                                                                                                                                                                      

![](box_problem_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

    ## 
    ## Parallel Analysis (PA) based on Minimum Rank Factor Analysis
    ## 
    ## Adequacy of the Dispersion Matrix:
    ## 
    ## Determinant of the matrix     = 0.000000843087289
    ## Bartlett's statistic          =   487.2 (df =    45; P = 0.000000)
    ## Kaiser-Meyer-Olkin (KMO) test = 0.80370 (good)
    ## 
    ## Implementation details:
    ## 
    ##   Correlation matrices analized:                Pearson correlation matrices
    ##   Number of random correlation matrices:        500
    ##   Method to obtain random correlation matrices: Permutation of the raw data
    ## 
    ## Item      Real-data        Mean of random   95 percentile of random
    ##           % of variance    % of variance    % of variance
    ## 
    ##    1       59.10**          23.28            27.89
    ##    2       20.66*           19.79            22.69
    ##    3       13.47            16.66            19.00
    ##    4        4.98            13.71            15.76
    ##    5        0.94            10.72            12.80
    ##    6        0.40             7.91            10.09
    ##    7        0.31             5.06             7.36
    ##    8        0.09             2.10             4.66
    ##    9        0.04             0.76             1.29
    ##   10        0.00             0.00             0.00
    ## 
    ## **  Advised number of factors:   1
    ## *   Advised number of factors:   2

## Hull Method

``` r
#--------------------------------------------------------------------
# dimensionality
#--------------------------------------------------------------------

# ----------------------------------------------- 
# Hull Method
# -----------------------------------------------

EFA.MRFA::hullEFA(box_items, extr = 'ULS', index_hull = 'CAF')
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

    ## HULL METHOD - CAF INDEX
    ## 
    ##         q      f          g       st
    ##         0      0.1963    45       0.0000 
    ##         1*     0.2637    35       
    ##         2*     0.3083    26       
    ##         3      0.4928    18       26.6300 
    ##         4      0.4956    11       0.0000 
    ## 
    ## Number of advised dimensions: 3 
    ## * Value outside the convex Hull 
    ## 
    ## -----------------------------------------------

![](box_problem_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Note: CAF stands for "common part accounted for".
#       CAF is an index that expresses the extent to which 
#       common variance in the observed data is captured by 
#       the common factor model. Simulation studies argue it
#       outperforms other methods (e.g., paralell, MAP)
#       (see Lorenzo-Seva, et al., 2011)
```

## MAP

``` r
#--------------------------------------------------------------------
# dimensionality
#--------------------------------------------------------------------

# ----------------------------------------------- 
# traditional output
# -----------------------------------------------

psych::VSS(x=box_items,fm="mle", plot = FALSE)
```

    ## 
    ## Very Simple Structure
    ## Call: vss(x = x, n = n, rotate = rotate, diagonal = diagonal, fm = fm, 
    ##     n.obs = n.obs, plot = plot, title = title, use = use, cor = cor)
    ## VSS complexity 1 achieves a maximimum of 0.78  with  1  factors
    ## VSS complexity 2 achieves a maximimum of 0.96  with  7  factors
    ## 
    ## The Velicer MAP achieves a minimum of 0.11  with  4  factors 
    ## BIC achieves a minimum of  -50.93  with  3  factors
    ## Sample Size adjusted BIC achieves a minimum of  -0.23  with  5  factors
    ## 
    ## Statistics by number of factors 
    ##   vss1 vss2  map dof   chisq    prob sqresid  fit RMSEA BIC  SABIC complex
    ## 1 0.78 0.00 0.25  35 2.8e+02 7.3e-41    8.18 0.78  0.42 155 264.94     1.0
    ## 2 0.68 0.93 0.21  26 1.5e+02 4.2e-20    2.44 0.93  0.35  58 139.25     1.3
    ## 3 0.64 0.93 0.16  18 1.5e+01 6.3e-01    0.82 0.98  0.00 -51   5.40     1.5
    ## 4 0.64 0.93 0.11  11 6.5e+00 8.4e-01    0.80 0.98  0.00 -34   0.35     1.5
    ## 5 0.64 0.92 0.15   5 2.6e+00 7.7e-01    0.69 0.98  0.00 -16  -0.23     1.6
    ## 6 0.64 0.85 0.19   0 2.9e-01      NA    0.65 0.98    NA  NA     NA     1.7
    ## 7 0.57 0.96 0.27  -4 9.6e-09      NA    0.13 1.00    NA  NA     NA     1.6
    ## 8 0.64 0.84 0.45  -7 6.5e-08      NA    0.58 0.98    NA  NA     NA     1.7
    ##    eChisq    SRMR eCRMS eBIC
    ## 1 1.9e+02 2.3e-01 0.259   58
    ## 2 3.9e+01 1.0e-01 0.138  -57
    ## 3 6.3e-01 1.3e-02 0.021  -66
    ## 4 4.4e-01 1.1e-02 0.022  -40
    ## 5 2.1e-01 7.6e-03 0.023  -18
    ## 6 7.8e-02 4.7e-03    NA   NA
    ## 7 2.0e-10 2.3e-07    NA   NA
    ## 8 1.2e-08 1.8e-06    NA   NA

``` r
# ----------------------------------------------- 
# output as plot
# -----------------------------------------------

map_out <- psych::VSS(box_items, rotate ="varimax", plot = FALSE)
```

    ## Warning in fa.stats(r = r, f = f, phi = phi, n.obs = n.obs, np.obs = np.obs, :
    ## The estimated weights for the factor scores are probably incorrect. Try a
    ## different factor score estimation method.

    ## Warning in fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, : An
    ## ultra-Heywood case was detected. Examine the results carefully

``` r
# extract MAP values and create vector of factors
map_vector  <- data.frame(map=map_out$map)
map_factors <- data.frame(factors=c(1:nrow(map_vector)))

# create data for plot
map_data <- dplyr::bind_cols(map_factors, map_vector)

# get values
map_value    <- min(map_vector, na.rm = TRUE)
map_retained <- dplyr::filter(map_data, map == min(map_vector, na.rm=TRUE)) %>% 
                dplyr::select(factors) %>% 
                as.numeric()

# plot MAP results
plot(map_data, 
  main = 'MAP criteria', 
  ylab = 'MAP values', 
  xlab = 'Number of factors', 
  ylim = c(0,.20), 
  type = 'p', 
  col = 'black', 
  pch=16)
abline(
  h = map_value, 
  v = map_retained, 
  col = "red", 
  lty=2, 
  lwd=1)
```

![](box_problem_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## EFA

``` r
#--------------------------------------------------------------------
# dimensionality
#--------------------------------------------------------------------

# ----------------------------------------------- 
# exploratory factor analysis
# -----------------------------------------------

library(psych)
library(GPArotation)
efa_fit <- fa(box_items, 
           fm = "mle", 
           nfactors = 3, 
           rotate = "varimax")

# ----------------------------------------------- 
# exploratory factor analysis
# -----------------------------------------------

efa_fit
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = box_items, nfactors = 3, rotate = "varimax", fm = "mle")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##       ML1  ML2   ML3   h2    u2 com
    ## i1   0.01 0.00  0.38 0.14 0.856 1.0
    ## i2   0.94 0.21 -0.18 0.95 0.045 1.2
    ## i3  -0.01 0.97  0.09 0.95 0.050 1.0
    ## i4   0.90 0.18  0.23 0.88 0.116 1.2
    ## i5   0.12 0.85  0.42 0.91 0.094 1.5
    ## i6   0.41 0.88 -0.04 0.94 0.061 1.4
    ## i7   0.82 0.07  0.51 0.93 0.067 1.7
    ## i8   0.42 0.42  0.78 0.97 0.032 2.1
    ## i9   0.70 0.65 -0.21 0.97 0.035 2.2
    ## i10  0.83 0.09  0.34 0.80 0.197 1.4
    ## 
    ##                        ML1  ML2  ML3
    ## SS loadings           3.89 3.12 1.44
    ## Proportion Var        0.39 0.31 0.14
    ## Cumulative Var        0.39 0.70 0.84
    ## Proportion Explained  0.46 0.37 0.17
    ## Cumulative Proportion 0.46 0.83 1.00
    ## 
    ## Mean item complexity =  1.5
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  13.99 with Chi Square of  487.19
    ## The degrees of freedom for the model are 18  and the objective function was  0.47 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.01 
    ## The df corrected root mean square of the residuals is  0.02 
    ## 
    ## The harmonic number of observations is  40 with the empirical chi square  0.63  with prob <  1 
    ## The total number of observations was  40  with Likelihood Chi Square =  15.47  with prob <  0.63 
    ## 
    ## Tucker Lewis Index of factoring reliability =  1.015
    ## RMSEA index =  0  and the 90 % confidence intervals are  0 0.122
    ## BIC =  -50.93
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    ML1  ML2  ML3
    ## Correlation of (regression) scores with factors   0.99 0.99 0.98
    ## Multiple R square of scores with factors          0.98 0.98 0.96
    ## Minimum correlation of possible factor scores     0.96 0.95 0.92

``` r
# ----------------------------------------------- 
# exploratory factor analysis
# -----------------------------------------------

library(psych)
library(GPArotation)
efa_fit <- fa(box_items, 
           fm = "mle", 
           nfactors = 2, 
           rotate = "varimax")

# ----------------------------------------------- 
# exploratory factor analysis
# -----------------------------------------------

efa_fit
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = box_items, nfactors = 2, rotate = "varimax", fm = "mle")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##       ML2  ML1    h2    u2 com
    ## i1   0.11 0.03 0.013 0.987 1.1
    ## i2   0.84 0.20 0.751 0.249 1.1
    ## i3  -0.03 1.00 0.995 0.005 1.0
    ## i4   0.92 0.22 0.898 0.102 1.1
    ## i5   0.19 0.86 0.777 0.223 1.1
    ## i6   0.36 0.86 0.866 0.134 1.3
    ## i7   0.89 0.13 0.816 0.184 1.0
    ## i8   0.55 0.49 0.552 0.448 2.0
    ## i9   0.59 0.63 0.750 0.250 2.0
    ## i10  0.87 0.14 0.781 0.219 1.0
    ## 
    ##                        ML2  ML1
    ## SS loadings           3.96 3.24
    ## Proportion Var        0.40 0.32
    ## Cumulative Var        0.40 0.72
    ## Proportion Explained  0.55 0.45
    ## Cumulative Proportion 0.55 1.00
    ## 
    ## Mean item complexity =  1.3
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  45  and the objective function was  13.99 with Chi Square of  487.19
    ## The degrees of freedom for the model are 26  and the objective function was  4.59 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.1 
    ## The df corrected root mean square of the residuals is  0.14 
    ## 
    ## The harmonic number of observations is  40 with the empirical chi square  39.33  with prob <  0.045 
    ## The total number of observations was  40  with Likelihood Chi Square =  153.8  with prob <  4.2e-20 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.478
    ## RMSEA index =  0.35  and the 90 % confidence intervals are  0.302 0.41
    ## BIC =  57.89
    ## Fit based upon off diagonal values = 0.96
    ## Measures of factor score adequacy             
    ##                                                    ML2  ML1
    ## Correlation of (regression) scores with factors   0.98 1.00
    ## Multiple R square of scores with factors          0.96 1.00
    ## Minimum correlation of possible factor scores     0.92 0.99

## CFA

``` r
#--------------------------------------------------------------------
# dimensionality
#--------------------------------------------------------------------

# ----------------------------------------------- 
# confirmatory factor analysis
# -----------------------------------------------

lavaan_model <- '

axis_x =~ start( 0.01)*i1 
axis_x =~ start( 0.94)*i2 
axis_x =~ start(-0.01)*i3 
axis_x =~ start( 0.90)*i4 
axis_x =~ start( 0.12)*i5 
axis_x =~ start( 0.41)*i6 
axis_x =~ start( 0.82)*i7 
axis_x =~ start( 0.42)*i8 
axis_x =~ start( 0.70)*i9 
axis_x =~ start( 0.83)*i10

axis_y =~ start(0.00)*i1 
axis_y =~ start(0.21)*i2 
axis_y =~ start(0.97)*i3 
axis_y =~ start(0.18)*i4 
axis_y =~ start(0.85)*i5 
axis_y =~ start(0.88)*i6 
axis_y =~ start(0.07)*i7 
axis_y =~ start(0.42)*i8 
axis_y =~ start(0.65)*i9 
axis_y =~ start(0.09)*i10


axis_z =~ start( 0.38)*i1 
axis_z =~ start(-0.18)*i2 
axis_z =~ start( 0.09)*i3 
axis_z =~ start( 0.23)*i4 
axis_z =~ start( 0.42)*i5 
axis_z =~ start(-0.04)*i6 
axis_z =~ start( 0.51)*i7 
axis_z =~ start( 0.78)*i8 
axis_z =~ start(-0.21)*i9 
axis_z =~ start( 0.34)*i10


axis_x ~~ 0*axis_y
axis_x ~~ 0*axis_z
axis_y ~~ 0*axis_z

'
# ----------------------------------------------- 
# fit lavaan model
# -----------------------------------------------

cfa_fit <- lavaan::sem(lavaan_model, 
            data = box_items,
            mimic='mplus',
            estimator = 'MLR')
```

    ## Warning in lav_model_vcov(lavmodel = lavmodel, lavsamplestats = lavsamplestats, : lavaan WARNING:
    ##     The variance-covariance matrix of the estimated parameters (vcov)
    ##     does not appear to be positive definite! The smallest eigenvalue
    ##     (= -1.595469e-02) is smaller than zero. This may be a symptom that
    ##     the model is not identified.

``` r
# ----------------------------------------------- 
# display results
# -----------------------------------------------

lavaan::summary(cfa_fit, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
```

    ## lavaan 0.6-12 ended normally after 320 iterations
    ## 
    ##   Estimator                                         ML
    ##   Optimization method                           NLMINB
    ##   Number of model parameters                        50
    ## 
    ##   Number of observations                            40
    ##   Number of missing patterns                         1
    ## 
    ## Model Test User Model:
    ##                                               Standard      Robust
    ##   Test Statistic                                18.849      24.878
    ##   Degrees of freedom                                15          15
    ##   P-value (Chi-square)                           0.221       0.052
    ##   Scaling correction factor                                  0.758
    ##     Yuan-Bentler correction (Mplus variant)                       
    ## 
    ## Model Test Baseline Model:
    ## 
    ##   Test statistic                               559.448     516.656
    ##   Degrees of freedom                                45          45
    ##   P-value                                        0.000       0.000
    ##   Scaling correction factor                                  1.083
    ## 
    ## User Model versus Baseline Model:
    ## 
    ##   Comparative Fit Index (CFI)                    0.993       0.979
    ##   Tucker-Lewis Index (TLI)                       0.978       0.937
    ##                                                                   
    ##   Robust Comparative Fit Index (CFI)                         0.985
    ##   Robust Tucker-Lewis Index (TLI)                            0.956
    ## 
    ## Loglikelihood and Information Criteria:
    ## 
    ##   Loglikelihood user model (H0)               -697.299    -697.299
    ##   Scaling correction factor                                  1.323
    ##       for the MLR correction                                      
    ##   Loglikelihood unrestricted model (H1)       -687.875    -687.875
    ##   Scaling correction factor                                  1.193
    ##       for the MLR correction                                      
    ##                                                                   
    ##   Akaike (AIC)                                1494.599    1494.599
    ##   Bayesian (BIC)                              1579.043    1579.043
    ##   Sample-size adjusted Bayesian (BIC)         1422.580    1422.580
    ## 
    ## Root Mean Square Error of Approximation:
    ## 
    ##   RMSEA                                          0.080       0.128
    ##   90 Percent confidence interval - lower         0.000       0.000
    ##   90 Percent confidence interval - upper         0.179       0.227
    ##   P-value RMSEA <= 0.05                          0.311       0.117
    ##                                                                   
    ##   Robust RMSEA                                               0.112
    ##   90 Percent confidence interval - lower                     0.000
    ##   90 Percent confidence interval - upper                     0.187
    ## 
    ## Standardized Root Mean Square Residual:
    ## 
    ##   SRMR                                           0.011       0.011
    ## 
    ## Parameter Estimates:
    ## 
    ##   Standard errors                             Sandwich
    ##   Information bread                           Observed
    ##   Observed information based on                Hessian
    ## 
    ## Latent Variables:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   axis_x =~                                                             
    ##     i1                0.010                               0.001    0.000
    ##     i2               68.025 1043.074    0.065    0.948    4.039    0.870
    ##     i3              -12.862  280.109   -0.046    0.963   -0.764   -0.241
    ##     i4               65.883 1010.981    0.065    0.948    3.912    0.821
    ##     i5               -5.365  169.370   -0.032    0.975   -0.319   -0.091
    ##     i6               11.128  136.885    0.081    0.935    0.661    0.197
    ##     i7               11.318  174.912    0.065    0.948    0.672    0.764
    ##     i8                3.957   54.646    0.072    0.942    0.235    0.283
    ##     i9                8.111  114.847    0.071    0.944    0.482    0.536
    ##     i10              31.896  491.501    0.065    0.948    1.894    0.770
    ##   axis_y =~                                                             
    ##     i1                0.000                               0.000    0.000
    ##     i2               23.795  484.392    0.049    0.961    1.951    0.420
    ##     i3               36.262  617.990    0.059    0.953    2.974    0.940
    ##     i4               22.164  455.520    0.049    0.961    1.818    0.381
    ##     i5               36.212  624.733    0.058    0.954    2.970    0.851
    ##     i6               38.689  683.689    0.057    0.955    3.173    0.948
    ##     i7                2.737   59.684    0.046    0.963    0.224    0.255
    ##     i8                5.077   92.005    0.055    0.956    0.416    0.502
    ##     i9                8.781  160.613    0.055    0.956    0.720    0.801
    ##     i10               8.330  176.266    0.047    0.962    0.683    0.278
    ##   axis_z =~                                                             
    ##     i1                0.380                               6.342    0.380
    ##     i2               -0.041    0.099   -0.416    0.677   -0.685   -0.147
    ##     i3                0.016    0.030    0.548    0.584    0.275    0.087
    ##     i4                0.073    0.106    0.688    0.491    1.219    0.256
    ##     i5                0.087    0.037    2.330    0.020    1.457    0.417
    ##     i6               -0.006    0.043   -0.142    0.887   -0.103   -0.031
    ##     i7                0.028    0.020    1.429    0.153    0.469    0.533
    ##     i8                0.040    0.014    2.830    0.005    0.661    0.797
    ##     i9               -0.010    0.015   -0.677    0.499   -0.173   -0.193
    ##     i10               0.054    0.053    1.022    0.307    0.897    0.365
    ## 
    ## Covariances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##   axis_x ~~                                                             
    ##     axis_y            0.000                               0.000    0.000
    ##     axis_z            0.000                               0.000    0.000
    ##   axis_y ~~                                                             
    ##     axis_z            0.000                               0.000    0.000
    ## 
    ## Intercepts:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .i1               19.820    2.641    7.506    0.000   19.820    1.187
    ##    .i2                9.471    0.734   12.897    0.000    9.471    2.039
    ##    .i3                4.292    0.500    8.580    0.000    4.292    1.357
    ##    .i4               12.062    0.754   16.004    0.000   12.062    2.530
    ##    .i5                7.911    0.552   14.334    0.000    7.911    2.266
    ##    .i6                5.801    0.529   10.962    0.000    5.801    1.733
    ##    .i7                5.146    0.139   37.011    0.000    5.146    5.852
    ##    .i8                4.566    0.131   34.822    0.000    4.566    5.506
    ##    .i9                3.628    0.142   25.519    0.000    3.628    4.035
    ##    .i10              14.112    0.389   36.290    0.000   14.112    5.738
    ##     axis_x            0.000                               0.000    0.000
    ##     axis_y            0.000                               0.000    0.000
    ##     axis_z            0.000                               0.000    0.000
    ## 
    ## Variances:
    ##                    Estimate  Std.Err  z-value  P(>|z|)   Std.lv  Std.all
    ##    .i1              238.700  220.807    1.081    0.280  238.700    0.856
    ##    .i2                0.978    0.462    2.117    0.034    0.978    0.045
    ##    .i3                0.505    0.170    2.975    0.003    0.505    0.050
    ##    .i4                2.627    0.952    2.759    0.006    2.627    0.116
    ##    .i5                1.140    0.355    3.211    0.001    1.140    0.094
    ##    .i6                0.687    0.151    4.556    0.000    0.687    0.061
    ##    .i7                0.051    0.018    2.936    0.003    0.051    0.067
    ##    .i8                0.022    0.020    1.107    0.268    0.022    0.032
    ##    .i9                0.028    0.017    1.678    0.093    0.028    0.035
    ##    .i10               1.189    0.776    1.533    0.125    1.189    0.197
    ##     axis_x            0.004    0.112    0.031    0.975    1.000    1.000
    ##     axis_y            0.007    0.234    0.029    0.977    1.000    1.000
    ##     axis_z          278.507   92.478    3.012    0.003    1.000    1.000
    ## 
    ## R-Square:
    ##                    Estimate
    ##     i1                0.144
    ##     i2                0.955
    ##     i3                0.950
    ##     i4                0.884
    ##     i5                0.906
    ##     i6                0.939
    ##     i7                0.933
    ##     i8                0.968
    ##     i9                0.965
    ##     i10               0.803

## Annotated References

-   Summary of common methods to assess dimensionality

Timmerman, M. E., Lorenzo-Seva, U., & Ceulemans, E. (2017). The number
of factors problem. The Wiley Handbook of Psychometric Testing: A
Multidisciplinary Reference on Survey, Scale and Test Development, 1–2,
305–324. <https://doi.org/10.1002/9781118489772.ch11>

-   Thurstone Box

Kaiser, H. F., & Horst, P. (1975). A Score Matrix for Thurstone’s Box
Problem. Multivariate Behavioral Research, 10(1), 17–26. <http://>
doi.org/10.1207/s15327906mbr1001_2

-   Hull Method

Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. a. L. (2011). The Hull
Method for Selecting the Number of Common Factors. Multivariate
Behavioral Research, 46(2), 340–364.
<https://doi.org/10.1080/00273171.2011.564527>

-   Parallel

Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality assessment
of ordered polytomous items with parallel analysis. Psychological
Methods, 16(2), 209–220. <https://doi.org/10.1037/a0023353>
