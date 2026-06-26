phynotype
================

`phynotype` is an R package for clustering workflows, consensus
meta-clustering, validation, exploration, prediction, and plotting.

`phynotype` is a clustering workflow package. It is not a phenotypic
data processing package.

## Installation

`phynotype` is not on CRAN yet. Install the development version from
GitHub:

``` r
# install.packages("pak")
pak::pkg_install("ielbadisy/phynotype")
```

Or install from a local source checkout:

``` r
pak::pkg_install("path/to/phynotype")
```

## Quick start

``` r
library(phynotype)

fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
fit
#> <cluster_fit>
#>   Method: kmeans
#>   Observations: 150
#>   Clusters: 3
summary(fit)
#> Cluster fit summary
#>   Method: kmeans
#>   Observations: 150
#>   Clusters: 3
#>   Sizes: 1=62, 2=38, 3=50
```

## Meta-clustering

``` r
mfit <- metacluster(
  iris[, 1:4],
  methods = c("kmeans", "pam", "hclust"),
  k = 2:5,
  consensus = "coassoc",
  seed = 1
)

mfit
#> <metacluster_fit>
#>   Methods: kmeans, pam, hclust
#>   Candidate fits: 12
#>   Final clusters: 5
summary(mfit)
#> Meta-cluster summary
#>   Methods: kmeans, pam, hclust
#>   Candidate fits: 12
#>   Final clusters: 5
#>   Sizes: 1=50, 2=37, 3=28, 4=23, 5=12
```

## Validation

``` r
validate(fit)
#> <cluster_validation>
#>   Object type: cluster_fit
#>   Metrics: 5
#>             metric       value               scale        direction
#>         silhouette   0.5528190             -1 to 1 higher is better
#>  calinski_harabasz 561.6277566 positive, unbounded higher is better
#>     davies_bouldin   0.6619715 positive, unbounded  lower is better
#>       total_within  78.8514414 positive, unbounded  lower is better
#>      bootstrap_ari   0.9710021                <NA>             <NA>
validate(mfit)
#> <cluster_validation>
#>   Object type: metacluster_fit
#>   Metrics: 4
#>                        metric       value               scale        direction
#>                    silhouette   0.4925619             -1 to 1 higher is better
#>             calinski_harabasz 494.0510734 positive, unbounded higher is better
#>                davies_bouldin   0.8168048 positive, unbounded  lower is better
#>  pairwise_partition_agreement   0.8186128                <NA>             <NA>
validate(iris[, 1:4], method = "kmeans", k = 2:6, seed = 1)
#> <cluster_validation>
#>   Object type: validation_grid
#>   Metrics: 25
#>             metric       value               scale        direction k
#>         silhouette   0.6810462             -1 to 1 higher is better 2
#>  calinski_harabasz 513.9245460 positive, unbounded higher is better 2
#>     davies_bouldin   0.4042928 positive, unbounded  lower is better 2
#>       total_within 152.3479518 positive, unbounded  lower is better 2
#>      bootstrap_ari   0.9973179                <NA>             <NA> 2
#>         silhouette   0.5528190             -1 to 1 higher is better 3
#>  calinski_harabasz 561.6277566 positive, unbounded higher is better 3
#>     davies_bouldin   0.6619715 positive, unbounded  lower is better 3
#>       total_within  78.8514414 positive, unbounded  lower is better 3
#>      bootstrap_ari   0.9710021                <NA>             <NA> 3
#>         silhouette   0.4980505             -1 to 1 higher is better 4
#>  calinski_harabasz 530.7658082 positive, unbounded higher is better 4
#>     davies_bouldin   0.7803070 positive, unbounded  lower is better 4
#>       total_within  57.2284732 positive, unbounded  lower is better 4
#>      bootstrap_ari   0.9052257                <NA>             <NA> 4
#>         silhouette   0.4912400             -1 to 1 higher is better 5
#>  calinski_harabasz 495.3699060 positive, unbounded higher is better 5
#>     davies_bouldin   0.8159888 positive, unbounded  lower is better 5
#>       total_within  46.4611727 positive, unbounded  lower is better 5
#>      bootstrap_ari   0.9218582                <NA>             <NA> 5
#>         silhouette   0.3648340             -1 to 1 higher is better 6
#>  calinski_harabasz 473.8506068 positive, unbounded higher is better 6
#>     davies_bouldin   0.9141580 positive, unbounded  lower is better 6
#>       total_within  39.0399872 positive, unbounded  lower is better 6
#>      bootstrap_ari   0.8684184                <NA>             <NA> 6
```

## Exploration

``` r
exp <- explore(fit)
exp
#> <cluster_explore>
#>   Rows in feature summary: 12
head(exp$feature_summary)
#>   cluster      feature     mean        sd median min max
#> 1       1 Sepal.Length 5.901613 0.4664101    5.9 4.9 7.0
#> 2       1  Sepal.Width 2.748387 0.2962841    2.8 2.0 3.4
#> 3       1 Petal.Length 4.393548 0.5088950    4.5 3.0 5.1
#> 4       1  Petal.Width 1.433871 0.2974997    1.4 1.0 2.4
#> 5       2 Sepal.Length 6.850000 0.4941550    6.7 6.1 7.9
#> 6       2  Sepal.Width 3.073684 0.2900924    3.0 2.5 3.8
```

## Plotting

``` r
plot_clusters(fit)
```

![](README_files/figure-gfm/plot-clusters-1.png)<!-- -->

``` r
plot_silhouette(fit)
```

![](README_files/figure-gfm/plot-silhouette-1.png)<!-- -->

``` r
plot_consensus(mfit)
```

![](README_files/figure-gfm/plot-consensus-1.png)<!-- -->

``` r
plot_coassoc(mfit)
```

![](README_files/figure-gfm/plot-coassoc-1.png)<!-- -->

``` r
plot_feature_profiles(explore(fit))
```

![](README_files/figure-gfm/plot-feature-profiles-1.png)<!-- -->

``` r
plot_cluster_sizes(fit)
```

![](README_files/figure-gfm/plot-cluster-sizes-1.png)<!-- -->

## Prediction

``` r
pred <- predict(fit, iris[1:10, 1:4])
pred
#> <cluster_prediction>
#>   Method: kmeans
#>   Predictions: 10
data.frame(
  observation = seq_along(pred$clusters),
  cluster = pred$clusters
)
#>    observation cluster
#> 1            1       3
#> 2            2       3
#> 3            3       3
#> 4            4       3
#> 5            5       3
#> 6            6       3
#> 7            7       3
#> 8            8       3
#> 9            9       3
#> 10          10       3
```
