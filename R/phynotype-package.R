#' phynotype: Clustering Workflows and Consensus Meta-Clustering
#'
#' @description
#' `phynotype` provides tools for unsupervised phenotyping workflows.
#' The six-step pipeline is:
#'
#' \enumerate{
#'   \item **Cluster**: fit a single clustering solution with [cluster()].
#'   \item **Meta-cluster**: aggregate candidate solutions into a consensus
#'         partition with [metacluster()].
#'   \item **Validate**: score a solution with internal and external metrics
#'         using [validate()].
#'   \item **Explore**: summarize cluster sizes, feature profiles, and
#'         two-dimensional embeddings with [explore()].
#'   \item **Predict**: assign new observations to learned clusters with
#'         [predict.cluster_fit()].
#'   \item **Interpret**: explain the clustering rule with
#'         [feature_importance()], [lime_explain()], and
#'         [ceteris_paribus()].
#' }
#'
#' All steps operate through S3 generic functions and return structured objects
#' with `print()`, `summary()`, and `plot()` methods.
#'
#' @section Supported clustering methods:
#' \describe{
#'   \item{`"kmeans"`}{Lloyd's k-means algorithm for numeric data.}
#'   \item{`"pam"`}{Partitioning Around Medoids for numeric data.}
#'   \item{`"hclust"`}{Hierarchical clustering via `stats::hclust()`.}
#'   \item{`"agnes"`}{Agglomerative nesting via `cluster::agnes()`.}
#'   \item{`"dbscan"`}{Density-based spatial clustering via `dbscan::dbscan()`.}
#'   \item{`"gmm"`}{Gaussian mixture models via `mclust::Mclust()`.}
#'   \item{`"kproto"`}{K-prototypes for mixed numeric/categorical data.}
#'   \item{`"kmm"`}{K-Mixed-Modes, a native mixed-data algorithm.}
#' }
#'
#' @section Validation metrics:
#' Internal metrics: silhouette width (Rousseeuw, 1987), Calinski-Harabasz
#' index (Calinski and Harabasz, 1974), Davies-Bouldin index (Davies and
#' Bouldin, 1979), total within-cluster sum of squares, and bootstrap ARI.
#' External metrics (when reference labels are available): adjusted Rand index
#' (Hubert and Arabie, 1985) and normalized mutual information (Strehl and
#' Ghosh, 2002).
#'
#' @section Mixed-type data:
#' Use [prepare_mixed_data()] to encode mixed-type data frames into a numeric
#' matrix before distance-based methods. Use [mixed_distance()] to build a
#' Gower distance matrix (Gower, 1971) before hierarchical methods.
#'
#' @references
#' Rousseeuw, P.J. (1987). Silhouettes: A graphical aid to the interpretation
#' and validation of cluster analysis. *Journal of Computational and Applied
#' Mathematics*, **20**, 53–65.
#'
#' Calinski, T. and Harabasz, J. (1974). A dendrite method for cluster
#' analysis. *Communications in Statistics*, **3**(1), 1–27.
#'
#' Davies, D.L. and Bouldin, D.W. (1979). A cluster separation measure. *IEEE
#' Transactions on Pattern Analysis and Machine Intelligence*, **1**(2),
#' 224–227.
#'
#' Hubert, L. and Arabie, P. (1985). Comparing partitions. *Journal of
#' Classification*, **2**(1), 193–218.
#'
#' Strehl, A. and Ghosh, J. (2002). Cluster ensembles: A knowledge reuse
#' framework for combining multiple partitions. *Journal of Machine Learning
#' Research*, **3**, 583–617.
#'
#' Gower, J.C. (1971). A general coefficient of similarity and some of its
#' properties. *Biometrics*, **27**(4), 857–874.
#'
#' @seealso [cluster()], [metacluster()], [validate()], [explore()],
#'   [predict.cluster_fit()], [feature_importance()], [lime_explain()],
#'   [ceteris_paribus()]
#'
#' @docType package
#' @name phynotype-package
#' @aliases phynotype
#' @importFrom rlang .data
"_PACKAGE"
