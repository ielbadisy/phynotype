#' Fit a clustering solution
#'
#' Fit a single clustering solution using a supported method.
#'
#' @param x Numeric matrix/data frame for numeric methods, a `dist` object
#'   (for example a Gower dissimilarity from [mixeddistance()]) for the
#'   distance-based methods `"hclust"`, `"agnes"`, `"pam"`, and `"dbscan"`,
#'   or a mixed-type data frame for `"kproto"` and `"kmm"`.
#' @param method Clustering method. Supported values are `"kmeans"`, `"pam"`,
#'   `"hclust"`, `"agnes"`, `"dbscan"`, `"gmm"`, `"kproto"`, and `"kmm"`.
#' @param ... Additional method-specific arguments passed to the underlying
#'   engine (e.g. `nstart`, `iter.max`, `linkage`, `eps`, `minPts`, `lambda`).
#' @param k Number of clusters for methods that require it (`"kmeans"`,
#'   `"pam"`, `"hclust"`, `"agnes"`, `"gmm"`, `"kproto"`, `"kmm"`).
#' @param scale Logical; if `TRUE`, scale columns before fitting.
#' @param center Logical; if `TRUE`, center columns before fitting.
#' @param seed Optional integer random seed for reproducible results.
#'
#' @details
#' `cluster()` is the single entry point for all supported methods. It
#' dispatches to a method-specific backend registered via the internal
#' `cluster_registry`, validates inputs, applies any requested preprocessing,
#' and returns a `cluster_fit` object with a uniform interface regardless of
#' the underlying engine.
#'
#' Method-specific notes:
#' \describe{
#'   \item{`"kmeans"`}{Minimizes total within-cluster sum of squares
#'     \eqn{\sum_j \sum_{x_i \in C_j} \|x_i - \mu_j\|^2} (Lloyd, 1982).
#'     Requires numeric `x` and `k`.}
#'   \item{`"pam"`}{Partitioning Around Medoids: minimizes
#'     \eqn{\sum_i \min_g d(x_i, m_g)} over actual observations (Kaufman and
#'     Rousseeuw, 1990). Accepts raw numeric data or a precomputed `dist`
#'     object (passed to `cluster::pam(diss = TRUE)`), which gives the Gower
#'     plus PAM workflow for mixed data via [mixeddistance()]. Fits on a
#'     `dist` object have no prototypes and cannot predict new data.
#'     Requires the `cluster` package.}
#'   \item{`"hclust"` / `"agnes"`}{Hierarchical agglomerative methods. Accept
#'     a precomputed `dist` object or raw numeric data. Specify `linkage` to
#'     choose the merging criterion (default `"complete"` for `hclust`,
#'     `"average"` for `agnes`). `"agnes"` requires the `cluster` package.}
#'   \item{`"dbscan"`}{Density-Based Spatial Clustering of Applications with
#'     Noise (Ester et al., 1996). Requires `eps` (neighborhood radius) and
#'     `minPts`. Observations in sparse regions are labeled noise (cluster 0).
#'     Also accepts a precomputed `dist` object (no prediction in that case).
#'     Requires the `dbscan` package.}
#'   \item{`"gmm"`}{Gaussian Mixture Model fitted via the EM algorithm (Fraley
#'     and Raftery, 2002). Returns soft membership probabilities. Requires the
#'     `mclust` package.}
#'   \item{`"kproto"`}{K-prototypes for mixed numeric/categorical data (Huang,
#'     1998). Use `lambda` to control the numeric-vs-categorical trade-off.
#'     Requires the `clustMixType` package.}
#'   \item{`"kmm"`}{K-Mixed-Modes: a native mixed-data algorithm that
#'     minimizes a weighted prototype distance combining squared Euclidean and
#'     Hamming contributions controlled by `lambda`.}
#' }
#'
#' @section Name clash with the survival package:
#' The \pkg{survival} package also exports `cluster()` (a formula special
#' for robust variances in `coxph()`). When both packages are attached, the
#' one attached last masks the other. Inside a `coxph()` formula the
#' `cluster()` term is still handled by \pkg{survival} either way, but a
#' plain call such as `cluster(x, method = "pam", k = 3)` reaches
#' \pkg{phynotype} only if it was attached last. In scripts that use both
#' packages, call `phynotype::cluster()` explicitly.
#'
#' @return A `cluster_fit` object with components:
#' \describe{
#'   \item{`method`}{Character; the method used.}
#'   \item{`clusters`}{Integer vector of cluster assignments.}
#'   \item{`n_clusters`}{Integer; number of clusters found.}
#'   \item{`centers`}{Cluster centers (numeric methods) or `NULL`.}
#'   \item{`prototypes`}{Cluster prototypes (mixed methods) or `NULL`.}
#'   \item{`membership`}{Soft membership matrix (GMM) or `NULL`.}
#'   \item{`data_info`}{List of input metadata for downstream use.}
#' }
#'
#' @seealso [validate()] to score the fit, [explore()] to summarize cluster
#'   structure, [predict.cluster_fit()] to assign new observations,
#'   [metacluster()] for consensus clustering,
#'   [featureimportance()], [limeexplain()], [ceterisparibus()] for
#'   interpretability.
#'
#' @references
#' Lloyd, S.P. (1982). Least squares quantization in PCM. *IEEE Transactions
#' on Information Theory*, **28**(2), 129–137.
#'
#' Kaufman, L. and Rousseeuw, P.J. (1990). *Finding Groups in Data: An
#' Introduction to Cluster Analysis*. John Wiley & Sons, New York.
#'
#' Ester, M., Kriegel, H.-P., Sander, J. and Xu, X. (1996). A density-based
#' algorithm for discovering clusters in large spatial databases with noise.
#' *Proceedings of the 2nd ACM SIGKDD*, pp. 226–231.
#'
#' Fraley, C. and Raftery, A.E. (2002). Model-based clustering, discriminant
#' analysis, and density estimation. *Journal of the American Statistical
#' Association*, **97**(458), 611–631.
#'
#' Huang, Z. (1998). Extensions to the k-means algorithm for clustering large
#' data sets with categorical values. *Data Mining and Knowledge Discovery*,
#' **2**(3), 283–304.
#'
#' @export
#'
#' @examples
#' # Numeric data: k-means
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' fit
#' clusters(fit)
#' centers(fit)
#'
#' # PAM (requires the cluster package)
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   fit_pam <- cluster(iris[, 1:4], method = "pam", k = 3)
#'   fit_pam
#' }
#'
#' # Hierarchical clustering from a distance object
#' d <- mixeddistance(iris[, 1:4])
#' fit_hc <- cluster(d, method = "hclust", k = 3)
#' fit_hc
#'
#' # Mixed data: Gower dissimilarity + PAM
#' if (requireNamespace("cluster", quietly = TRUE)) {
#'   fit_gpam <- cluster(mixeddistance(iris), method = "pam", k = 3)
#'   fit_gpam
#' }
#'
#' # Mixed data: k-prototypes
#' if (requireNamespace("clustMixType", quietly = TRUE)) {
#'   fit_kp <- cluster(iris, method = "kproto", k = 3, seed = 1)
#'   fit_kp
#' }
cluster <- function(x,
                    method = "kmeans",
                    ...,
                    k = NULL,
                    scale = FALSE,
                    center = TRUE,
                    seed = NULL) {
  method <- normalize_method_name(method)
  registry <- get_cluster_registry()
  entry <- registry[[method]]
  if (is.null(entry)) {
    stop("Unsupported clustering method `", method, "`.", call. = FALSE)
  }
  if (!is.null(seed)) {
    validate_seed(seed)
  }

  params <- c(list(k = k, scale = scale, center = center, seed = seed), list(...))
  prepared <- prepare_cluster_input(x, method = method, scale = scale, center = center, k = k)
  entry$validate(prepared$data, params)

  result <- with_seed(seed, entry$fit(prepared$data, params))
  new_cluster_fit(
    method = method,
    call = match.call(),
    params = params,
    clusters = result$clusters,
    n_clusters = result$n_clusters,
    membership = result$membership,
    centers = result$centers,
    prototypes = result$prototypes,
    distance_info = result$distance_info,
    fitted_object = result$fitted_object,
    data_info = prepared$data_info,
    extras = result$extras
  )
}
