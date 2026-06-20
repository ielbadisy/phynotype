#' Fit a consensus meta-clustering solution
#'
#' Fit several candidate clustering solutions and summarize their agreement
#' through a co-association consensus matrix. The consensus partition is
#' obtained by hierarchical clustering of the dissimilarity matrix
#' \eqn{D = 1 - C}, where \eqn{C_{ij}} is the proportion of candidate
#' partitions that place observations \eqn{i} and \eqn{j} in the same cluster.
#'
#' @param x Numeric matrix or numeric data frame.
#' @param methods Character vector of candidate clustering methods. Any method
#'   supported by [cluster()] may be used (e.g. `c("kmeans", "pam",
#'   "hclust")`).
#' @param k Integer vector of candidate cluster counts. All combinations of
#'   `methods` and `k` are fitted and pooled.
#' @param consensus Consensus strategy. Currently only `"coassoc"` (Evidence
#'   Accumulation Clustering) is implemented.
#' @param scale Logical; if `TRUE`, scale columns before fitting candidates.
#' @param center Logical; if `TRUE`, center columns before fitting candidates.
#' @param seed Optional integer random seed. Each candidate is given a
#'   deterministic offset seed so results are jointly reproducible.
#' @param ... Additional arguments passed through to [cluster()].
#'
#' @details
#' ## Co-association consensus
#'
#' For \eqn{B} candidate partitions, the co-association matrix is
#' \deqn{
#'   C_{ij} = \frac{1}{B} \sum_{b=1}^{B} I\{i \text{ and } j \text{ are in
#'   the same cluster in partition } b\}.
#' }
#' This estimator is related to the Evidence Accumulation Clustering framework
#' (Fred and Jain, 2002). The consensus dissimilarity \eqn{D = 1 - C} is then
#' hierarchically clustered (average linkage), and the optimal number of
#' clusters is chosen by maximizing the mean silhouette width.
#'
#' Stability of the ensemble is summarized by the mean pairwise partition
#' agreement (PPA) across all candidate pairs:
#' \deqn{
#'   \mathrm{PPA}(U, V) = \frac{1}{\binom{n}{2}}
#'   \sum_{i < j} I\{(u_i = u_j) = (v_i = v_j)\}.
#' }
#'
#' @return A `metacluster_fit` object with components:
#' \describe{
#'   \item{`final_clusters`}{Integer vector of final consensus assignments.}
#'   \item{`final_k`}{Integer; the selected number of clusters.}
#'   \item{`coassoc_matrix`}{The \eqn{n \times n} co-association matrix.}
#'   \item{`candidate_table`}{Data frame listing all fitted candidate
#'     solutions.}
#'   \item{`stability_summary`}{Mean, min, and max pairwise partition
#'     agreement.}
#'   \item{`selection_summary`}{Per-\eqn{k} silhouette scores used for
#'     selection.}
#' }
#'
#' @seealso [cluster()] for single solutions, [validate()] to score the
#'   consensus partition, [plot_coassoc()] to visualize the agreement matrix,
#'   [plot_consensus()] for a 2-D embedding of the final clusters.
#'
#' @references
#' Fred, A.L.N. and Jain, A.K. (2002). Data clustering using evidence
#' accumulation. *Proceedings of the 16th International Conference on Pattern
#' Recognition (ICPR'02)*, Vol. 4, pp. 276–280.
#'
#' Strehl, A. and Ghosh, J. (2002). Cluster ensembles: A knowledge reuse
#' framework for combining multiple partitions. *Journal of Machine Learning
#' Research*, **3**, 583–617.
#'
#' @export
#'
#' @examples
#' mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"), k = 2:4,
#'                     seed = 1)
#' mfit
#'
#' # Inspect the co-association matrix
#' mfit$coassoc_matrix[1:4, 1:4]
#'
#' # Score the consensus partition
#' validate(mfit)$metrics_table
metacluster <- function(x,
                        methods = c("kmeans", "pam", "hclust"),
                        k = 2:6,
                        consensus = "coassoc",
                        scale = FALSE,
                        center = TRUE,
                        seed = NULL,
                        ...) {
  if (!identical(consensus, "coassoc")) {
    stop("Only `consensus = \"coassoc\"` is currently supported.", call. = FALSE)
  }
  methods <- validate_methods_vector(methods)
  k_values <- check_k_grid(k)
  if (!is.null(seed)) {
    validate_seed(seed)
  }

  ## Route input validation through the right checker based on requested methods.
  ## Previously hardcoded to "kmeans" which rejected mixed-type data frames even
  ## when only kmm/kproto were requested.
  input_method <- if (any(methods %in% c("kproto", "kmm"))) "kmm" else "kmeans"
  prepared <- prepare_cluster_input(x, method = input_method, scale = scale, center = center, k = k_values[[1]])
  data <- prepared$data_info$original_data
  if (inherits(data, "dist")) {
    stop("`metacluster()` requires a numeric matrix or data frame.", call. = FALSE)
  }

  candidates <- list()
  candidate_labels <- list()
  candidate_rows <- list()
  idx <- 1L
  for (method in methods) {
    for (k_i in k_values) {
      fit_seed <- if (is.null(seed)) NULL else as.integer(seed) + idx - 1L
      fit <- cluster(
        x,
        method = method,
        k = k_i,
        scale = scale,
        center = center,
        seed = fit_seed,
        ...
      )
      candidates[[idx]] <- fit
      candidate_labels[[idx]] <- clusters(fit)
      candidate_rows[[idx]] <- data.frame(
        candidate = idx,
        method = method,
        k = k_i,
        n_clusters = n_clusters(fit)
      )
      idx <- idx + 1L
    }
  }

  coassoc <- compute_coassoc_matrix(candidate_labels)
  consensus_dist <- stats::as.dist(1 - coassoc)
  selected <- select_consensus_k(consensus_dist, k_values)
  consensus_fit <- stats::hclust(consensus_dist, method = "average")
  final_clusters <- stats::cutree(consensus_fit, k = selected$final_k)

  new_metacluster_fit(
    call = match.call(),
    params = list(methods = methods, k = k_values, consensus = consensus, scale = scale, center = center, seed = seed),
    candidate_fits = candidates,
    candidate_labels = candidate_labels,
    candidate_table = do.call(rbind, candidate_rows),
    coassoc_matrix = coassoc,
    consensus_dissimilarity = consensus_dist,
    consensus_fit = consensus_fit,
    final_clusters = final_clusters,
    final_k = selected$final_k,
    selection_summary = selected$score_table,
    stability_summary = compute_metacluster_stability(candidate_labels),
    data_info = candidates[[1]]$data_info,
    extras = list(consensus = consensus)
  )
}
