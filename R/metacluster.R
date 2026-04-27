#' Fit a consensus meta-clustering solution
#'
#' Fit several candidate clustering solutions and summarize their agreement
#' through a co-association consensus matrix. The consensus partition is
#' obtained by hierarchical clustering of the dissimilarity matrix
#' \eqn{D = 1 - C}, where \eqn{C_{ij}} is the proportion of candidate
#' partitions that place observations \eqn{i} and \eqn{j} in the same cluster.
#'
#' @param x Numeric matrix or numeric data frame.
#' @param methods Character vector of candidate clustering methods.
#' @param k Integer vector of candidate cluster counts.
#' @param consensus Consensus strategy. Currently only `"coassoc"` is
#'   implemented.
#' @param scale Logical; if `TRUE`, scale columns before fitting candidates.
#' @param center Logical; if `TRUE`, center columns before fitting candidates.
#' @param seed Optional integer random seed.
#' @param ... Additional arguments passed to `cluster()`.
#'
#' @return A `metacluster_fit` object.
#' @export
#'
#' @examples
#' mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"), k = 2:3, seed = 1)
#' mfit
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

  prepared <- prepare_cluster_input(x, method = "kmeans", scale = scale, center = center, k = k_values[[1]])
  data <- prepared$data_info$original_data
  if (inherits(data, "dist")) {
    stop("`metacluster()` requires a numeric matrix or numeric data frame.", call. = FALSE)
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
