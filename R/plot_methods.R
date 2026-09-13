#' Plot a fitted cluster solution
#'
#' Draw one of the diagnostic plots available for a `cluster_fit` object, or
#' get all of them at once.
#'
#' @param x A `cluster_fit` object.
#' @param which Which plot to draw: `"clusters"` (2D embedding, the default),
#'   `"biplot"`, `"sizes"`, `"feature_profiles"`, `"dendrogram"` (hierarchical
#'   fits only), `"silhouette"`, or `"all"` to return every applicable plot
#'   (excluding `"dendrogram"`, which draws with base graphics instead of
#'   returning a `ggplot` object) as a named list.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method used by `"clusters"`, `"biplot"`, and
#'   `"all"`. `"auto"` selects PCA for numeric data, FAMD for mixed
#'   numeric/categorical data, MCA for categorical data, and classical MDS for
#'   distance inputs.
#' @param variant Biplot display variant: `"cluster"`, `"cos2"`, or `"label"`.
#' @param top_n Optional integer; if supplied, only the `top_n` biplot
#'   variables (or categories) contributing the most are drawn.
#' @param features Optional character vector of feature names used by
#'   `"feature_profiles"`.
#' @param ... Unused.
#'
#' @return A `ggplot` object, or for `which = "dendrogram"` the underlying
#'   `hclust` object (invisibly), or for `which = "all"` a named list of
#'   `ggplot` objects.
#'
#' @seealso [explore()] for the underlying embedding, [feature_importance()],
#'   [lime_explain()], and [ceteris_paribus()] for model-agnostic
#'   explanations.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot(fit)
#' plot(fit, which = "sizes")
plot.cluster_fit <- function(x,
                              which = c("clusters", "biplot", "sizes", "feature_profiles",
                                        "dendrogram", "silhouette", "all"),
                              data = NULL,
                              embedding = c("auto", "pca", "famd", "mca", "mds"),
                              variant = c("cluster", "cos2", "label"),
                              top_n = NULL,
                              features = NULL,
                              ...) {
  which <- match.arg(which)
  variant <- match.arg(variant)

  if (which == "dendrogram") {
    if (!inherits(x$fitted_object, "hclust")) {
      stop("`which = \"dendrogram\"` is only available for hierarchical fits.", call. = FALSE)
    }
    graphics::plot(x$fitted_object, main = "Cluster dendrogram", xlab = "", sub = "")
    return(invisible(x$fitted_object))
  }
  if (which == "silhouette") {
    return(.plot_silhouette_fit(x))
  }

  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)

  if (which == "all") {
    plots <- list(
      clusters = .plot_clusters_explore(exp),
      biplot = tryCatch(.plot_biplot_explore(exp, variant = variant, top_n = top_n), error = function(e) NULL),
      sizes = .plot_sizes_explore(exp),
      feature_profiles = tryCatch(.plot_feature_profiles_explore(exp, features = features), error = function(e) NULL),
      silhouette = tryCatch(.plot_silhouette_fit(x), error = function(e) NULL)
    )
    return(Filter(Negate(is.null), plots))
  }

  plot(exp, which = which, variant = variant, top_n = top_n, features = features)
}

#' Plot a metaclustering consensus solution
#'
#' Draw one of the diagnostic plots available for a `metacluster_fit` object,
#' or get all of them at once.
#'
#' @param x A `metacluster_fit` object.
#' @param which Which plot to draw: `"clusters"`/`"consensus"` (2D consensus
#'   embedding, the default), `"biplot"`, `"sizes"`, `"dendrogram"` (consensus
#'   hierarchical tree), `"coassoc"` (co-association heatmap), or `"all"` to
#'   return every applicable plot (excluding `"dendrogram"`) as a named list.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method used by `"clusters"`, `"biplot"`, and
#'   `"all"`. `"auto"` selects PCA for numeric data, FAMD for mixed
#'   numeric/categorical data, MCA for categorical data, and classical MDS for
#'   distance inputs.
#' @param variant Biplot display variant: `"cluster"`, `"cos2"`, or `"label"`.
#' @param top_n Optional integer; if supplied, only the `top_n` biplot
#'   variables (or categories) contributing the most are drawn.
#' @param ... Unused.
#'
#' @return A `ggplot` object, or for `which = "dendrogram"` the underlying
#'   `hclust` object (invisibly), or for `which = "all"` a named list of
#'   `ggplot` objects.
#'
#' @seealso [metacluster()], [explore()]
#'
#' @export
#'
#' @examples
#' mfit <- metacluster(iris[, 1:4], methods = c("kmeans", "hclust"),
#'                     k = 2:3, seed = 1)
#' plot(mfit)
#' plot(mfit, which = "coassoc")
plot.metacluster_fit <- function(x,
                                  which = c("clusters", "consensus", "biplot", "sizes",
                                            "dendrogram", "coassoc", "all"),
                                  data = NULL,
                                  embedding = c("auto", "pca", "famd", "mca", "mds"),
                                  variant = c("cluster", "cos2", "label"),
                                  top_n = NULL,
                                  ...) {
  which <- match.arg(which)
  variant <- match.arg(variant)

  if (which == "dendrogram") {
    graphics::plot(x$consensus_fit, main = "Consensus dendrogram", xlab = "", sub = "")
    return(invisible(x$consensus_fit))
  }
  if (which == "coassoc") {
    return(.plot_coassoc_meta(x))
  }

  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)

  if (which == "all") {
    plots <- list(
      consensus = .plot_consensus_explore(exp),
      biplot = tryCatch(.plot_biplot_explore(exp, variant = variant, top_n = top_n), error = function(e) NULL),
      sizes = .plot_sizes_explore(exp),
      coassoc = .plot_coassoc_meta(x)
    )
    return(Filter(Negate(is.null), plots))
  }
  if (which %in% c("clusters", "consensus")) {
    return(.plot_consensus_explore(exp))
  }
  if (which == "biplot") {
    return(.plot_biplot_explore(exp, variant = variant, top_n = top_n))
  }
  .plot_sizes_explore(exp)
}

#' Plot a cluster exploration summary
#'
#' Draw one of the diagnostic plots available for a `cluster_explore` object
#' (produced by [explore()]), or get all of them at once.
#'
#' @param x A `cluster_explore` object.
#' @param which Which plot to draw: `"clusters"` (2D embedding, the default),
#'   `"biplot"`, `"sizes"`, `"feature_profiles"`, or `"all"` to return every
#'   applicable plot as a named list.
#' @param variant Biplot display variant: `"cluster"`, `"cos2"`, or `"label"`.
#' @param top_n Optional integer; if supplied, only the `top_n` biplot
#'   variables (or categories) contributing the most are drawn.
#' @param features Optional character vector of feature names used by
#'   `"feature_profiles"`.
#' @param ... Unused.
#'
#' @return A `ggplot` object, or for `which = "all"` a named list of `ggplot`
#'   objects.
#'
#' @seealso [explore()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' exp <- explore(fit)
#' plot(exp)
#' plot(exp, which = "feature_profiles")
plot.cluster_explore <- function(x,
                                  which = c("clusters", "biplot", "sizes", "feature_profiles", "all"),
                                  variant = c("cluster", "cos2", "label"),
                                  top_n = NULL,
                                  features = NULL,
                                  ...) {
  which <- match.arg(which)
  variant <- match.arg(variant)

  if (which == "all") {
    plots <- list(
      clusters = .plot_clusters_explore(x),
      biplot = tryCatch(.plot_biplot_explore(x, variant = variant, top_n = top_n), error = function(e) NULL),
      sizes = .plot_sizes_explore(x),
      feature_profiles = tryCatch(.plot_feature_profiles_explore(x, features = features), error = function(e) NULL)
    )
    return(Filter(Negate(is.null), plots))
  }

  switch(which,
    clusters = .plot_clusters_explore(x),
    biplot = .plot_biplot_explore(x, variant = variant, top_n = top_n),
    sizes = .plot_sizes_explore(x),
    feature_profiles = .plot_feature_profiles_explore(x, features = features)
  )
}
