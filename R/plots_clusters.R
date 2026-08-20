#' Plot clustered observations in 2D
#'
#' Project observations onto a two-dimensional embedding and color them by
#' cluster assignment. For `metacluster_fit` objects, delegates to
#' [plot_consensus()].
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for numeric data,
#'   FAMD for mixed numeric/categorical data, MCA for categorical data, and
#'   classical MDS for distance inputs. `"pca"`, `"famd"`, `"mca"`, and
#'   `"mds"` may be selected explicitly.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [explore()] for the underlying embedding, [plot_cluster_sizes()],
#'   [plot_feature_profiles()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_clusters(fit)
plot_clusters <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  UseMethod("plot_clusters")
}

#' @export
plot_clusters.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  embedding <- match.arg(embedding)
  exp <- explore(x, data = data, embedding = embedding)
  plot_clusters(exp)
}

#' @export
plot_clusters.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "famd", "mca", "mds"), ...) {
  embedding <- match.arg(embedding)
  plot_consensus(x, data = data, embedding = embedding, ...)
}

#' @export
plot_clusters.cluster_explore <- function(x, ...) {
  labels <- x$embedding_labels
  if (is.null(labels)) {
    labels <- list(x = "PC1", y = "PC2")
  }
  ggplot2::ggplot(x$plot_data, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cluster"]])) +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(title = "Cluster embedding", x = labels$x, y = labels$y, color = "Cluster") +
    scale_color_phynotype() +
    theme_phynotype()
}

#' Plot a cluster biplot
#'
#' A self-contained reimplementation of `factoextra`'s `fviz_pca_biplot()`
#' and `fviz_mca_biplot()` geometry (individual points/arrows, variable
#' arrows or category points, axis percentage labels, dashed origin lines),
#' producing the same plots without depending on `factoextra`. Only the
#' `"pca"` and `"mca"` embeddings are supported, matching the biplot
#' functions `factoextra` itself provides; there is no FAMD biplot
#' equivalent, and MDS has no variable space to project.
#'
#' Three display variants, matching `factoextra`'s own documented examples
#' (`?factoextra::fviz_pca`):
#' \describe{
#'   \item{`"cluster"`}{Individuals colored and shaped by cluster assignment.
#'     The default.}
#'   \item{`"cos2"`}{Individuals colored by their quality of representation
#'     (`cos2`) on a white-blue-orange gradient.}
#'   \item{`"label"`}{Individuals shown as text labels (observation names or
#'     row numbers) instead of points.}
#' }
#'
#' For a `"pca"` embedding, variables are drawn as arrows from the origin,
#' scaled so their spread matches that of the individuals (`factoextra`'s own
#' scaling rule (`ratio * 0.7`), where `ratio` is the ratio of the individuals' to
#' variables' coordinate ranges). For a `"mca"` embedding, variable
#' categories are drawn as unscaled points (matching
#' `factoextra::fviz_mca_biplot()`'s default `arrows = c(FALSE, FALSE)`).
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param data Optional numeric matrix, data frame, or distance object used to
#'   compute the embedding. Defaults to the training data stored in `x`.
#' @param embedding Embedding method. `"auto"` selects PCA for numeric data
#'   and MCA for categorical data. `"pca"` and `"mca"` may be selected
#'   explicitly.
#' @param variant Display variant: `"cluster"`, `"cos2"`, or `"label"`. See
#'   Details.
#' @param top_n Optional integer; if supplied, only the `top_n` variables (or
#'   categories) contributing the most to the plotted axes are drawn.
#' @param ... Reserved for future extensions.
#'
#' @return A `ggplot` object.
#'
#' @seealso [plot_clusters()], [explore()] for the underlying embedding.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_biplot(fit)
#' plot_biplot(fit, variant = "cos2")
#' plot_biplot(fit, variant = "label")
plot_biplot <- function(x, data = NULL, embedding = c("auto", "pca", "mca"),
                         variant = c("cluster", "cos2", "label"), top_n = NULL, ...) {
  UseMethod("plot_biplot")
}

#' @export
plot_biplot.cluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "mca"),
                                     variant = c("cluster", "cos2", "label"), top_n = NULL, ...) {
  embedding <- match.arg(embedding)
  variant <- match.arg(variant)
  exp <- explore(x, data = data, embedding = embedding)
  plot_biplot(exp, variant = variant, top_n = top_n, ...)
}

#' @export
plot_biplot.metacluster_fit <- function(x, data = NULL, embedding = c("auto", "pca", "mca"),
                                         variant = c("cluster", "cos2", "label"), top_n = NULL, ...) {
  embedding <- match.arg(embedding)
  variant <- match.arg(variant)
  exp <- explore(x, data = data, embedding = embedding)
  plot_biplot(exp, variant = variant, top_n = top_n, ...)
}

#' @export
plot_biplot.cluster_explore <- function(x, data = NULL, embedding = c("auto", "pca", "mca"),
                                         variant = c("cluster", "cos2", "label"), top_n = NULL, ...) {
  if (is.null(x$embedding_fit) || !x$embedding_method %in% c("pca", "mca")) {
    stop(
      "Biplots require a `\"pca\"` or `\"mca\"` embedding; ",
      "the `\"", x$embedding_method, "\"` embedding is not supported.",
      call. = FALSE
    )
  }
  variant <- match.arg(variant)
  biplot_data <- biplot_layers(x$embedding_fit, x$embedding_method, x$plot_data, top_n = top_n)
  ind_df <- biplot_data$ind
  var_df <- biplot_data$var

  p <- ggplot2::ggplot()
  p <- p + switch(
    variant,
    cluster = list(
      ggplot2::geom_point(
        data = ind_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cluster"]], shape = .data[["cluster"]]),
        size = 1.5
      ),
      scale_color_phynotype()
    ),
    cos2 = ggplot2::geom_point(
      data = ind_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], color = .data[["cos2"]]),
      size = 1.5
    ),
    label = ggrepel::geom_text_repel(
      data = ind_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["name"]]),
      color = "black", size = 4
    )
  )
  if (variant == "cos2") {
    p <- p + ggplot2::scale_color_gradientn(colors = c("white", "#2E9FDF", "#FC4E07"), name = "cos2")
  }

  p <- p + biplot_variable_layer(var_df, biplot_data$var_geom, biplot_data$var_color)

  p +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    ggplot2::labs(title = "Cluster biplot", x = biplot_data$xlab, y = biplot_data$ylab) +
    theme_phynotype()
}

biplot_variable_layer <- function(var_df, geom, color) {
  if (identical(geom, "arrow")) {
    list(
      ggplot2::geom_segment(
        data = var_df,
        ggplot2::aes(x = 0, y = 0, xend = .data[["x"]], yend = .data[["y"]]),
        arrow = grid::arrow(length = grid::unit(0.2, "cm")), color = color, linetype = "dashed"
      ),
      ggrepel::geom_text_repel(
        data = var_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["name"]]),
        color = color, size = 4
      )
    )
  } else {
    list(
      ggplot2::geom_point(
        data = var_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]]),
        color = color, shape = 17, size = 1.5
      ),
      ggrepel::geom_text_repel(
        data = var_df, ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["name"]]),
        color = color, size = 4
      )
    )
  }
}

#' Compute biplot individual/variable coordinates for one embedding
#'
#' Reimplements the coordinate, scaling, cos2, and contribution formulas
#' `factoextra::get_pca_var()`/`get_pca_ind()` and `FactoMineR::MCA()`'s own
#' `$var`/`$ind` elements use, so `plot_biplot()` reproduces the same
#' geometry without depending on `factoextra`.
#'
#' @noRd
biplot_layers <- function(fit, method, plot_data, top_n = NULL) {
  ind_names <- rownames(plot_data)
  if (is.null(ind_names)) {
    ind_names <- as.character(seq_len(nrow(plot_data)))
  }

  if (method == "pca") {
    pc <- fit
    eig <- pc$sdev^2
    pct <- 100 * eig / sum(eig)
    var_coord <- sweep(pc$rotation[, 1:2, drop = FALSE], 2, pc$sdev[1:2], `*`)
    var_cos2 <- var_coord^2
    comp_cos2 <- colSums(var_cos2)
    var_contrib <- sweep(var_cos2, 2, comp_cos2, function(a, b) a * 100 / b)
    contrib <- rowSums(sweep(var_contrib, 2, eig[1:2], `*`)) / sum(eig[1:2])
    d2 <- rowSums(pc$x^2)
    ind_cos2 <- (plot_data$x^2 + plot_data$y^2) / d2
    var_geom <- "arrow"
    var_color <- "steelblue"
  } else {
    mca <- fit
    eig <- mca$eig[, 1]
    pct <- mca$eig[, 2]
    var_coord <- mca$var$coord[, 1:2, drop = FALSE]
    contrib <- rowSums(sweep(mca$var$contrib[, 1:2, drop = FALSE], 2, eig[1:2], `*`)) / sum(eig[1:2])
    ind_cos2 <- rowSums(mca$ind$cos2[, 1:2, drop = FALSE])
    var_geom <- "point"
    var_color <- "red"
  }

  var_names <- rownames(var_coord)
  if (is.null(var_names)) {
    var_names <- as.character(seq_len(nrow(var_coord)))
  }
  var_df <- data.frame(x = var_coord[, 1], y = var_coord[, 2], name = var_names, contrib = contrib)
  if (!is.null(top_n) && top_n < nrow(var_df)) {
    var_df <- var_df[order(var_df$contrib, decreasing = TRUE)[seq_len(top_n)], , drop = FALSE]
  }

  if (var_geom == "arrow") {
    r <- min(
      (max(plot_data$x) - min(plot_data$x)) / (max(var_df$x) - min(var_df$x)),
      (max(plot_data$y) - min(plot_data$y)) / (max(var_df$y) - min(var_df$y))
    )
    var_df$x <- var_df$x * r * 0.7
    var_df$y <- var_df$y * r * 0.7
  }

  ind_df <- data.frame(
    x = plot_data$x, y = plot_data$y, cluster = plot_data$cluster,
    cos2 = ind_cos2, name = ind_names
  )

  list(
    ind = ind_df,
    var = var_df,
    var_geom = var_geom,
    var_color = var_color,
    xlab = sprintf("Dim1 (%.1f%%)", pct[1]),
    ylab = sprintf("Dim2 (%.1f%%)", pct[2])
  )
}

#' Plot cluster sizes
#'
#' Display a bar chart of the number of observations per cluster.
#'
#' @param x A `cluster_fit`, `metacluster_fit`, or `cluster_explore` object.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [sizes()], [plot_clusters()], [plot_feature_profiles()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_cluster_sizes(fit)
plot_cluster_sizes <- function(x, ...) {
  UseMethod("plot_cluster_sizes")
}

#' @export
plot_cluster_sizes.cluster_fit <- function(x, ...) {
  exp <- explore(x)
  plot_cluster_sizes(exp)
}

#' @export
plot_cluster_sizes.metacluster_fit <- function(x, ...) {
  exp <- explore(x)
  plot_cluster_sizes(exp)
}

#' @export
plot_cluster_sizes.cluster_explore <- function(x, ...) {
  dat <- x$size_table
  dat$cluster <- factor(dat$cluster)
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[["cluster"]], y = .data[["size"]])) +
    ggplot2::geom_col(fill = .phynotype_palette$accent) +
    ggplot2::labs(title = "Cluster sizes", x = "Cluster", y = "Size") +
    theme_phynotype()
}

#' Plot feature profiles by cluster
#'
#' Display per-cluster mean values for each feature as a grouped bar chart.
#'
#' @param x A `cluster_explore` object (produced by [explore()]).
#' @param features Optional character vector of feature names to include.
#'   Defaults to all features.
#' @param ... Unused.
#'
#' @return A `ggplot` object.
#'
#' @seealso [explore()], [plot_clusters()], [plot_cluster_sizes()]
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' exp <- explore(fit)
#' plot_feature_profiles(exp)
#' plot_feature_profiles(exp, features = c("Sepal.Length", "Petal.Length"))
plot_feature_profiles <- function(x, features = NULL, ...) {
  UseMethod("plot_feature_profiles")
}

#' @export
plot_feature_profiles.cluster_explore <- function(x, features = NULL, ...) {
  if (is.null(x$feature_summary)) {
    stop("Feature profiles are unavailable for this explore object.", call. = FALSE)
  }
  dat <- x$feature_summary
  if (!is.null(features)) {
    dat <- dat[dat$feature %in% features, , drop = FALSE]
  }
  dat$cluster <- factor(dat$cluster)
  ggplot2::ggplot(dat, ggplot2::aes(x = .data[["feature"]], y = .data[["mean"]], fill = .data[["cluster"]])) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(title = "Feature profiles", x = "Feature", y = "Mean", fill = "Cluster") +
    scale_fill_phynotype() +
    theme_phynotype()
}
