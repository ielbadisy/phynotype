#' phynotype ggplot2 theme
#'
#' The single ggplot2 theme used consistently across every `phynotype` plot
#' function (`plot_clusters()`, `plot_cluster_sizes()`,
#' `plot_feature_profiles()`, `plot_consensus()`, `plot_coassoc()`,
#' `plot_biplot()`, `plot()` methods for `feature_importance`,
#' `ceteris_paribus`, and `lime_explanation` objects, and `plot_validation()`).
#' A thin wrapper around [ggplot2::theme_minimal()].
#'
#' @param base_size Base font size.
#'
#' @return A ggplot2 theme object.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_clusters(fit) + theme_phynotype()
theme_phynotype <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "grey88", linewidth = 0.3),
      strip.text = ggplot2::element_text(face = "bold")
    )
}
