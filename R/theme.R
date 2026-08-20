.phynotype_okabe_ito <- c(
  orange = "#E69F00", sky_blue = "#56B4E9", green = "#009E73",
  yellow = "#F0E442", blue = "#0072B2", vermillion = "#D55E00",
  pink = "#CC79A7", black = "#000000", grey = "#999999"
)

.phynotype_palette <- list(
  ink = "#000000",
  panel = "white",
  grid = "grey92",
  accent = unname(.phynotype_okabe_ito["blue"]),
  accent_alt = unname(.phynotype_okabe_ito["vermillion"]),
  positive = unname(.phynotype_okabe_ito["green"]),
  negative = unname(.phynotype_okabe_ito["vermillion"]),
  neutral = unname(.phynotype_okabe_ito["grey"])
)

.phynotype_discrete_palette <- unname(
  .phynotype_okabe_ito[c("blue", "orange", "green", "vermillion", "sky_blue", "pink", "yellow", "grey")]
)

.phynotype_pal <- function(n) {
  if (n <= length(.phynotype_discrete_palette)) {
    .phynotype_discrete_palette[seq_len(n)]
  } else {
    grDevices::colorRampPalette(.phynotype_discrete_palette)(n)
  }
}

#' phynotype ggplot2 theme
#'
#' The single ggplot2 theme used consistently across every `phynotype` plot
#' function (`plot_clusters()`, `plot_cluster_sizes()`,
#' `plot_feature_profiles()`, `plot_consensus()`, `plot_coassoc()`,
#' `plot_biplot()`, `plot()` methods for `feature_importance`,
#' `ceteris_paribus`, and `lime_explanation` objects, and `plot_validation()`).
#' Built on `ggplot2::theme_classic()` with an Okabe-Ito colorblind-safe
#' palette, matching the style used by the `funcml` package's `theme_funcml()`.
#'
#' @param base_size Base font size.
#'
#' @return A ggplot2 theme object.
#'
#' @seealso [scale_color_phynotype()], [scale_fill_phynotype()]
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_clusters(fit) + theme_phynotype()
theme_phynotype <- function(base_size = 11) {
  ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30"),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = .phynotype_palette$grid, linewidth = 0.3),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Discrete color/fill scales matching \code{\link{theme_phynotype}}
#'
#' Colorblind-friendly discrete color and fill scales (Okabe-Ito palette,
#' recycled/interpolated beyond 8 levels) for consistent group colors across
#' \pkg{phynotype} figures.
#'
#' @param ... Additional arguments passed to \code{ggplot2::discrete_scale()}.
#'
#' @return A \pkg{ggplot2} discrete scale object.
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' plot_clusters(fit) + scale_color_phynotype()
#'
#' @name phynotype-scales
#' @export
scale_color_phynotype <- function(...) {
  ggplot2::discrete_scale("colour", palette = .phynotype_pal, ...)
}

#' @rdname phynotype-scales
#' @export
scale_fill_phynotype <- function(...) {
  ggplot2::discrete_scale("fill", palette = .phynotype_pal, ...)
}

.phynotype_direction_scale_fill <- function(...) {
  ggplot2::scale_fill_manual(
    values = c(positive = .phynotype_palette$positive, negative = .phynotype_palette$negative),
    ...
  )
}
