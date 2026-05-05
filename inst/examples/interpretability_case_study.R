args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) >= 1L) args[[1]] else "interpretability-results"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package `ggplot2` is required to save the example plots.", call. = FALSE)
}

if (requireNamespace("phynotype", quietly = TRUE)) {
  suppressPackageStartupMessages(library(phynotype))
} else if (file.exists("DESCRIPTION") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE)
} else {
  stop(
    "Install `phynotype`, or run this script from the package source directory with `pkgload` installed.",
    call. = FALSE
  )
}

x <- iris[, 1:4]
fit <- cluster(x, method = "kmeans", k = 3, scale = TRUE, seed = 11)
cluster_labels <- clusters(fit)
representatives <- tapply(seq_len(nrow(x)), cluster_labels, function(idx) idx[1])
representative_data <- x[unlist(representatives), , drop = FALSE]

validation <- validate(fit, truth = iris$Species, n_boot = 3)
validation_table <- validation$metrics_table
cluster_species_table <- as.data.frame.matrix(table(cluster = cluster_labels, species = iris$Species))

importance <- feature_importance(fit, n_repeats = 10, seed = 11)

profiles <- ceteris_paribus(
  fit,
  new_data = representative_data,
  features = c("Petal.Length", "Petal.Width"),
  grid_size = 25,
  target = "score"
)

lime <- lime_explain(
  fit,
  new_data = representative_data,
  n_features = 3,
  n_permutations = 200,
  seed = 12
)

utils::write.csv(
  data.frame(observation = seq_len(nrow(x)), cluster = cluster_labels, species = iris$Species),
  file.path(output_dir, "cluster_assignments.csv"),
  row.names = FALSE
)
utils::write.csv(cluster_species_table, file.path(output_dir, "cluster_species_table.csv"))
utils::write.csv(validation_table, file.path(output_dir, "validation_metrics.csv"), row.names = FALSE)
utils::write.csv(importance$summary, file.path(output_dir, "feature_importance.csv"), row.names = FALSE)
utils::write.csv(profiles$profiles, file.path(output_dir, "ceteris_paribus_profiles.csv"), row.names = FALSE)
utils::write.csv(lime$explanations, file.path(output_dir, "lime_explanations.csv"), row.names = FALSE)
utils::write.csv(lime$neighborhoods, file.path(output_dir, "lime_neighborhoods.csv"), row.names = FALSE)

ggplot2::ggsave(
  file.path(output_dir, "feature_importance.png"),
  plot(importance),
  width = 7,
  height = 4,
  dpi = 150
)
ggplot2::ggsave(
  file.path(output_dir, "ceteris_paribus_profiles.png"),
  plot(profiles),
  width = 8,
  height = 5,
  dpi = 150
)
ggplot2::ggsave(
  file.path(output_dir, "lime_explanations.png"),
  plot(lime),
  width = 8,
  height = 5,
  dpi = 150
)

writeLines(
  c(
    "# Interpretability Case Study",
    "",
    "Run from an installed phynotype package or from a loaded source checkout:",
    "",
    "```sh",
    "Rscript inst/examples/interpretability_case_study.R interpretability-results",
    "```",
    "",
    "Generated files:",
    "- cluster_assignments.csv",
    "- cluster_species_table.csv",
    "- validation_metrics.csv",
    "- feature_importance.csv",
    "- ceteris_paribus_profiles.csv",
    "- lime_explanations.csv",
    "- lime_neighborhoods.csv",
    "- feature_importance.png",
    "- ceteris_paribus_profiles.png",
    "- lime_explanations.png"
  ),
  file.path(output_dir, "README.md")
)

message("Interpretability results written to: ", normalizePath(output_dir, mustWork = FALSE))
