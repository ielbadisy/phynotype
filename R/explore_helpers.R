build_feature_summary <- function(data, clusters) {
  cluster_levels <- sort(unique(clusters))
  parts <- lapply(cluster_levels, function(cl) {
    subset <- data[clusters == cl, , drop = FALSE]
    data.frame(
      cluster = cl,
      feature = colnames(data),
      mean = vapply(seq_len(ncol(subset)), function(i) mean(subset[, i]), numeric(1)),
      sd = vapply(seq_len(ncol(subset)), function(i) stats::sd(subset[, i]), numeric(1)),
      median = vapply(seq_len(ncol(subset)), function(i) stats::median(subset[, i]), numeric(1)),
      min = vapply(seq_len(ncol(subset)), function(i) min(subset[, i]), numeric(1)),
      max = vapply(seq_len(ncol(subset)), function(i) max(subset[, i]), numeric(1))
    )
  })
  do.call(rbind, parts)
}

compute_separation_table <- function(data, clusters) {
  total_var <- apply(data, 2, stats::var)
  means <- rowsum(data, group = clusters) / as.vector(table(clusters))
  grand_mean <- colMeans(data)
  between <- vapply(seq_len(ncol(data)), function(i) {
    sum(as.vector(table(clusters)) * (means[, i] - grand_mean[i])^2)
  }, numeric(1))
  data.frame(
    feature = colnames(data),
    separation = between / ((nrow(data) - 1) * total_var)
  )
}
