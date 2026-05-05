check_interpretable_fit <- function(object, require_predict = TRUE) {
  if (!inherits(object, "cluster_fit")) {
    stop("`object` must be a `cluster_fit` object.", call. = FALSE)
  }
  if (require_predict) {
    registry <- get_cluster_registry()
    entry <- registry[[object$method]]
    if (is.null(entry) || !isTRUE(entry$supports_predict)) {
      stop("Interpretability methods require a clustering method with prediction support.", call. = FALSE)
    }
  }
  invisible(object)
}

prepare_interpretability_data <- function(object, data = NULL, arg = "data") {
  check_interpretable_fit(object)
  if (is.null(data)) {
    data <- object$data_info$original_data
  }
  if (inherits(data, "dist") || identical(object$data_info$input_type, "dist")) {
    stop(
      "Interpretability methods require a fit trained on row-by-feature data, not only a distance object.",
      call. = FALSE
    )
  }
  if (identical(object$data_info$input_type, "mixed")) {
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    if (ncol(data) != object$data_info$n_features) {
      stop("`", arg, "` must have the same number of columns as the fitted training data.", call. = FALSE)
    }
    if (is.null(colnames(data))) {
      colnames(data) <- feature_names_or_default(object, ncol(data))
    }
    return(data)
  }
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  }
  if (!is.matrix(data) || !is.numeric(data)) {
    stop("`", arg, "` must be a numeric matrix or numeric data frame.", call. = FALSE)
  }
  if (ncol(data) != object$data_info$n_features) {
    stop("`", arg, "` must have the same number of columns as the fitted training data.", call. = FALSE)
  }
  if (is.null(colnames(data))) {
    colnames(data) <- feature_names_or_default(object, ncol(data))
  }
  data
}

feature_names_or_default <- function(object, p = object$data_info$n_features) {
  names <- object$data_info$feature_names
  if (is.null(names) || anyNA(names) || any(names == "")) {
    names <- paste0("feature_", seq_len(p))
  }
  names
}

match_interpretability_features <- function(features, data) {
  if (is.null(features)) {
    return(colnames(data))
  }
  if (is.numeric(features)) {
    if (any(features < 1L | features > ncol(data) | is.na(features))) {
      stop("Numeric `features` must index columns in `data`.", call. = FALSE)
    }
    return(colnames(data)[as.integer(features)])
  }
  if (!is.character(features) || length(features) < 1L) {
    stop("`features` must be `NULL`, a character vector, or a numeric column index.", call. = FALSE)
  }
  missing <- setdiff(features, colnames(data))
  if (length(missing)) {
    stop("Unknown feature(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  unique(features)
}

phynotype_map <- function(.x, .f, parallel = FALSE, cores = NULL, workers = NULL, progress = FALSE, ...) {
  if (isTRUE(parallel)) {
    if (!requireNamespace("functionals", quietly = TRUE)) {
      warning("Package `functionals` is not installed; falling back to sequential execution.", call. = FALSE)
      return(lapply(.x, .f, ...))
    }
    ncores <- if (!is.null(cores)) {
      as.integer(cores)
    } else if (!is.null(workers)) {
      as.integer(workers)
    } else {
      max(1L, parallel::detectCores(logical = FALSE) - 1L)
    }
    if (length(ncores) != 1L || !is.finite(ncores) || ncores < 1L) {
      stop("`cores` must be a positive integer.", call. = FALSE)
    }
    return(functionals::fmap(.x, .f, ncores = ncores, pb = isTRUE(progress), ...))
  }
  lapply(.x, .f, ...)
}

prediction_score_matrix <- function(prediction) {
  if (!is.null(prediction$membership)) {
    scores <- as.matrix(prediction$membership)
  } else if (!is.null(prediction$distances)) {
    distances <- as.matrix(prediction$distances)
    sigma <- stats::median(distances[is.finite(distances)])
    if (!is.finite(sigma) || sigma <= 0) {
      sigma <- 1
    }
    scores <- exp(-(distances^2) / (2 * sigma^2))
    scores <- scores / pmax(rowSums(scores), .Machine$double.eps)
  } else {
    levels <- sort(unique(prediction$clusters))
    scores <- outer(prediction$clusters, levels, FUN = "==") * 1
    colnames(scores) <- as.character(levels)
  }
  if (is.null(colnames(scores))) {
    colnames(scores) <- as.character(seq_len(ncol(scores)))
  }
  scores
}

predict_interpretability <- function(object, data) {
  pred <- stats::predict(object, data)
  list(prediction = pred, scores = prediction_score_matrix(pred))
}

cluster_score_column <- function(scores, cluster, fallback = 1L) {
  if (is.null(cluster)) {
    cluster <- fallback
  }
  cluster <- as.character(cluster)
  if (cluster %in% colnames(scores)) {
    return(cluster)
  }
  alt <- paste0("cluster_", cluster)
  if (alt %in% colnames(scores)) {
    return(alt)
  }
  stop("Cluster `", cluster, "` is not available in the prediction scores.", call. = FALSE)
}

make_feature_grid <- function(training_data, feature, grid = NULL, grid_size = 25L) {
  if (!is.null(grid)) {
    if (is.list(grid)) {
      values <- grid[[feature]]
      if (is.null(values)) {
        values <- grid[[which(colnames(training_data) == feature)]]
      }
    } else {
      values <- grid
    }
    if (is.null(values) || length(values) < 1L) {
      stop("`grid` values must not be empty.", call. = FALSE)
    }
    if (is.numeric(values)) {
      return(sort(unique(as.numeric(values))))
    }
    return(sort(unique(as.character(values))))
  }
  values <- training_data[, feature]
  if (is.numeric(values)) {
    values <- values[is.finite(values)]
    if (!length(values)) {
      stop("Feature `", feature, "` has no finite values for grid construction.", call. = FALSE)
    }
    if (length(unique(values)) <= grid_size) {
      return(sort(unique(values)))
    }
    return(as.numeric(stats::quantile(values, probs = seq(0, 1, length.out = grid_size), names = FALSE, type = 8)))
  }
  values <- as.character(values)
  values <- values[!is.na(values)]
  if (!length(values)) {
    stop("Feature `", feature, "` has no observed values for grid construction.", call. = FALSE)
  }
  sort(unique(values))
}

interpretability_metric <- function(metric, data, clusters) {
  metric <- match.arg(metric, c("instability", "silhouette", "total_within"))
  data_num <- interpretability_numeric_basis(data)
  switch(
    metric,
    instability = NA_real_,
    silhouette = {
      if (!requireNamespace("cluster", quietly = TRUE)) {
        stop("Package `cluster` is required for `metric = \"silhouette\"`.", call. = FALSE)
      }
      compute_silhouette_metric(data_num, clusters)
    },
    total_within = compute_total_within(data_num, clusters)
  )
}

interpretability_numeric_basis <- function(data) {
  if (is.data.frame(data)) {
    numeric_cols <- vapply(data, is.numeric, logical(1))
    if (all(numeric_cols)) {
      return(as.matrix(data))
    }
    return(prepare_mixed_data(data, center = TRUE, scale = TRUE))
  }
  if (is.matrix(data) && is.numeric(data)) {
    return(data)
  }
  prepare_mixed_data(data, center = TRUE, scale = TRUE)
}

summarize_importance_results <- function(results) {
  rows <- split(results, results$feature)
  out <- lapply(rows, function(x) {
    data.frame(
      feature = x$feature[[1]],
      importance = mean(x$importance, na.rm = TRUE),
      std_error = stats::sd(x$importance, na.rm = TRUE) / sqrt(sum(!is.na(x$importance))),
      n_repeats = sum(!is.na(x$importance))
    )
  })
  summary <- do.call(rbind, out)
  summary <- summary[order(summary$importance, decreasing = TRUE), , drop = FALSE]
  rownames(summary) <- NULL
  summary
}

safe_column_sds <- function(data) {
  data <- interpretability_numeric_basis(data)
  sds <- apply(data, 2, stats::sd)
  sds[!is.finite(sds) | sds <= 0] <- 1
  sds
}
