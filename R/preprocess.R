restore_preprocessed_matrix <- function(new_data, data_info) {
  if (is.data.frame(new_data)) {
    new_data <- as.matrix(new_data)
  }
  if (!is.matrix(new_data) || !is.numeric(new_data)) {
    stop("`new_data` must be a numeric matrix or numeric data frame.", call. = FALSE)
  }
  prep <- data_info$preprocessing
  if (isTRUE(prep$center)) {
    new_data <- sweep(new_data, 2, prep$scaled_center, FUN = "-")
  }
  if (isTRUE(prep$scale)) {
    new_data <- sweep(new_data, 2, prep$scaled_scale, FUN = "/")
  }
  new_data
}

#' Prepare mixed-type data for numeric clustering methods
#'
#' Convert a data frame containing numeric, logical, character, or factor
#' columns into a numeric design matrix suitable for methods such as
#' `"kmeans"`, `"pam"`, `"dbscan"`, and `"gmm"`.
#'
#' Categorical variables are one-hot encoded (full-rank by default), numeric
#' variables are optionally centered and scaled, and missingness can be
#' preserved as an explicit level or trigger an error.
#'
#' This is intentionally a manual first step. Clustering algorithms that use
#' Euclidean distances or Gaussian models should not silently coerce mixed data.
#' For hierarchical methods on mixed data, use [mixed_distance()] instead.
#'
#' @param x Data frame, matrix, or vector-like object coercible to a data frame.
#' @param center Logical; if `TRUE`, center numeric columns to zero mean after
#'   encoding.
#' @param scale Logical; if `TRUE`, scale numeric columns to unit variance after
#'   encoding.
#' @param drop_first Logical; if `TRUE`, drop the first dummy column for each
#'   categorical feature to avoid the dummy-variable trap. The default (`FALSE`)
#'   keeps the full one-hot representation, which is usually preferable for
#'   distance-based clustering.
#' @param na_action Missing-value strategy. `"keep"` (default) adds an explicit
#'   `"missing"` level for categoricals and median-imputes numeric columns.
#'   `"fail"` stops with an error on any missing value.
#'
#' @return A numeric matrix with preprocessing metadata stored in the attribute
#'   `"phynotype_preprocess"`.
#'
#' @seealso [mixed_distance()] for Gower-distance computation before
#'   hierarchical clustering.
#'
#' @export
#'
#' @examples
#' # Mixed data frame
#' df <- data.frame(
#'   x1 = c(1.2, 0.5, 3.1),
#'   x2 = factor(c("A", "B", "A"))
#' )
#' mat <- prepare_mixed_data(df)
#' mat
prepare_mixed_data <- function(x,
                               center = TRUE,
                               scale = TRUE,
                               drop_first = FALSE,
                               na_action = c("keep", "fail")) {
  na_action <- match.arg(na_action)
  if (inherits(x, "dist")) {
    stop("`prepare_mixed_data()` expects row-by-feature data, not a distance object.", call. = FALSE)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  if (nrow(x) < 1L || ncol(x) < 1L) {
    stop("`x` must contain at least one row and one column.", call. = FALSE)
  }
  if (na_action == "fail" && anyNA(x)) {
    stop("`x` contains missing values. Use `na_action = \"keep\"` or impute before encoding.", call. = FALSE)
  }

  feature_types <- vapply(x, infer_feature_type, character(1))
  encoded_parts <- list()
  encoded_map <- data.frame(
    original_feature = character(),
    encoded_feature = character(),
    type = character(),
    stringsAsFactors = FALSE
  )

  for (nm in names(x)) {
    col <- x[[nm]]
    type <- feature_types[[nm]]
    if (type == "numeric") {
      col <- as.numeric(col)
      if (anyNA(col)) {
        med <- stats::median(col, na.rm = TRUE)
        if (!is.finite(med)) {
          med <- 0
        }
        col[is.na(col)] <- med
      }
      mat <- matrix(col, ncol = 1)
      colnames(mat) <- nm
    } else {
      fac <- as.factor(col)
      if (anyNA(fac)) {
        fac <- add_missing_level(fac)
      }
      mat <- stats::model.matrix(~ fac - 1)
      levels_clean <- make.names(levels(fac), unique = TRUE)
      colnames(mat) <- paste0(nm, "_", levels_clean)
      if (drop_first && ncol(mat) > 1L) {
        mat <- mat[, -1L, drop = FALSE]
      }
    }
    encoded_parts[[nm]] <- mat
    encoded_map <- rbind(
      encoded_map,
      data.frame(
        original_feature = nm,
        encoded_feature = colnames(mat),
        type = type,
        stringsAsFactors = FALSE
      )
    )
  }

  out <- do.call(cbind, encoded_parts)
  storage.mode(out) <- "double"
  if (center || scale) {
    out <- scale(out, center = center, scale = scale)
    bad_scale <- !is.finite(colSums(out))
    if (any(bad_scale)) {
      out[, bad_scale] <- 0
    }
  }
  attr(out, "phynotype_preprocess") <- list(
    input = "mixed",
    feature_types = feature_types,
    encoded_map = encoded_map,
    center = center,
    scale = scale,
    drop_first = drop_first,
    na_action = na_action
  )
  out
}

#' Compute a mixed-type distance matrix
#'
#' Build a Gower dissimilarity matrix for mixed-type data using
#' `cluster::daisy()`. This is the recommended first step before hierarchical
#' clustering (`"hclust"` or `"agnes"`) when the input contains categorical
#' variables.
#'
#' For two observations \eqn{x_i} and \eqn{x_j}, the Gower coefficient
#' (Gower, 1971) is
#'
#' \deqn{
#'   d_G(i, j) = \frac{\sum_{f=1}^{p} w_{ijf}\, \delta_{ijf}\, s_{ijf}}
#'                    {\sum_{f=1}^{p} w_{ijf}\, \delta_{ijf}},
#' }
#'
#' where \eqn{\delta_{ijf}} indicates whether feature \eqn{f} is comparable
#' (not missing), \eqn{w_{ijf}} is an optional weight, and \eqn{s_{ijf}} is the
#' partial similarity: the scaled absolute difference for numeric features and 0
#' (match) or 1 (mismatch) for categorical features.
#'
#' @param x Data frame containing numeric and/or categorical columns.
#' @param metric Distance metric passed to `cluster::daisy()`. Defaults to
#'   `"gower"`.
#' @param ... Additional arguments passed to `cluster::daisy()`.
#'
#' @return A `dist` object.
#'
#' @seealso [prepare_mixed_data()] for converting mixed data to a numeric
#'   matrix, [cluster()] for fitting with the resulting distance object.
#'
#' @references
#' Gower, J.C. (1971). A general coefficient of similarity and some of its
#' properties. *Biometrics*, **27**(4), 857--874.
#'
#' @export
#'
#' @examples
#' d <- mixed_distance(iris)
#' fit <- cluster(d, method = "hclust", k = 3)
#' fit
mixed_distance <- function(x, metric = "gower", ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for `mixed_distance()`.", call. = FALSE)
  }
  if (inherits(x, "dist")) {
    return(x)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  for (nm in names(x)) {
    if (is.character(x[[nm]]) || is.logical(x[[nm]])) {
      x[[nm]] <- as.factor(x[[nm]])
    }
  }
  cluster::daisy(x, metric = metric, ...)
}

infer_feature_type <- function(x) {
  if (is.numeric(x) || is.integer(x)) {
    "numeric"
  } else if (is.ordered(x)) {
    "ordinal"
  } else {
    "categorical"
  }
}

add_missing_level <- function(x) {
  x <- as.factor(x)
  levels(x) <- c(levels(x), "missing")
  x[is.na(x)] <- "missing"
  x
}
