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

#' Prepare mixed-type data for clustering
#'
#' Single entry point for turning a mixed numeric/categorical data frame into
#' either a numeric design matrix (`method = "encode"`) or a Gower dissimilarity
#' matrix (`method = "distance"`).
#'
#' \describe{
#'   \item{`"encode"`}{(default) One-hot encodes categorical variables
#'     (full-rank by default), optionally centers/scales numeric variables,
#'     and returns a numeric design matrix suitable for methods such as
#'     `"kmeans"`, `"pam"`, `"dbscan"`, and `"gmm"`. Extra arguments: `center`,
#'     `scale`, `drop_first`, `na_action`.}
#'   \item{`"distance"`}{Builds a Gower dissimilarity matrix
#'     (Gower, 1971) via `cluster::daisy()`, the recommended first step
#'     before hierarchical clustering (`"hclust"` or `"agnes"`) on mixed
#'     data. Extra arguments: `metric` (default `"gower"`), and `...` passed
#'     to `cluster::daisy()`.}
#' }
#'
#' Encoding is intentionally a manual first step: clustering algorithms that
#' use Euclidean distances or Gaussian models should not silently coerce
#' mixed data.
#'
#' @param x Data frame, matrix, or vector-like object coercible to a data
#'   frame.
#' @param method Which preparation to perform: `"encode"` or `"distance"`.
#'   See Details.
#' @param ... Arguments passed on to the selected method. See Details.
#'
#' @return For `method = "encode"`, a numeric matrix with preprocessing
#'   metadata stored in the attribute `"phynotype_preprocess"`. For
#'   `method = "distance"`, a `dist` object.
#'
#' @seealso [cluster()] for fitting with the resulting matrix or distance
#'   object.
#'
#' @references
#' Gower, J.C. (1971). A general coefficient of similarity and some of its
#' properties. *Biometrics*, **27**(4), 857--874.
#'
#' @export
#'
#' @examples
#' # Mixed data frame
#' df <- data.frame(
#'   x1 = c(1.2, 0.5, 3.1),
#'   x2 = factor(c("A", "B", "A"))
#' )
#' mat <- prepare(df)
#' mat
#'
#' d <- prepare(iris, method = "distance")
#' fit <- cluster(d, method = "hclust", k = 3)
#' fit
prepare <- function(x, method = c("encode", "distance"), ...) {
  method <- match.arg(method)
  switch(method,
    encode = .prepare_encode(x, ...),
    distance = .prepare_distance(x, ...)
  )
}

.prepare_encode <- function(x,
                             center = TRUE,
                             scale = TRUE,
                             drop_first = FALSE,
                             na_action = c("keep", "fail")) {
  na_action <- match.arg(na_action)
  if (inherits(x, "dist")) {
    stop("`prepare()` with `method = \"encode\"` expects row-by-feature data, not a distance object.", call. = FALSE)
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

.prepare_distance <- function(x, metric = "gower", ...) {
  if (!requireNamespace("cluster", quietly = TRUE)) {
    stop("Package `cluster` is required for `prepare()` with `method = \"distance\"`.", call. = FALSE)
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
