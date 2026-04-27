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
