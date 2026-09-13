#' Model-agnostic interpretability for a fitted clustering rule
#'
#' Single entry point for phynotype's interpretability methods. All three
#' work directly from the fitted object's `predict()` method and therefore
#' apply to any clustering engine with native prediction support.
#'
#' \describe{
#'   \item{`"importance"`}{Permutation feature importance: how much the
#'     fitted partition changes when a feature is independently permuted.
#'     Extra arguments: `data`, `features`, `metric`, `loss`, `n_repeats`,
#'     `seed`. See the internal `.interpret_importance()` documentation
#'     source for full argument details (not exported).}
#'   \item{`"lime"`}{LIME-style local surrogate explanations for individual
#'     observations. Extra arguments: `new_data`, `n_features`,
#'     `n_permutations`, `kernel_width`, `target`, `cluster`, `seed`.}
#'   \item{`"ceteris_paribus"`}{Individual conditional profiles showing how a
#'     prediction changes as one feature is varied. Extra arguments:
#'     `new_data`, `features`, `grid`, `grid_size`, `target`, `cluster`.}
#' }
#'
#' All three methods additionally accept `parallel`, `cores`, `workers`, and
#' `progress` for controlling parallel execution via `functionals::fmap()`.
#'
#' @param object A `cluster_fit` object with prediction support.
#' @param method Which interpretability method to run: `"importance"`,
#'   `"lime"`, or `"ceteris_paribus"`. See Details.
#' @param ... Arguments passed on to the selected method. See Details.
#'
#' @return A `feature_importance`, `lime_explanation`, or `ceteris_paribus`
#'   object, depending on `method`. Each has a `plot()` method.
#'
#' @seealso [predict.cluster_fit()] which is used internally by every method.
#'
#' @export
#'
#' @examples
#' fit <- cluster(iris[, 1:4], method = "kmeans", k = 3, seed = 1)
#' imp <- interpret(fit, method = "importance", n_repeats = 3, seed = 1)
#' imp$summary
#'
#' lx <- interpret(fit, method = "lime", new_data = iris[1:2, 1:4],
#'                  n_permutations = 50, seed = 1)
#' lx$explanations
#'
#' cp <- interpret(fit, method = "ceteris_paribus", new_data = iris[1:3, 1:4],
#'                  features = 1:2, grid_size = 10)
#' head(cp$profiles)
interpret <- function(object, method = c("importance", "lime", "ceteris_paribus"), ...) {
  method <- match.arg(method)
  switch(method,
    importance = .interpret_importance(object, ...),
    lime = .interpret_lime(object, ...),
    ceteris_paribus = .interpret_ceteris_paribus(object, ...)
  )
}
