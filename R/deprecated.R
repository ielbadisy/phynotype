#' Deprecated functions in phynotype
#'
#' These functions were renamed to glued lowercase names and will be removed
#' in a future release. Use the new names instead.
#'
#' @param ... Arguments passed to the new function.
#' @return The result of the new function.
#' @name phynotype-deprecated
#' @keywords internal
NULL

#' @rdname phynotype-deprecated
#' @export
adjusted_rand_index <- function(...) {
  .Deprecated("adjustedrandindex")
  adjustedrandindex(...)
}

#' @rdname phynotype-deprecated
#' @export
ceteris_paribus <- function(...) {
  .Deprecated("ceterisparibus")
  ceterisparibus(...)
}

#' @rdname phynotype-deprecated
#' @export
feature_importance <- function(...) {
  .Deprecated("featureimportance")
  featureimportance(...)
}

#' @rdname phynotype-deprecated
#' @export
lime_explain <- function(...) {
  .Deprecated("limeexplain")
  limeexplain(...)
}

#' @rdname phynotype-deprecated
#' @export
method_used <- function(...) {
  .Deprecated("methodused")
  methodused(...)
}

#' @rdname phynotype-deprecated
#' @export
mixed_distance <- function(...) {
  .Deprecated("mixeddistance")
  mixeddistance(...)
}

#' @rdname phynotype-deprecated
#' @export
n_clusters <- function(...) {
  .Deprecated("nclusters")
  nclusters(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_biplot <- function(...) {
  .Deprecated("plotbiplot")
  plotbiplot(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_cluster_sizes <- function(...) {
  .Deprecated("plotclustersizes")
  plotclustersizes(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_clusters <- function(...) {
  .Deprecated("plotclusters")
  plotclusters(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_coassoc <- function(...) {
  .Deprecated("plotcoassoc")
  plotcoassoc(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_consensus <- function(...) {
  .Deprecated("plotconsensus")
  plotconsensus(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_dendrogram <- function(...) {
  .Deprecated("plotdendrogram")
  plotdendrogram(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_feature_profiles <- function(...) {
  .Deprecated("plotfeatureprofiles")
  plotfeatureprofiles(...)
}

#' @rdname phynotype-deprecated
#' @export
plot_silhouette <- function(...) {
  .Deprecated("plotsilhouette")
  plotsilhouette(...)
}

#' @rdname phynotype-deprecated
#' @export
prepare_mixed_data <- function(...) {
  .Deprecated("preparemixeddata")
  preparemixeddata(...)
}

#' @rdname phynotype-deprecated
#' @export
scale_color_phynotype <- function(...) {
  .Deprecated("scalecolorphynotype")
  scalecolorphynotype(...)
}

#' @rdname phynotype-deprecated
#' @export
scale_fill_phynotype <- function(...) {
  .Deprecated("scalefillphynotype")
  scalefillphynotype(...)
}

#' @rdname phynotype-deprecated
#' @export
theme_phynotype <- function(...) {
  .Deprecated("themephynotype")
  themephynotype(...)
}
